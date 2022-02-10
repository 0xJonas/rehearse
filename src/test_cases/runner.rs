use super::{TestCase, TestOptions};
use crate::test_cases::reporter::{ProgressReporter, DefaultProgressReporter, State};
use crate::cli::{Terminal};

use log::{trace, error};
use tokio::process::Command;
use tokio::io::AsyncReadExt;
use futures::future::join_all;

use std::path::Path;
use std::fmt::{Display, Formatter};
use std::error::Error;
use std::process::Stdio;
use std::sync::{Mutex, Arc};

/// Any error that can occur when running a `TestCase`.
#[derive(Debug)]
pub enum TestCaseError {
    /// Generic message
    Message(String),

    /// A command could not be run
    LaunchFailed(std::io::Error),

    /// A command returned a non-zero exit code which was not ignored
    NonzeroExit(String),

    /// An IO-Error occured
    IO(std::io::Error),
}

impl Display for TestCaseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TestCaseError::Message(str) => write!(f, "{}", str),
            TestCaseError::LaunchFailed(err) => write!(f, "Command \"{}\" failed to launch", err),
            TestCaseError::NonzeroExit(str) => write!(f, "Command \"{}\" terminated with a non-zero exit code", str),
            TestCaseError::IO(err) => write!(f, "An IO Error occurred: {}", err)
        }
    }
}

impl Error for TestCaseError {}

/// Constructs a shell command from a `command_string`, with the given `working_directory`
/// and shell command and arguments from `options`.
/// 
/// By default, all standard streams are discarded (e.g. piped into `/dev/null`), so they have
/// to be reenabled if input/output is to be used.
/// This function does not actually run the command but merely builds an instance of `tokio::process::Command`
/// that can be further modified and run later.
fn build_shell_command(command_string: &str, working_directory: &Path, options: &TestOptions) -> Command {
    let mut command = Command::new(&options.shell);
    command
        .args(&options.shell_args)
        .current_dir(working_directory)
        .arg("-c")
        .arg(command_string)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    return command;
}

/// Converts the output from `.await`-ing an instance of `tokio::process::Command` into the
/// appropriate `TestCaseError`.
/// 
/// Utility function.
fn get_error_from_command_result(res: std::io::Result<std::process::ExitStatus>, shell_command: &str, ignore_exit_code: bool) -> Result<(), TestCaseError> {
    match res {
        // Command launched
        Ok(exit_status) => {
            let exit_code = exit_status.code();
            if exit_code.is_none() {
                // Command was interrupted
                return Err(TestCaseError::Message(format!("Command \"{}\" terminated unexpectedly", shell_command)));
            } else if exit_code.unwrap() != 0 && !ignore_exit_code {
                // Command terminated with a non-zero exit code and the exit codes are not ignored
                return Err(TestCaseError::NonzeroExit(String::from(shell_command)));
            } else {
                // Command is considered to have terminated successfully
                return Ok(());
            }
        },

        // Command failed to launch
        Err(e) => return Err(TestCaseError::LaunchFailed(e))
    }
}

/// Runs the main command of the `test_case`, sending the command's stdout to the
/// `process_stdout` function and the command's stderr to the `process_stderr` function.
/// 
/// The two functions receive the command's output in blocks of bytes. The blocks will
/// always have at least length 1, but apart from that, no other assumptions should
/// be made about the content of the blocks. It is especially not guaranteed, that
/// the blocks will have a consistent length or that the end of a block matches up
/// with the end of a utf-8 char.
pub async fn process_test_case_command<A, B>(test_case: &TestCase, mut process_stdout: A, mut process_stderr: B) -> Result<(), TestCaseError>
where 
    A: FnMut(&[u8]),
    B: FnMut(&[u8])
{
    let child_res = build_shell_command(&test_case.command, &test_case.working_directory, &test_case.options)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn();

    let mut child = match child_res {
        Ok(child) => child,
        Err(e) => return Err(TestCaseError::IO(e))
    };

    let mut stdout_buf = Vec::<u8>::with_capacity(test_case.options.buffer_size);
    let mut stdout_offset = 0;
    let mut stdout_open = true;
    stdout_buf.resize(test_case.options.buffer_size, 0);

    let mut stderr_buf = Vec::<u8>::with_capacity(test_case.options.buffer_size);
    let mut stderr_offset = 0;
    let mut stderr_open = true;
    stderr_buf.resize(test_case.options.buffer_size, 0);

    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();

    while stdout_open || stderr_open {
        tokio::select!(
            res_out = stdout.read(&mut stdout_buf[stdout_offset..]), if stdout_open => {
                // Handle data on stdout

                match res_out {
                    Ok(bytes_read) => {
                        // Data available on stdout
                        stdout_offset += bytes_read;

                        if bytes_read == 0 {
                            // 0 bytes read means that the stream was closed
                            process_stdout(&stdout_buf[..stdout_offset]);
                            stdout_open = false;
                        } else if stdout_offset >= stdout_buf.len() {
                            // Buffer is full, send it to the processing function
                            process_stdout(&stdout_buf);
                            stdout_offset = 0;
                        }
                    },
                    Err(e) => {
                        // IO error occurred on stdout
                        return Err(TestCaseError::IO(e))
                    }
                }
            },
            res_err = stderr.read(&mut stderr_buf[stderr_offset..]), if stderr_open => {
                // Handle data on stderr

                match res_err {
                    Ok(bytes_read) => {
                        // Data available on stderr
                        stderr_offset += bytes_read;

                        if bytes_read == 0 {
                            // 0 bytes read means that the stream was closed
                            process_stderr(&stderr_buf[..stderr_offset]);
                            stderr_open = false;
                        } else if stderr_offset >= stderr_buf.len() {
                            // Buffer is full, send it to the processing function
                            process_stderr(&stderr_buf);
                            stderr_offset = 0;
                        }
                    },
                    Err(e) => {
                        // IO error occurred on stderr
                        return Err(TestCaseError::IO(e))
                    }
                }
            }
        );
    }

    let command_res = child
        .wait()
        .await;
    get_error_from_command_result(command_res, &test_case.command, test_case.options.ignore_exit_code)?;

    return Ok(()); // TODO replace with resulting diff
}

/// Processes a single `test_case`.
/// 
/// This function runs the prelaunch, command and postlaunch scripts of a 
/// TestCase in order, returning an error if any of the commands fails.
pub async fn process_single_test_case<P: ProgressReporter>(test_case: TestCase, progress_reporter: Arc<Mutex<P>>) -> Result<(), TestCaseError>{
    trace!("Starting test case {}...", test_case.name);

    progress_reporter.lock().unwrap().set_test_case_state(&test_case, crate::test_cases::reporter::State::Running);
    
    // Run prelaunch script
    if let Some(command) = &test_case.prelaunch {
        trace!("Running prelaunch script for {}...", test_case.name);
        let res = build_shell_command(command, &test_case.working_directory, &test_case.options).status().await;

        get_error_from_command_result(res, command, test_case.options.ignore_exit_code)
            .map_err(|err| {
                progress_reporter.lock().unwrap()
                    .set_test_case_state(&test_case, State::Error(format!("{}", err)));
                err
            })?;
    }

    // Run command
    process_test_case_command(
        &test_case,
        |_stdout_bef| {},
        |_stderr_bef| {}
    ).await
    .map_err(|err| {
        progress_reporter.lock().unwrap()
            .set_test_case_state(&test_case, State::Error(format!("{}", err)));
        err
    })?;

    // Run postlaunch script
    if let Some(command) = &test_case.postlaunch {
        trace!("Running postlaunch script for {}...", test_case.name);
        let res = build_shell_command(command, &test_case.working_directory, &test_case.options).status().await;
        
        get_error_from_command_result(res, command, test_case.options.ignore_exit_code)
            .map_err(|err| {
                progress_reporter.lock().unwrap()
                    .set_test_case_state(&test_case, State::Error(format!("{}", err)));
                err
            })?;
    }

    progress_reporter.lock().unwrap().set_test_case_state(&test_case, crate::test_cases::reporter::State::Passed);
    return Ok(());
}

/// Concurrently processes all given `test_cases`.
pub async fn process_test_cases<T>(test_cases: Vec<TestCase>, mut terminal: T) -> ()
where
    T: Terminal + Send + Clone + 'static
{
    let progress_reporter_mutex = Arc::new(Mutex::new(DefaultProgressReporter::new()));

    // Clone of the progress_reporter_mutex that gets moved into the
    // coroutine. Because the ProgressReporter lives behind an Arc,
    // the clone is actually just another reference to the same object.
    let c_progress_reporter_mutex = progress_reporter_mutex.clone();

    // Same thing with the terminal
    let mut c_terminal = terminal.clone();

    // Setup progress reporter. The progress reporter will receive updates from
    // the TestCase processes and print the current state to the terminal every 30ms.
    let progress_reporter_coro = tokio::spawn(async move {
        let mut delay = tokio::time::interval(tokio::time::Duration::from_millis(30));
        loop {
            // The first await will complete immediately, so put it at the front of the loop.
            delay.tick().await;
            let mut progress_reporter_lock = c_progress_reporter_mutex.lock().unwrap();
            progress_reporter_lock
                .write_status(&mut c_terminal)
                .unwrap_or_else(|e| error!("Error writing progress report: {}", e));
            progress_reporter_lock.clear_changes();
        }
    });
    
    // Run all test_cases
    let futures: Vec<tokio::task::JoinHandle<_>> = test_cases.into_iter()
        .map(|tc| {
            let c_progress_reporter_mutex = progress_reporter_mutex.clone();
            tokio::spawn(process_single_test_case(tc, c_progress_reporter_mutex))
        })
        .collect();
    let _results = join_all(futures).await;

    // Stop the progress reporter coroutine
    progress_reporter_coro.abort();

    // Write a final status line
    progress_reporter_mutex.lock().unwrap()
        .write_status(&mut terminal)
        .unwrap_or_else(|e| error!("Error writing progress report: {}", e));
    terminal.write("\n")
        .unwrap_or_else(|e| error!("Error writing progress report: {}", e));
}

#[cfg(test)]
mod tests {

    use super::process_test_case_command;
    use crate::test_cases::get_default_test_options;
    use crate::test_cases::parser::parse_test_case;

    use std::path::PathBuf;

    #[tokio::test]
    async fn process_test_case_command_captures_standard_streams() {
        let test_case = parse_test_case(&PathBuf::from("examples/rehearse.json"), &get_default_test_options()).unwrap();

        let stdout_expected = "Hello, World!\n";
        let mut stdout_actual = Vec::<u8>::with_capacity(stdout_expected.len());

        let stderr_expected = "Hello, Error!\n";
        let mut stderr_actual = Vec::<u8>::with_capacity(stderr_expected.len());

        process_test_case_command(
            &test_case,
            |buf| {
                println!("{}", buf.len());
                for b in buf {
                    stdout_actual.push(*b);
                }
            },
            |buf| {
                for b in buf {
                    stderr_actual.push(*b);
                }
            },
        ).await.unwrap();

        assert_eq!(String::from_utf8(stdout_actual).unwrap(), stdout_expected, "Error in stdout capture");
        assert_eq!(String::from_utf8(stderr_actual).unwrap(), stderr_expected, "Error in stdout capture");
    }
}
