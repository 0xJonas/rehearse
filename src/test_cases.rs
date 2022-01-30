use glob::glob;
use log::{error, trace};
use serde_json::{Value, from_reader};
use tokio::process::Command;
use tokio::io::AsyncReadExt;

use std::path::{Path, PathBuf};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::process::Stdio;

const KEY_VERSION: &'static str = "version";
const KEY_NAME: &'static str = "name";
const KEY_COMMAND: &'static str = "command";
const KEY_PRELAUNCH: &'static str = "prelaunch";
const KEY_POSTLAUNCH: &'static str = "postlaunch";
const KEY_REFERENCE_STDOUT: &'static str = "referenceStdout";
const KEY_REFERENCE_STDERR: &'static str = "referenceStderr";

const KEY_ARTIFACTS: &'static str = "artifacts";
const KEY_ARTIFACT_NAME: &'static str = "name";
const KEY_ARTIFACT_REFERENCE: &'static str = "reference";
const KEY_ARTIFACT_ENCODING: &'static str = "encoding";
const KEY_ARTIFACT_IS_BINARY_FILE: &'static str = "isBinaryFile";

const KEY_OPTIONS: &'static str = "options";
const KEY_OPTION_IGNORE_EXIT_CODE: &'static str = "ignoreExitCode";
const KEY_OPTION_DELIMITER_TAG: &'static str = "delimiterTag";
const KEY_OPTION_SHELL: &'static str = "shell";
const KEY_OPTION_SHELL_ARGS: &'static str = "shellArgs";
const KEY_OPTION_BUFFER_SIZE: &'static str = "bufferSize";

const DEFAULT_OPTION_IGNORE_EXIT_CODE: bool = false;
const DEFAULT_OPTION_DELIMITER_TAG: &'static str = "";
const DEFAULT_OPTION_SHELL: &'static str = "sh";
const DEFAULT_OPTION_BUFFER_SIZE: usize = 1 << 20; // 1 MiB

const MAX_BUFFER_SIZE: usize = 1 << 30; // 1 GiB

const DEFAULT_ARTIFACT_ENCODING: &'static str = "utf-8";

/// Additional options for a TestCase
#[derive(Debug, Clone)]
pub struct TestOptions {
    pub ignore_exit_code: bool,
    pub delimiter_tag: String,
    pub shell: String,
    pub shell_args: Vec<String>,
    pub buffer_size: usize
}

pub fn get_default_test_options() -> TestOptions {
    TestOptions {
        ignore_exit_code: DEFAULT_OPTION_IGNORE_EXIT_CODE,
        delimiter_tag: String::from(DEFAULT_OPTION_DELIMITER_TAG),
        shell: String::from(DEFAULT_OPTION_SHELL),
        shell_args: Vec::with_capacity(0),
        buffer_size: DEFAULT_OPTION_BUFFER_SIZE
    }
}

#[derive(Debug)]
pub struct ArtifactConfig {
    pub name: PathBuf,
    pub reference: PathBuf,
    pub encoding: String,
    pub is_binary_file: bool
}

/// A test case as contained in a rehearse.json.
#[derive(Debug)]
pub struct TestCase {
    pub version: String,
    pub name: String,
    pub working_directory: PathBuf,
    pub command: String,
    pub prelaunch: Option<String>,
    pub postlaunch: Option<String>,
    pub reference_stdout: Option<PathBuf>,
    pub reference_stderr: Option<PathBuf>,
    pub artifacts: Vec<ArtifactConfig>,
    pub options: TestOptions
}

/// Error returned when the parsing of rehearse.json failed.
/// The variant contains more information about the failure.
#[derive(Debug)]
pub enum ParseError {
    /// This error generally indicates that rehearse.json was
    /// read successfully, but some of its contents were invalid.
    /// E.g. wrong type for certain fields, referenced files not existing etc.
    BadContent(String),

    /// An IO Error occurred.
    IOError(std::io::Error),

    /// JSON file was malformed
    JSONError(serde_json::error::Error)
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::BadContent(message) => write!(f, "{}", message),
            ParseError::IOError(err) => write!(f, "{}", err),
            ParseError::JSONError(err) => write!(f, "{}", err)
        }
    }
}

impl Error for ParseError {

    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ParseError::BadContent(_) => None,
            ParseError::IOError(err) => Some(err),
            ParseError::JSONError(err) => Some(err)
        }
    }
}

fn ensure_absolute_path(maybe_relative: PathBuf, working_directory: &PathBuf) -> PathBuf {
    if maybe_relative.is_absolute() {
        return maybe_relative;
    } else {
        let mut out = working_directory.clone();
        out.push(maybe_relative);
        return out;
    }
}

/// Read the field `key` in JSON object `obj`. Returns the content on success or a
/// ParseError if the value was not a string or the field did not exist.
fn read_required_string_field(obj: &serde_json::Map<String, Value>, key: &str) -> Result<String, ParseError> {
    match obj.get(key) {
        Some(Value::String(str)) => Ok(str.clone()),
        Some(_) => Err(ParseError::BadContent(format!("Field \"{}\" must be a string", key))),
        _ => Err(ParseError::BadContent(format!("Missing required field \"{}\"", key)))
    }
}

/// Read the field `key` in JSON object `obj`. Returns the content on success,
/// a ParseError if the value was not a string, or None if the field did not exist.
fn read_optional_string_field(obj: &serde_json::Map<String, Value>, key: &str) -> Result<Option<String>, ParseError> {
    match obj.get(key) {
        Some(Value::String(str)) => Ok(Some(str.clone())),
        Some(_) => Err(ParseError::BadContent(format!("Field \"{}\" must be a string", key))),
        None => Ok(None)
    }
}

/// Parses an element of the artifacts array in rehearse.json.
fn parse_artifact(obj: &serde_json::Map<String, Value>, working_directory: &PathBuf) -> Result<ArtifactConfig, ParseError> {
    let name_raw = read_required_string_field(obj, KEY_ARTIFACT_NAME)?;
    let name = ensure_absolute_path(PathBuf::from(name_raw), working_directory);

    let reference_raw = read_required_string_field(obj, KEY_ARTIFACT_REFERENCE)?;
    let reference = ensure_absolute_path(PathBuf::from(reference_raw), working_directory);

    let encoding = read_optional_string_field(obj, KEY_ARTIFACT_ENCODING)?.unwrap_or(String::from(DEFAULT_ARTIFACT_ENCODING));

    let is_binary_file = match obj.get(KEY_ARTIFACT_IS_BINARY_FILE) {
        Some(Value::Bool(b)) => *b,
        Some(_) => return Err(ParseError::BadContent(format!("Field \"{}\" must be a bool", KEY_ARTIFACT_IS_BINARY_FILE))),
        None => false
    };

    return Ok(ArtifactConfig {
        name: name,
        reference: reference,
        encoding: encoding,
        is_binary_file: is_binary_file
    });
}

/// Parses the artifacts array in rehearse.json.
fn parse_artifacts(array: &Vec<Value>, working_directory: &PathBuf) -> Result<Vec<ArtifactConfig>, ParseError> {
    trace!("Parsing artifacts...");
    let mut out = Vec::with_capacity(array.len());
    for v in array.iter() {
        if let Value::Object(obj) = v {
            let artifact_config = parse_artifact(obj, working_directory)?;
            out.push(artifact_config);
        } else {
            return Err(ParseError::BadContent(format!("Field \"{}\" must be an array of objects", KEY_ARTIFACTS)));
        }
    }
    return Ok(out);
}

/// Reads the contents of the "options" field in rehearse.json from
/// the given JSON object.
fn parse_options(obj: &serde_json::Map<String, Value>, defaults: &TestOptions) -> Result<TestOptions, ParseError> {
    trace!("Parsing options...");
    let ignore_exit_code = match obj.get(KEY_OPTION_IGNORE_EXIT_CODE) {
        Some(Value::Bool(b)) => *b,
        Some(_) => {
            return Err(ParseError::BadContent(format!("Field \"{}\" must be a bool", KEY_OPTION_IGNORE_EXIT_CODE)));
        },
        _ => {
            defaults.ignore_exit_code
        }
    };

    let delimiter_tag = read_optional_string_field(obj, KEY_OPTION_DELIMITER_TAG)?.unwrap_or(defaults.delimiter_tag.clone());
    let shell = read_optional_string_field(obj, KEY_OPTION_SHELL)?.unwrap_or(defaults.shell.clone());
    
    let shell_args = match obj.get(KEY_OPTION_SHELL_ARGS) {
        Some(Value::Array(values)) => {
            let mut out = Vec::with_capacity(values.len());
            for v in values {
                if let Value::String(s) = v {
                    out.push(String::from(s));
                } else {
                    return Err(ParseError::BadContent(format!("Field \"{}\" must be an array of strings", KEY_OPTION_SHELL_ARGS)));
                }
            }
            out
        }
        Some(_) => {
            return Err(ParseError::BadContent(format!("Field \"{}\" must be an array of strings", KEY_OPTION_SHELL_ARGS)));
        }
        _ => defaults.shell_args.clone()
    };

    let buffer_size = match obj.get(KEY_OPTION_BUFFER_SIZE) {
        Some(Value::Number(v)) => {
            if v.is_u64() {
                let size = v.as_u64().unwrap();
                if size < MAX_BUFFER_SIZE as u64 && size != 0{
                    size as usize
                } else {
                    return Err(ParseError::BadContent(format!("Field \"{}\" must be an integer between 1 Byte and 1 GiByte", KEY_OPTION_BUFFER_SIZE)));
                }
            } else {
                return Err(ParseError::BadContent(format!("Field \"{}\" must be an integer between 1 Byte and 1 GiByte", KEY_OPTION_BUFFER_SIZE)));
            }
        },
        Some(_) => {
            return Err(ParseError::BadContent(format!("Field \"{}\" must be an integer between 1 Byte and 1 GiByte", KEY_OPTION_BUFFER_SIZE)));
        },
        _ => defaults.buffer_size
    };

    return Ok(TestOptions {
        ignore_exit_code: ignore_exit_code,
        delimiter_tag: delimiter_tag,
        shell: shell,
        shell_args: shell_args,
        buffer_size: buffer_size
    });
}

/// Parses the test case specification located at `path`. This file
/// will almost always be called 'rehearse.json', but this function does
/// not enforce this.
pub fn parse_test_case(path: &Path, defaults: &TestOptions) -> Result<TestCase, ParseError> {
    trace!("Opening file {}...", path.display());
    let file = match std::fs::File::open(path) {
        Ok(f) => f,
        Err(err) => {
            return Err(ParseError::IOError(err));
        }
    };

    trace!("Parsing {}...", path.display());
    let json_data: Value = match from_reader(file) {
        Ok(data) => data,
        Err(err) => {
            return Result::Err(ParseError::JSONError(err));
        }
    };

    let root = match json_data {
        Value::Object(map) => map,
        _ => return Result::Err(ParseError::BadContent(String::from("Root element must be a JSON object")))
    };
    
    let version = read_required_string_field(&root, KEY_VERSION)?;
    let command = read_required_string_field(&root, KEY_COMMAND)?;
    let name = read_required_string_field(&root, KEY_NAME)?;

    let working_directory = match path.parent() {
        Some(parent) => PathBuf::from(parent),
        None => PathBuf::from(".")
    };

    let prelaunch = read_optional_string_field(&root, KEY_PRELAUNCH)?;
    let postlaunch = read_optional_string_field(&root, KEY_POSTLAUNCH)?;

    let reference_stdout_raw = read_optional_string_field(&root, KEY_REFERENCE_STDOUT)?;
    let reference_stdout = if let Some(ref ref_file) = reference_stdout_raw {
        let path = ensure_absolute_path(PathBuf::from(ref_file), &working_directory);
        if path.exists() {
            Some(path)
        } else {
            return Err(ParseError::BadContent(format!("File {} not found", ref_file)));
        }
    } else {
        None
    };

    let reference_stderr = match root.get(KEY_REFERENCE_STDERR) {
        Some(Value::String(str)) => {
            let path = ensure_absolute_path(PathBuf::from(str), &working_directory);
            if path.exists() {
                Some(path)
            } else {
                return Err(ParseError::BadContent(format!("File {} not found", str)));
            }
        },
        Some(_) => {
            return Err(ParseError::BadContent(format!("Field \"{}\" must be a string", KEY_REFERENCE_STDERR)));
        },
        _ => {
            None
        }
    };

    let artifacts = match root.get(KEY_ARTIFACTS) {
        Some(Value::Array(array)) => match parse_artifacts(array, &working_directory) {
            Ok(artifacts) => artifacts,
            Err(e) => return Result::Err(e)
        },
        Some(_) => {
            return Err(ParseError::BadContent(format!("Field \"{}\" must be an array of objects", KEY_ARTIFACTS)));
        },
        _ => {
            Vec::new()
        }
    };

    let options = match root.get(KEY_OPTIONS) {
        Some(Value::Object(obj)) => parse_options(obj, defaults)?,
        Some(_) => {
            return Err(ParseError::BadContent(format!("Field \"{}\" must be an object", KEY_OPTIONS)));
        },
        _ => {
            trace!("No options found. Using defaults...");
            defaults.clone()
        }
    };

    return Ok(TestCase {
        version: version,
        name: name,
        working_directory: working_directory,
        command: command,
        prelaunch: prelaunch,
        postlaunch: postlaunch,
        reference_stdout: reference_stdout,
        reference_stderr: reference_stderr,
        artifacts: artifacts,
        options: options
    });
}

pub fn discover_test_cases(files: &str, defaults: &TestOptions) -> Vec<TestCase> {
    trace!("Looking for tests in {}...", files);

    let mut test_cases = Vec::<TestCase>::new();
    
    let parsed_glob = match glob(files) {
        Ok(iter) => iter,
        Err(err) => {
            error!("Failed to parse glob {}: {}", files, err);
            return test_cases;
        }
    };

    for path_res in parsed_glob {
        let path = match path_res {
            Ok(p) => p,
            Err(e) => {
                error!("Error matching glob: {:?}", e);
                continue;
            }
        };
        let file_name = path.file_name();
        if file_name.is_some() && file_name.unwrap() == "rehearse.json" {
            match parse_test_case(&path, defaults) {
                Ok(test_case) => {
                    trace!("Adding test case {}", path.display());
                    trace!("{:?}", test_case);
                    test_cases.push(test_case)
                },
                Err(err) => match err {
                    ParseError::BadContent(msg) => {
                        error!("{}", msg);
                    },
                    ParseError::IOError(io_error) => {
                        error!("Error opening file {}: {}", path.display(), io_error);
                    },
                    ParseError::JSONError(json_error) => {
                        error!("Test case {} could not be parsed: {}", path.display(), json_error);
                    }
                }
            }
        }
    }

    return test_cases;
}

#[derive(Debug)]
pub enum TestCaseError {
    /// Generic message
    Message(String),

    /// A command could not be run
    LaunchFailed(String),

    /// A command returned a non-zero exit code which was not ignored
    NonzeroExit(String),

    /// An IO-Error occured
    IO(std::io::Error),
}

impl Display for TestCaseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TestCaseError::Message(str) => write!(f, "{}", str),
            TestCaseError::LaunchFailed(str) => write!(f, "Command \"{}\" failed to launch", str),
            TestCaseError::NonzeroExit(str) => write!(f, "Command \"{}\" terminated with a non-zero exit code", str),
            TestCaseError::IO(err) => write!(f, "An IO Error occurred: {}", err)
        }
    }
}

impl Error for TestCaseError {}

pub struct RunOptions {
    pub parallel: bool,
    pub shell: String
}

fn build_shell_command(command_string: &str, options: &TestOptions) -> Command {
    let mut command = Command::new(&options.shell);
    command
        .args(&options.shell_args)
        .arg("-C")
        .arg(command_string)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    return command;
}

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
        Err(e) => return Err(TestCaseError::IO(e))
    }
}

pub async fn process_test_case_command<A, B>(test_case: &TestCase, mut process_stdout: A, mut process_stderr: B) -> Result<(), TestCaseError>
where 
    A: FnMut(&[u8]),
    B: FnMut(&[u8])
{
    let child_res = build_shell_command(&test_case.command, &test_case.options)
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

    let mut stderr_buf = Vec::<u8>::with_capacity(test_case.options.buffer_size);
    let mut stderr_offset = 0;
    let mut stderr_open = true;

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
                            process_stdout(&stdout_buf[..stderr_offset]);
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

pub async fn process_single_test_case(test_case: &TestCase) -> Result<(), TestCaseError>{
    trace!("Starting test case {}...", test_case.name);

    // Run prelaunch script
    if let Some(command) = &test_case.prelaunch {
        trace!("Running prelaunch script for {}...", test_case.name);
        let res = build_shell_command(command, &test_case.options).status().await;
        get_error_from_command_result(res, command, test_case.options.ignore_exit_code)?;
    }

    // Run command
    process_test_case_command(
        test_case,
        |_stdout_bef| {},
        |_stderr_bef| {}
    ).await?;

    // Run postlaunch script
    if let Some(command) = &test_case.postlaunch {
        trace!("Running postlaunch script for {}...", test_case.name);
        let res = build_shell_command(command, &test_case.options).status().await;
        get_error_from_command_result(res, command, test_case.options.ignore_exit_code)?;
    }

    return Ok(());
}

pub fn process_test_cases(test_cases: &Vec<TestCase>) {
    
}

#[cfg(test)]
mod test {

    use crate::test_cases::{get_default_test_options, parse_test_case, discover_test_cases, ParseError};
    use std::path::PathBuf;

    #[test]
    fn test_discover_test_cases() {
        let defaults = get_default_test_options();
        let mut tests = discover_test_cases("examples/*", &defaults);
        tests.sort_by_key(|tc| tc.name.clone());
        assert_eq!(tests.len(), 1);

        // Test cases in lexicographical order
        assert_eq!(tests[0].name, "Hello, Test!");
    }

    #[test]
    fn parse_test_case_hello_world() {
        let defaults = get_default_test_options();
        match parse_test_case(&PathBuf::from("examples/rehearse.json"), &defaults) {
            Ok(test_case) => {
                assert_eq!(test_case.version, "0.1");
                assert_eq!(test_case.name, "Hello, Test!");
                assert_eq!(test_case.prelaunch, Some(String::from("echo \"prelaunch\"")));
                assert_eq!(test_case.command, "echo \"Hello, World!\n\"; echo \"Hello, File!\n\" > output.txt");
                assert_eq!(test_case.postlaunch, Some(String::from("echo \"postlaunch\"")));
                assert_eq!(test_case.reference_stdout, Some(PathBuf::from("examples/stdout.expected")));
                assert_eq!(test_case.artifacts.len(), 1);
                if let Some(artifact_config) = test_case.artifacts.get(0) {
                    assert_eq!(artifact_config.name, PathBuf::from("examples/output.txt"));
                    assert_eq!(artifact_config.reference, PathBuf::from("examples/output.txt.expected"));
                    assert_eq!(artifact_config.encoding, "utf-8");
                    assert_eq!(artifact_config.is_binary_file, false);
                } else {
                    assert!(false);
                }
            },
            Err(e) => {
                println!("{}", e);
                assert!(false);
            }
        };
    }

    #[test]
    fn parse_test_case_bad_format_1() {
        let defaults = get_default_test_options();
        match parse_test_case(&PathBuf::from("examples/failing-tests/bad_format_1.json"), &defaults) {
            Ok(_) => assert!(false, "Test case with invalid format should not parse."),
            Err(e) => match e {
                ParseError::BadContent(msg) => assert_eq!(msg, "File does_not_exist not found"),
                _ => assert!(false, "Got the wrong error")
            }
        }
    }

    #[test]
    fn parse_test_case_bad_format_2() {
        let defaults = get_default_test_options();
        match parse_test_case(&PathBuf::from("examples/failing-tests/bad_format_2.json"), &defaults) {
            Ok(_) => assert!(false, "Test case with invalid format should not parse."),
            Err(e) => match e {
                ParseError::BadContent(msg) => assert_eq!(msg, "Field \"artifacts\" must be an array of objects"),
                _ => assert!(false, "Got the wrong error")
            }
        }
    }

    #[test]
    fn parse_test_case_bad_format_3() {
        let defaults = get_default_test_options();
        match parse_test_case(&PathBuf::from("examples/failing-tests/bad_format_3.json"), &defaults) {
            Ok(_) => assert!(false, "Test case with invalid format should not parse."),
            Err(e) => match e {
                ParseError::BadContent(msg) => assert_eq!(msg, "Missing required field \"version\""),
                _ => assert!(false, "Got the wrong error")
            }
        }
    }

    #[test]
    fn parse_test_case_bad_format_4() {
        let defaults = get_default_test_options();
        match parse_test_case(&PathBuf::from("examples/failing-tests/bad_format_4.json"), &defaults) {
            Ok(_) => assert!(false, "Test case with invalid format should not parse."),
            Err(e) => match e {
                ParseError::JSONError(_) => {},
                _ => assert!(false, "Got the wrong error")
            }
        }
    }
}