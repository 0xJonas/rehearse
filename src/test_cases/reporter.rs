use super::{TestCase};
use crate::cli::{Terminal, TermColor, write_blanks};

use log::trace;

use std::collections::HashMap;

/// States that a `TestCase` can be in during or after execution.
pub enum State {
    /// The `TestCase` is currently running.
    Running,

    /// The `TestCase` is not yet started. This is the default state for newly registered TestCases.
    Pending,

    /// The `TestCase` has finished and no difference to the expected output was found.
    Passed,

    /// The `TestCase` has finished, but its output did not match the expected output.
    Diff,

    /// The `TestCase` did not finish successfully.
    Error(String)
}

/// Represents an object that can receive status updates from TestCases and write
/// a formatted report to the terminal.
pub trait ProgressReporter {

    /// Registers a new `test_case` to be tracked by this `ProgressReporter`.
    fn register_test_case(&mut self, test_case: &TestCase) -> ();

    /// Sets the state of a `test_case` to `state`.
    /// 
    /// The `TestCase` must have been previously registered with `register_test_case`.
    fn set_test_case_state(&mut self, test_case: &TestCase, state: State) -> ();

    /// Writes the current state of the observed `TestCase`s to the `terminal`.
    fn write_status<T: Terminal>(&self, terminal: &mut T) -> std::io::Result<()>;

    /// Resets any internal change flags of the ProgressReporter.
    /// 
    /// An implementation might choose to only display changed TestCases, instead of
    /// a complete snapshot of TestCase states.
    fn clear_changes(&mut self) -> ();
}

/// The default ProgressReporter implementation.
pub struct DefaultProgressReporter {
    states: HashMap<String, State>,
    changed_test_cases: Vec<String>
}

impl DefaultProgressReporter {

    /// Creates a new empty `DefaultProgressReporter`.
    pub fn new() -> DefaultProgressReporter {
        DefaultProgressReporter {
            states: HashMap::new(),
            changed_test_cases: Vec::new()
        }
    }
}

impl ProgressReporter for DefaultProgressReporter {

    fn register_test_case(&mut self, test_case: &TestCase) {
        self.states.insert(test_case.name.clone(), State::Pending);
        self.changed_test_cases.push(test_case.name.clone());
    }

    fn set_test_case_state(&mut self, test_case: &TestCase, state: State) {
        // TODO: check if the given TestCase was actually registered and do something
        // if it wasn't.
        match &state {
            State::Pending => trace!("Test case {} is pending", test_case.name),
            State::Running => trace!("Test case {} is running", test_case.name),
            State::Passed => trace!("Test case {} passed", test_case.name),
            State::Diff => trace!("Test case {} has a diff", test_case.name),
            State::Error(msg) => trace!("Test case {} encountered an error: {}", test_case.name, msg)
        }

        self.states.insert(test_case.name.clone(), state);
        if !self.changed_test_cases.contains(&test_case.name) {
            self.changed_test_cases.push(test_case.name.clone());
        }
    }
    
    fn write_status<T: Terminal>(&self, terminal: &mut T) -> std::io::Result<()> {
        // Clear status line
        if let Ok((width, _)) = terminal.size() {
            terminal.write("\r")?;
            write_blanks(terminal, width)?;
            terminal.write("\r")?;
        }

        // Report newly finished test cases
        for name in &self.changed_test_cases {
            terminal.reset_color()?;

            match self.states.get(name) {
                Some(State::Passed) => {
                    terminal.write(format!("{}: ", name))?;
                    terminal.set_color_fg(&TermColor::Highlight)?;
                    terminal.write("Passed\n")?;
                },
                Some(State::Diff) => {
                    terminal.write(format!("{}: ", name))?;
                    terminal.set_color_fg(&TermColor::Warning)?;
                    terminal.write("Diff found\n")?;
                },
                Some(State::Error(msg)) => {
                    terminal.write(format!("{}: ", name))?;
                    terminal.set_color_fg(&TermColor::Error)?;
                    terminal.write("Error\n")?;
                    terminal.reset_color()?;
                    terminal.write(format!("  {}\n", msg))?;
                },
                _ => {} // Do nothing
            }
        }

        // Write status line
        let (running, pending, passed, failed, error) = self.states
            .values()
            .fold((0, 0, 0, 0, 0), |(running, pending, passed, failed, error), state| match state {
                State::Running => (running + 1, pending, passed, failed, error),
                State::Pending => (running, pending + 1, passed, failed, error),
                State::Passed => (running, pending, passed + 1, failed, error),
                State::Diff => (running, pending, passed, failed + 1, error),
                State::Error(_) => (running, pending, passed, failed, error + 1)
            });
        terminal.set_color_fg(&TermColor::Highlight)?;
        terminal.write(format!("<< Status: {} running, {} pending, {} passed, {} failed, {} error(s) >>", running, pending, passed, failed, error))?;
        // Move cursor back to the left, so the status line does not stay on screen
        // when another coroutine writes to the terminal.
        terminal.write("\r")?;
        terminal.flush()?;

        return Ok(());
    }

    fn clear_changes(&mut self) -> () {
        self.changed_test_cases.clear();
    }
}
