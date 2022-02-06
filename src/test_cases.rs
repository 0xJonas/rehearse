pub mod parser;
pub mod runner;
mod reporter;

use std::path::PathBuf;

const DEFAULT_OPTION_IGNORE_EXIT_CODE: bool = false;
const DEFAULT_OPTION_DELIMITER_TAG: &'static str = "";
const DEFAULT_OPTION_SHELL: &'static str = "sh";
const DEFAULT_OPTION_BUFFER_SIZE: usize = 1 << 20; // 1 MiB

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
