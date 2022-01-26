use glob::glob;
use log::{error, debug, trace};
use serde_json::{Value, from_reader};

use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};

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

const DEFAULT_IGNORE_EXIT_CODE: bool = false;
const DEFAULT_DELIMITER_TAG: &'static str = "";
const DEFAULT_ARTIFACT_ENCODING: &'static str = "utf-8";

/// Additional options for a TestCase
#[derive(Debug)]
pub struct TestOptions {
    pub ignore_exit_code: bool,
    pub delimiter_tag: String
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

    let encoding = read_optional_string_field(obj, KEY_ARTIFACT_ENCODING)?.map_or(String::from(DEFAULT_ARTIFACT_ENCODING), |e| e);

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
fn parse_options(obj: &serde_json::Map<String, Value>) -> Result<TestOptions, ParseError> {
    trace!("Parsing options...");
    let ignore_exit_code = match obj.get(KEY_OPTION_IGNORE_EXIT_CODE) {
        Some(Value::Bool(b)) => *b,
        Some(_) => {
            return Err(ParseError::BadContent(format!("Field \"{}\" must be a bool", KEY_OPTION_IGNORE_EXIT_CODE)));
        },
        _ => {
            DEFAULT_IGNORE_EXIT_CODE
        }
    };

    let delimiter_tag = match obj.get(KEY_OPTION_DELIMITER_TAG) {
        Some(Value::String(str)) => str.clone(),
        Some(_) => {
            return Err(ParseError::BadContent(format!("Field \"{}\" must be a string", KEY_OPTION_DELIMITER_TAG)));
        },
        _ => {
            String::from(DEFAULT_DELIMITER_TAG)
        }
    };

    return Ok(TestOptions {
        ignore_exit_code: ignore_exit_code,
        delimiter_tag: delimiter_tag
    });
}

/// Parses the test case specification located at `path`. This file
/// will almost always be called 'rehearse.json', but this function does
/// not enforce this.
pub fn parse_test_case(path: &Path) -> Result<TestCase, ParseError> {
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
        Some(Value::Object(obj)) => parse_options(obj)?,
        Some(_) => {
            return Err(ParseError::BadContent(format!("Field \"{}\" must be an object", KEY_OPTIONS)));
        },
        _ => {
            trace!("No options found. Using defaults...");
            TestOptions {
                ignore_exit_code: DEFAULT_IGNORE_EXIT_CODE,
                delimiter_tag: String::from(DEFAULT_DELIMITER_TAG)
            }
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

pub fn discover_test_cases(files: &str) -> Vec<TestCase> {
    trace!("Looking for tests in {}...", files);

    let mut test_cases = Vec::<TestCase>::new();
    
    let parsed_glob = match glob(files) {
        Ok(iter) => iter,
        Err(err) => {
            error!("Failed to parse glob!");
            debug!("{:?}", err);
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
            match parse_test_case(&path) {
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

#[cfg(test)]
mod test {

    use crate::test_cases::{parse_test_case, discover_test_cases, ParseError};
    use std::path::PathBuf;

    #[test]
    fn test_discover_test_cases() {
        let mut tests = discover_test_cases("examples/*");
        tests.sort_by_key(|tc| tc.name.clone());
        assert_eq!(tests.len(), 1);

        // Test cases in lexicographical order
        assert_eq!(tests[0].name, "Hello, Test!");
    }

    #[test]
    fn hello_world() {
        match parse_test_case(&PathBuf::from("examples/rehearse.json")) {
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
    fn bad_format_1() {
        match parse_test_case(&PathBuf::from("examples/failing-tests/bad_format_1.json")) {
            Ok(_) => assert!(false, "Test case with invalid format should not parse."),
            Err(e) => match e {
                ParseError::BadContent(msg) => assert_eq!(msg, "File does_not_exist not found"),
                _ => assert!(false, "Got the wrong error")
            }
        }
    }

    #[test]
    fn bad_format_2() {
        match parse_test_case(&PathBuf::from("examples/failing-tests/bad_format_2.json")) {
            Ok(_) => assert!(false, "Test case with invalid format should not parse."),
            Err(e) => match e {
                ParseError::BadContent(msg) => assert_eq!(msg, "Field \"artifacts\" must be an array of objects"),
                _ => assert!(false, "Got the wrong error")
            }
        }
    }

    #[test]
    fn bad_format_3() {
        match parse_test_case(&PathBuf::from("examples/failing-tests/bad_format_3.json")) {
            Ok(_) => assert!(false, "Test case with invalid format should not parse."),
            Err(e) => match e {
                ParseError::BadContent(msg) => assert_eq!(msg, "Missing required field \"version\""),
                _ => assert!(false, "Got the wrong error")
            }
        }
    }

    #[test]
    fn bad_format_4() {
        match parse_test_case(&PathBuf::from("examples/failing-tests/bad_format_4.json")) {
            Ok(_) => assert!(false, "Test case with invalid format should not parse."),
            Err(e) => match e {
                ParseError::JSONError(_) => {},
                _ => assert!(false, "Got the wrong error")
            }
        }
    }
}