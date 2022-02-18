mod priority_queue;
mod test_cases;
mod cli;
mod artifacts;

use cli::{Terminal, ColorTerminal, TermColorMode, TermColor, ArgTemplate, Arg, ArgError, ArgResult};
use test_cases::get_default_test_options;
use test_cases::{discover_test_cases, process_test_cases};

use log::{trace, error, LevelFilter};
use simplelog::{Config, ConfigBuilder, CombinedLogger, TermLogger, WriteLogger, ColorChoice, TerminalMode};
use atty::Stream;
use tokio::runtime::Builder;

use std::process::{Command, exit};
use std::path::PathBuf;
use std::str::FromStr;

const ENV_LOG_LEVEL: &'static str = "REHEARSE_LOG_LEVEL";
const ENV_LOG_FILE: &'static str = "REHEARSE_LOG_FILE";
const ENV_COLOR_MODE: &'static str = "REHEARSE_COLOR_MODE";

const COLOR_MODE_AUTO: &'static str = "auto";
const COLOR_MODE_NONE: &'static str = "none";
const COLOR_MODE_8_COLORS: &'static str = "8";
const COLOR_MODE_256_COLORS: &'static str = "256";

const DEFAULT_LOG_FILE: &'static str = "rehearse.log";

const ARG_HELP: &'static str = "--help";
const ARG_COLOR_MODE: &'static str = "--color-mode";
const ARG_LOG_LEVEL: &'static str = "--log-level";
const ARG_LOG_FILE: &'static str = "--log-file";
const ARG_OUTPUT: &'static str = "--output";
const ARG_PATCH: &'static str = "--patch";
const ARG_PROMOTE: &'static str = "--promote";
const ARG_REVERT: &'static str = "--revert";
const ARG_VERSION: &'static str = "--version";

// Error codes: <16 bits additional data> <16 bits error code>
const EXIT_CODE_OK: i32 = 0x0000_0000;
const EXIT_CODE_NO_FILES: i32 = 0x0000_0001;
const EXIT_CODE_BAD_ARGS: i32 = 0x0000_0002;
const EXIT_CODE_NO_RUNTIME: i32 = 0x0000_0003;

fn write_version<T: Terminal>(terminal: &mut T) -> std::io::Result<()> {
    trace!("Printing version...");
    terminal.write(format!("\
rehearse - Expectation testing tool
version {}
~ by Delphi1024
", env!("CARGO_PKG_VERSION")))?;
    terminal.flush()?;
    return Ok(());
}

fn get_default_color_mode() -> TermColorMode {
    let is_tty = atty::is(Stream::Stdout);
    if !is_tty {
        // Not a terminal, do not print colors
        return TermColorMode::None;
    }

    // Check terminal color capabilities by running tput
    let tput_output = Command::new("tput")
        .arg("colors")
        .output();
    
    // Parse output from tput
    let available_colors = if let Ok(o) = tput_output {
        if let Ok(s) = String::from_utf8(o.stdout) {
            usize::from_str_radix(&s.trim(), 10).map_or(8, |n| n)
        } else {
            8   // Output was somehow not a string
        }
    } else {
        8   // Command failed
    };
    
    // Determine color mode
    let auto_color_mode = if available_colors >= 256 {
        TermColorMode::C256
    } else if available_colors >= 8 {
        TermColorMode::C8
    } else {
        TermColorMode::None
    };

    // Overwrite with the environment variable
    return match std::env::var(ENV_COLOR_MODE).as_ref().map(|v| &v[..]) {
        Ok(COLOR_MODE_AUTO) => auto_color_mode,
        Ok(COLOR_MODE_NONE) => TermColorMode::None,
        Ok(COLOR_MODE_8_COLORS) => TermColorMode::C8,
        Ok(COLOR_MODE_256_COLORS) => TermColorMode::C256,
        _ => auto_color_mode
    };
}

fn setup_logging(arg_result: &ArgResult, color_mode: TermColorMode) -> Result<(), Box<dyn std::error::Error>> {
    // Determine logging level
    let log_level = if let Some(Ok(level)) = arg_result.get_argument(ARG_LOG_LEVEL).map(|s| LevelFilter::from_str(s)) {
        level
    } else if let Ok(Ok(level)) = std::env::var(ENV_LOG_LEVEL).map(|s| LevelFilter::from_str(&s)) {
        level
    } else {
        LevelFilter::Info
    };

    // Determine log file
    let target = if let Some(str) = arg_result.get_argument(ARG_LOG_FILE) {
        PathBuf::from(str)
    } else if let Ok(str) = std::env::var(ENV_LOG_FILE) {
        PathBuf::from(str)
    } else {
        PathBuf::from(DEFAULT_LOG_FILE)
    };
    let file = std::fs::File::create(target)?;

    // Build loggers
    let file_logger = WriteLogger::new(log_level, Config::default(), file);
    let term_logger = TermLogger::new(
        log_level,
        ConfigBuilder::new()
            .set_time_level(LevelFilter::Off)
            .set_target_level(LevelFilter::Off)
            .set_location_level(LevelFilter::Off)
            .build(),
        TerminalMode::Mixed,
        match color_mode {
            TermColorMode::None => ColorChoice::Never,
            _ => ColorChoice::Always
        }
    );

    // Build CombinedLogger
    CombinedLogger::init(vec![
        file_logger,
        term_logger
    ])?;

    return Ok(());
}

fn main() {
    let default_color_mode = get_default_color_mode();

    // Initial terminal which is basically only used to print the usage
    // if argument parsing fails. Uses 'auto' color mode.
    let mut initial_terminal = ColorTerminal::new(default_color_mode.clone());
    
    let arg_template = ArgTemplate::new()
        .set_usage_header("rehearse - Expectation testing tool")
        .add_argument(Arg::new(ARG_HELP, "Prints this help text").short("-h"))
        .add_argument(Arg::new(ARG_COLOR_MODE, "Selects how color is used in the output:\n- auto: auto-detect color capabilities (default)\n- none: Do not use color\n- 8:    8-color palette\n- 256:  256-color palette").short("-cm").parameter("<mode>"))
        .add_argument(Arg::new(ARG_LOG_LEVEL, "Selects the logging level. Valid values are \"error\", \"warn\", \"info\", \"debug\" and \"trace\"").short("-ll").parameter("<level>"))
        .add_argument(Arg::new(ARG_LOG_FILE, &format!("The name of the log file. Default is {}", DEFAULT_LOG_FILE)).short("-lf").parameter("<path>"))
        .add_argument(Arg::new(ARG_OUTPUT, "Also output the diff to a file").short("-o").parameter("<path>"))
        .add_argument(Arg::new(ARG_PATCH, "Applies a test case diff to the test case reference").parameter("<path-to-diff>"))
        .add_argument(Arg::new(ARG_PROMOTE, "Runs a test case and unconditionally applies the diff to the reference"))
        .add_argument(Arg::new(ARG_REVERT, "Reverts a previously patched reference").short("-r").parameter("<path-to-diff>"))
        .add_argument(Arg::new(ARG_VERSION, "Prints the program version and exits"))
        .set_usage_trailer("written in 2022 by Delphi1024");

    let mut args = std::env::args().peekable();
    let program_name = if let Some(name) = args.peek() { name } else { "rehearse" };
    let arg_result = match arg_template.parse_args(std::env::args()) {
        Ok(res) => res,
        Err(ArgError(msg)) => {
            arg_template
                .set_usage_header(&msg)
                .write_usage(&mut initial_terminal, program_name).expect("Stdout is broken!!");
            exit(EXIT_CODE_BAD_ARGS);
        }
    };

    // Get color mode
    let color_mode = match arg_result.get_argument(ARG_COLOR_MODE) {
        Some(COLOR_MODE_256_COLORS) => TermColorMode::C256,
        Some(COLOR_MODE_8_COLORS) => TermColorMode::C8,
        Some(COLOR_MODE_NONE) => TermColorMode::None,
        Some(COLOR_MODE_AUTO) | None => default_color_mode,
        Some(mode) => {
            arg_template
                .set_usage_header(&format!("Unrecognized color mode: {}", mode))
                .write_usage(&mut initial_terminal, program_name).expect("Stdout is broken!!");
            exit(EXIT_CODE_BAD_ARGS);
        }
    };

    // Create application terminal
    let mut terminal = ColorTerminal::new(color_mode);

    // Setup logging
    if let Err(e) = setup_logging(&arg_result, color_mode) {
        terminal.set_color_fg(&TermColor::Error).expect("Stdout is broken!!");
        terminal.write(format!("Error setting up logging: {}\n", e)).expect("Stdout is broken!!");
        terminal.reset_color().expect("Stdout is broken!!");
        terminal.flush().expect("Stdout is broken!!");
    }

    // Help explicitly requested
    if let Some(_) = arg_result.get_argument(ARG_HELP) {
        trace!("Help requested");
        arg_template.write_usage(&mut terminal, program_name).expect("Stdout is broken!!");
        exit(EXIT_CODE_OK);
    }

    // Print version
    if let Some(_) = arg_result.get_argument(ARG_VERSION) {
        trace!("Version requested");
        write_version(&mut terminal).expect("Stdout is broken!!");
        exit(EXIT_CODE_OK);
    }

    // No files specified
    if arg_result.get_rest().len() == 0 {
        error!("No input files");
        arg_template.write_usage(&mut terminal, program_name).expect("Stdout is broken!!");
        exit(EXIT_CODE_NO_FILES);
    }

    // Initialize tokio runtime
    let tokio_runtime_res = Builder::new_current_thread()
        .enable_io()
        .enable_time()
        .build();
    let tokio_runtime = match tokio_runtime_res {
        Ok(rt) => rt,
        Err(e) => {
            error!("Failed to initialize runtime: {}", e);
            exit(EXIT_CODE_NO_RUNTIME);
        }
    };

    // Parse test cases
    let default_test_options = get_default_test_options();
    let test_cases = arg_result.get_rest().iter()
        .map(|arg| discover_test_cases(arg, &default_test_options))
        .flatten()
        .collect::<Vec<_>>();
    
    // Run Tests!
    tokio_runtime.block_on(process_test_cases(test_cases, terminal.clone()));
}
