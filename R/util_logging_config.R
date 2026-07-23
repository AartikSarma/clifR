#' Centralized logging configuration
#'
#' Port of `clifpy/utils/logging_config.py`. Provides a unified logging setup
#' that writes:
#'
#' * a main log file capturing everything at the configured level and above,
#' * an error log file capturing warnings and errors only,
#' * console output, so the user-facing experience is unchanged.
#'
#' Log files live in `<output_directory>/logs/`. clifpy names them
#' `clifpy_all.log` and `clifpy_errors.log`; clifR uses `clifr_all.log` and
#' `clifr_errors.log` so the two implementations can share an output directory
#' without interleaving.
#'
#' The implementation is deliberately dependency-free beyond `cli`: R has no
#' equivalent of Python's `logging` hierarchy, so a small package-level state
#' object plus a logger closure covers the same ground.
#'
#' @name clif-logging
NULL

# Numeric severities, matching Python's logging module.
LOG_LEVEL_VALUES <- c(debug = 10L, info = 20L, warning = 30L, error = 40L, critical = 50L)

# Emoji indicators, matching clifpy's EMOJI_MAP.
LOG_LEVEL_EMOJI <- c(
  debug = "\U0001F41B",
  info = "\U0001F4E2",
  warning = "⚠️",
  error = "❌",
  critical = "\U0001F198"
)

# Package-level logging state, replacing Python's module-level logger registry.
logging_state <- new.env(parent = emptyenv())
logging_state$is_configured <- FALSE
logging_state$level <- "info"
logging_state$console_output <- TRUE
logging_state$separate_error_log <- TRUE
logging_state$log_directory <- NULL
logging_state$all_log_file <- NULL
logging_state$error_log_file <- NULL

#' Normalise a log level to its canonical name
#'
#' Accepts a level name in any case, or a numeric severity as used by Python's
#' `logging` module.
#'
#' @param level A level name such as `"info"`, or a numeric severity such as `20`.
#' @return One of `"debug"`, `"info"`, `"warning"`, `"error"`, `"critical"`.
#' @keywords internal
normalize_log_level <- function(level) {
  if (is.numeric(level)) {
    matching_names <- names(LOG_LEVEL_VALUES)[LOG_LEVEL_VALUES <= level]
    if (length(matching_names) == 0) {
      return("debug")
    }
    return(matching_names[length(matching_names)])
  }
  level_name <- tolower(as.character(level))
  if (!level_name %in% names(LOG_LEVEL_VALUES)) {
    cli::cli_abort(
      "Unknown log level {.val {level}}. Use one of {.val {names(LOG_LEVEL_VALUES)}}."
    )
  }
  level_name
}

#' Configure logging for clifR
#'
#' Creates `<output_directory>/logs/` and directs log records to `clifr_all.log`,
#' `clifr_errors.log` and the console. Calling this again reconfigures logging
#' with the new parameters, so it is safe to call repeatedly — for example when
#' an orchestrator changes its output directory.
#'
#' @param output_directory Base output directory. Logs are written to
#'   `<output_directory>/logs/`. When `NULL`, uses `<working directory>/output`.
#' @param level Minimum severity to record: `"debug"`, `"info"`, `"warning"`,
#'   `"error"` or `"critical"`. A numeric Python-style severity is also accepted.
#' @param console_output Whether to also print records to the console.
#' @param separate_error_log Whether to write a second file containing warnings
#'   and errors only.
#'
#' @return The root clifR logger, invisibly. See [get_logger()].
#' @export
#'
#' @examples
#' \dontrun{
#' logger <- setup_logging(output_directory = "./output")
#' logger$info("Loading data...")
#' }
setup_logging <- function(output_directory = NULL,
                          level = "info",
                          console_output = TRUE,
                          separate_error_log = TRUE) {
  if (is.null(output_directory)) {
    output_directory <- file.path(getwd(), "output")
  }

  log_directory <- file.path(output_directory, "logs")
  if (!dir.exists(log_directory)) {
    dir.create(log_directory, recursive = TRUE, showWarnings = FALSE)
  }

  logging_state$level <- normalize_log_level(level)
  logging_state$console_output <- isTRUE(console_output)
  logging_state$separate_error_log <- isTRUE(separate_error_log)
  logging_state$log_directory <- log_directory
  logging_state$all_log_file <- file.path(log_directory, "clifr_all.log")
  logging_state$error_log_file <- if (isTRUE(separate_error_log)) {
    file.path(log_directory, "clifr_errors.log")
  } else {
    NULL
  }
  logging_state$is_configured <- TRUE

  root_logger <- get_logger("clifR")
  root_logger$debug(sprintf("Logging initialized - logs directory: %s", log_directory))
  invisible(root_logger)
}

#' Write one log record
#'
#' Applies the configured level filter, then appends to the log files and prints
#' to the console. When [setup_logging()] has not been called, nothing is written
#' to disk and only warnings and errors reach the console, so library code can
#' log unconditionally without spamming an unconfigured session.
#'
#' @param logger_name Name of the logger emitting the record.
#' @param level Canonical level name.
#' @param message Message text.
#' @return `NULL`, invisibly.
#' @keywords internal
emit_log_record <- function(logger_name, level, message) {
  level_name <- normalize_log_level(level)
  level_value <- LOG_LEVEL_VALUES[[level_name]]
  threshold_value <- LOG_LEVEL_VALUES[[logging_state$level]]
  if (level_value < threshold_value) {
    return(invisible(NULL))
  }

  emoji <- LOG_LEVEL_EMOJI[[level_name]]

  if (logging_state$is_configured) {
    file_record <- sprintf(
      "%s | %s %-8s | %s | %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      emoji, toupper(level_name), logger_name, message
    )
    cat(file_record, "\n", sep = "", file = logging_state$all_log_file, append = TRUE)
    if (!is.null(logging_state$error_log_file) && level_value >= LOG_LEVEL_VALUES[["warning"]]) {
      cat(file_record, "\n", sep = "", file = logging_state$error_log_file, append = TRUE)
    }
  }

  # Before setup_logging() runs, only warnings and errors reach the console. This
  # matches Python's last-resort handler, which is what clifpy's INFO-level
  # progress messages hit when the caller has not configured logging.
  show_on_console <- if (logging_state$is_configured) {
    logging_state$console_output
  } else {
    level_value >= LOG_LEVEL_VALUES[["warning"]]
  }

  if (show_on_console) {
    # clifpy's console format is exactly "<emoji> <message>"; the emoji already
    # carries the severity, so no cli bullet symbol is added on top of it.
    console_text <- paste(emoji, message)
    cli::cli_text("{console_text}")
  }

  invisible(NULL)
}

#' Get a logger for a clifR module
#'
#' Returns a logger whose name lives in the `clifR.*` namespace, mirroring
#' clifpy's `get_logger`. The result is a list of message functions, one per
#' severity, that route through the configuration set by [setup_logging()].
#'
#' @param name Module name. Prefixed with `clifR.` unless it already begins with
#'   `clifR`.
#'
#' @return A named list with a `name` element and `debug`, `info`, `warning`,
#'   `error` and `critical` functions, each taking a message string.
#' @export
#'
#' @examples
#' logger <- get_logger("tables.patient")
#' logger$name
get_logger <- function(name) {
  logger_name <- if (startsWith(name, "clifR")) name else paste0("clifR.", name)

  make_level_function <- function(level_name) {
    force(level_name)
    function(message) emit_log_record(logger_name, level_name, message)
  }

  logger <- list(name = logger_name)
  for (level_name in names(LOG_LEVEL_VALUES)) {
    logger[[level_name]] <- make_level_function(level_name)
  }
  logger
}

#' Log a message at a given level
#'
#' Convenience wrapper used by clifR internals that do not hold onto a logger
#' object.
#'
#' @param message Message text.
#' @param level Level name: `"debug"`, `"info"`, `"warning"`, `"error"` or
#'   `"critical"`.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
log_message <- function(message, level = "info") {
  emit_log_record("clifR", level, message)
}

#' Print a validation report header
#'
#' @param table_name Name of the table being validated.
#' @param n_rows Number of rows in the table.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
log_validation_header <- function(table_name, n_rows) {
  cli::cli_h1("Validating {table_name} table")
  cli::cli_text("Total rows: {.val {n_rows}}")
  cli::cli_text("Timestamp: {.val {Sys.time()}}")
  cli::cli_rule()
  invisible(NULL)
}

#' Print a validation results summary
#'
#' @param errors Named list of validation errors, grouped by type.
#' @param warnings Named list of validation warnings, grouped by type.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
log_validation_summary <- function(errors = list(), warnings = list()) {
  cli::cli_rule("Validation Summary")

  error_count <- length(unlist(errors))
  warning_count <- length(unlist(warnings))

  if (error_count == 0 && warning_count == 0) {
    cli::cli_alert_success("All validation checks passed!")
  } else {
    if (error_count > 0) {
      cli::cli_alert_danger("Found {.val {error_count}} error(s)")
      for (error_type in names(errors)) {
        cli::cli_text("  {.field {error_type}}: {length(errors[[error_type]])} error(s)")
      }
    }
    if (warning_count > 0) {
      cli::cli_alert_warning("Found {.val {warning_count}} warning(s)")
      for (warning_type in names(warnings)) {
        cli::cli_text("  {.field {warning_type}}: {length(warnings[[warning_type]])} warning(s)")
      }
    }
  }

  cli::cli_rule()
  invisible(NULL)
}

#' Create a progress bar for a long-running operation
#'
#' @param total Total number of iterations.
#' @param format Progress bar format string.
#'
#' @return A cli progress bar identifier.
#' @keywords internal
create_progress_bar <- function(total, format = "Processing {cli::pb_bar} {cli::pb_percent}") {
  cli::cli_progress_bar(total = total, format = format, clear = FALSE)
}
