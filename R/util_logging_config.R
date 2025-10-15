#' Setup logging for clifR
#'
#' @description
#' Configure logging level and output format for clifR operations.
#'
#' @param level Character. Logging level: "info", "warning", "error", or "debug".
#' @param show_timestamps Logical. Whether to show timestamps in log messages.
#'
#' @return Invisible NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' setup_logging(level = "info")
#' setup_logging(level = "debug", show_timestamps = TRUE)
#' }
setup_logging <- function(level = c("info", "warning", "error", "debug"),
                          show_timestamps = FALSE) {
  level <- match.arg(level)

  # Store logging configuration in package environment
  pkg_env <- new.env(parent = emptyenv())
  pkg_env$log_level <- level
  pkg_env$show_timestamps <- show_timestamps

  # Attach to clifR namespace
  assign("logging_config", pkg_env, envir = parent.env(environment()))

  cli::cli_alert_success("Logging configured: level = {.val {level}}")

  invisible(NULL)
}

#' Log message with appropriate level
#'
#' @description
#' Internal logging function that respects configured log level.
#'
#' @param message Character. Message to log.
#' @param level Character. Message level: "info", "warning", "error", or "debug".
#'
#' @return Invisible NULL.
#'
#' @keywords internal
log_message <- function(message, level = "info") {
  # Get current logging configuration
  config <- tryCatch(
    get("logging_config", envir = parent.env(environment())),
    error = function(e) list(log_level = "info", show_timestamps = FALSE)
  )

  level_priority <- c(error = 1, warning = 2, info = 3, debug = 4)
  current_priority <- level_priority[[config$log_level]]
  message_priority <- level_priority[[level]]

  # Only log if message priority is at or above current level
  if (message_priority <= current_priority) {
    timestamp <- if (config$show_timestamps) {
      paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ")
    } else {
      ""
    }

    switch(level,
      error = cli::cli_alert_danger("{timestamp}{message}"),
      warning = cli::cli_alert_warning("{timestamp}{message}"),
      info = cli::cli_alert_info("{timestamp}{message}"),
      debug = cli::cli_text("{timestamp}[DEBUG] {message}")
    )
  }

  invisible(NULL)
}

#' Create validation report header
#'
#' @description
#' Create formatted header for validation reports.
#'
#' @param table_name Character. Name of table being validated.
#' @param n_rows Integer. Number of rows in table.
#'
#' @return Invisible NULL.
#'
#' @keywords internal
log_validation_header <- function(table_name, n_rows) {
  cli::cli_h1("Validating {table_name} table")
  cli::cli_text("Total rows: {.val {n_rows}}")
  cli::cli_text("Timestamp: {.val {Sys.time()}}")
  cli::cli_rule()
  invisible(NULL)
}

#' Log validation results summary
#'
#' @description
#' Create formatted summary of validation results.
#'
#' @param errors List. Validation errors by type.
#' @param warnings List. Validation warnings by type.
#'
#' @return Invisible NULL.
#'
#' @keywords internal
log_validation_summary <- function(errors = list(), warnings = list()) {
  cli::cli_rule("Validation Summary")

  n_errors <- length(unlist(errors))
  n_warnings <- length(unlist(warnings))

  if (n_errors == 0 && n_warnings == 0) {
    cli::cli_alert_success("All validation checks passed!")
  } else {
    if (n_errors > 0) {
      cli::cli_alert_danger("Found {.val {n_errors}} error(s)")
      for (error_type in names(errors)) {
        cli::cli_text("  {.field {error_type}}: {length(errors[[error_type]])} error(s)")
      }
    }

    if (n_warnings > 0) {
      cli::cli_alert_warning("Found {.val {n_warnings}} warning(s)")
      for (warning_type in names(warnings)) {
        cli::cli_text("  {.field {warning_type}}: {length(warnings[[warning_type]])} warning(s)")
      }
    }
  }

  cli::cli_rule()
  invisible(NULL)
}

#' Progress bar for long-running operations
#'
#' @description
#' Create a progress bar for iterative operations.
#'
#' @param total Integer. Total number of iterations.
#' @param format Character. Progress bar format string.
#'
#' @return progress bar object from cli package.
#'
#' @keywords internal
create_progress_bar <- function(total, format = "Processing {cli::pb_bar} {cli::pb_percent}") {
  cli::cli_progress_bar(
    total = total,
    format = format,
    clear = FALSE
  )
}
