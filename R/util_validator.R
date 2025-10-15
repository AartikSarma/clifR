#' Validate CLIF table against schema
#'
#' @description
#' Comprehensive validation of CLIF table data against YAML schema definition.
#'
#' @param data tibble or data.frame. Data to validate.
#' @param schema List. Schema definition (from load_schema()).
#' @param table_name Character. Name of table for reporting.
#'
#' @return List containing validation results with errors and warnings.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' patient_schema <- load_schema("patient")
#' results <- validate_table(patient_data, patient_schema, "patient")
#' }
validate_table <- function(data, schema, table_name = "table") {

  log_validation_header(table_name, nrow(data))

  validation_results <- list(
    table_name = table_name,
    n_rows = nrow(data),
    n_cols = ncol(data),
    errors = list(),
    warnings = list(),
    is_valid = TRUE,
    timestamp = Sys.time()
  )

  # 1. Check required columns
  required_results <- check_required_columns(data, schema)
  validation_results$errors$missing_columns <- required_results$missing

  # 2. Check column data types
  type_results <- check_data_types(data, schema)
  validation_results$errors$type_errors <- type_results$errors
  validation_results$warnings$type_warnings <- type_results$warnings

  # 3. Check categorical values
  category_results <- check_categorical_values(data, schema)
  validation_results$errors$invalid_categories <- category_results$errors

  # 4. Check for duplicates on composite keys
  if ("composite_keys" %in% names(schema)) {
    duplicate_results <- check_duplicates(data, schema$composite_keys)
    validation_results$warnings$duplicates <- duplicate_results$duplicates
  }

  # 5. Analyze missing data
  missing_results <- analyze_missing_data(data, schema)
  validation_results$warnings$missing_data <- missing_results

  # 6. Check numeric ranges (if defined in schema)
  if ("vital_ranges" %in% names(schema)) {
    range_results <- check_numeric_ranges(data, schema)
    validation_results$warnings$out_of_range <- range_results$out_of_range
  }

  # 7. Check timezone for datetime columns
  timezone_results <- check_timezones(data, schema)
  validation_results$warnings$timezone_issues <- timezone_results$issues

  # Determine overall validity
  validation_results$is_valid <- length(unlist(validation_results$errors)) == 0

  # Log summary
  log_validation_summary(
    validation_results$errors,
    validation_results$warnings
  )

  return(validation_results)
}

#' Check required columns
#'
#' @keywords internal
check_required_columns <- function(data, schema) {
  required_cols <- purrr::map_lgl(schema$columns, ~.x$required)
  required_col_names <- purrr::map_chr(schema$columns[required_cols], ~.x$name)

  missing_cols <- setdiff(required_col_names, names(data))

  if (length(missing_cols) > 0) {
    cli::cli_alert_danger("Missing required columns: {.field {missing_cols}}")
  } else {
    cli::cli_alert_success("All required columns present")
  }

  return(list(missing = missing_cols))
}

#' Check data types
#'
#' @keywords internal
check_data_types <- function(data, schema) {
  errors <- list()
  warnings <- list()

  for (col_def in schema$columns) {
    col_name <- col_def$name
    expected_type <- col_def$data_type

    if (!(col_name %in% names(data))) {
      next  # Skip if column not present (handled by required check)
    }

    # Map CLIF types to R types
    actual_type <- get_r_type(data[[col_name]])
    expected_r_type <- map_clif_to_r_type(expected_type)

    if (!is_compatible_type(actual_type, expected_r_type)) {
      if (is_castable(actual_type, expected_r_type)) {
        warnings[[col_name]] <- glue::glue(
          "Column {col_name}: type {actual_type} can be cast to {expected_type}"
        )
      } else {
        errors[[col_name]] <- glue::glue(
          "Column {col_name}: expected {expected_type}, got {actual_type}"
        )
      }
    }
  }

  if (length(errors) > 0) {
    cli::cli_alert_danger("Found {length(errors)} type error(s)")
  }
  if (length(warnings) > 0) {
    cli::cli_alert_warning("Found {length(warnings)} type warning(s)")
  }
  if (length(errors) == 0 && length(warnings) == 0) {
    cli::cli_alert_success("All column types valid")
  }

  return(list(errors = errors, warnings = warnings))
}

#' Check categorical values
#'
#' @keywords internal
check_categorical_values <- function(data, schema) {
  errors <- list()

  # Find category columns with permissible values
  category_cols <- purrr::keep(schema$columns, ~.x$is_category_column)

  for (col_def in category_cols) {
    col_name <- col_def$name

    if (!(col_name %in% names(data))) {
      next
    }

    if ("permissible_values" %in% names(col_def)) {
      permitted <- col_def$permissible_values
      actual_values <- unique(data[[col_name]][!is.na(data[[col_name]])])
      invalid_values <- setdiff(actual_values, permitted)

      if (length(invalid_values) > 0) {
        errors[[col_name]] <- list(
          invalid_values = invalid_values,
          n_rows = sum(data[[col_name]] %in% invalid_values, na.rm = TRUE)
        )

        cli::cli_alert_danger(
          "Column {.field {col_name}}: {length(invalid_values)} invalid value(s)"
        )
      }
    }
  }

  if (length(errors) == 0) {
    cli::cli_alert_success("All categorical values valid")
  }

  return(list(errors = errors))
}

#' Check for duplicate records
#'
#' @keywords internal
check_duplicates <- function(data, key_columns) {
  if (length(key_columns) == 0) {
    return(list(duplicates = NULL))
  }

  # Check if key columns exist
  missing_keys <- setdiff(key_columns, names(data))
  if (length(missing_keys) > 0) {
    cli::cli_alert_warning(
      "Cannot check duplicates: missing key columns {.field {missing_keys}}"
    )
    return(list(duplicates = NULL))
  }

  # Find duplicates
  duplicated_rows <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_columns))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  if (nrow(duplicated_rows) > 0) {
    cli::cli_alert_warning(
      "Found {nrow(duplicated_rows)} duplicate row(s) on key {.field {key_columns}}"
    )
    return(list(duplicates = duplicated_rows))
  } else {
    cli::cli_alert_success("No duplicate records found")
    return(list(duplicates = NULL))
  }
}

#' Analyze missing data
#'
#' @keywords internal
analyze_missing_data <- function(data, schema) {
  missing_summary <- data %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(is.na(.)))) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "column",
      values_to = "n_missing"
    ) %>%
    dplyr::mutate(
      pct_missing = n_missing / nrow(data) * 100
    ) %>%
    dplyr::filter(n_missing > 0) %>%
    dplyr::arrange(dplyr::desc(pct_missing))

  if (nrow(missing_summary) > 0) {
    cli::cli_alert_info("Missing data summary:")
    for (i in seq_len(min(5, nrow(missing_summary)))) {
      cli::cli_text(
        "  {.field {missing_summary$column[i]}}: {missing_summary$n_missing[i]} ({round(missing_summary$pct_missing[i], 1)}%)"
      )
    }
    if (nrow(missing_summary) > 5) {
      cli::cli_text("  ... and {nrow(missing_summary) - 5} more column(s)")
    }
  } else {
    cli::cli_alert_success("No missing data")
  }

  return(missing_summary)
}

#' Check numeric ranges
#'
#' @keywords internal
check_numeric_ranges <- function(data, schema) {
  out_of_range <- list()

  if (!"vital_category" %in% names(data) || !"vital_value" %in% names(data)) {
    return(list(out_of_range = out_of_range))
  }

  ranges <- schema$vital_ranges

  for (vital_type in names(ranges)) {
    range_def <- ranges[[vital_type]]

    rows_of_type <- data %>%
      dplyr::filter(vital_category == vital_type)

    if (nrow(rows_of_type) > 0) {
      min_val <- range_def$min
      max_val <- range_def$max

      out_of_range_rows <- rows_of_type %>%
        dplyr::filter(
          !is.na(vital_value),
          vital_value < min_val | vital_value > max_val
        )

      if (nrow(out_of_range_rows) > 0) {
        out_of_range[[vital_type]] <- list(
          n_rows = nrow(out_of_range_rows),
          expected_range = c(min_val, max_val),
          actual_range = range(out_of_range_rows$vital_value)
        )

        cli::cli_alert_warning(
          "{vital_type}: {nrow(out_of_range_rows)} value(s) outside [{min_val}, {max_val}]"
        )
      }
    }
  }

  return(list(out_of_range = out_of_range))
}

#' Check timezone consistency
#'
#' @keywords internal
check_timezones <- function(data, schema) {
  issues <- list()

  # Find datetime columns
  datetime_cols <- purrr::map_lgl(schema$columns, ~.x$data_type == "DATETIME")
  datetime_col_names <- purrr::map_chr(schema$columns[datetime_cols], ~.x$name)

  for (col_name in datetime_col_names) {
    if (col_name %in% names(data)) {
      if (lubridate::is.POSIXct(data[[col_name]])) {
        tz <- attr(data[[col_name]], "tzone")
        if (is.null(tz) || tz == "") {
          issues[[col_name]] <- "No timezone information"
        }
      } else {
        issues[[col_name]] <- "Not a datetime type"
      }
    }
  }

  if (length(issues) > 0) {
    cli::cli_alert_warning("Timezone issues in {length(issues)} column(s)")
  }

  return(list(issues = issues))
}

#' Map CLIF data type to R type
#'
#' @keywords internal
map_clif_to_r_type <- function(clif_type) {
  switch(clif_type,
    VARCHAR = "character",
    INT = "integer",
    DOUBLE = "numeric",
    DATETIME = "POSIXct",
    BOOLEAN = "logical",
    "unknown"
  )
}

#' Get R type name
#'
#' @keywords internal
get_r_type <- function(x) {
  if (is.character(x)) return("character")
  if (is.integer(x)) return("integer")
  if (is.numeric(x)) return("numeric")
  if (lubridate::is.POSIXct(x)) return("POSIXct")
  if (lubridate::is.Date(x)) return("Date")
  if (is.logical(x)) return("logical")
  return(class(x)[1])
}

#' Check if types are compatible
#'
#' @keywords internal
is_compatible_type <- function(actual, expected) {
  if (actual == expected) return(TRUE)

  # Numeric compatibility
  if (expected == "numeric" && actual %in% c("integer", "numeric")) return(TRUE)
  if (expected == "integer" && actual == "numeric") return(FALSE)  # Lossy

  # Date/time compatibility
  if (expected == "POSIXct" && actual %in% c("POSIXct", "POSIXlt", "Date")) return(TRUE)

  return(FALSE)
}

#' Check if type can be cast
#'
#' @keywords internal
is_castable <- function(actual, expected) {
  # Character can be cast to anything (with potential loss)
  if (actual == "character") return(TRUE)

  # Numeric to integer (lossy but possible)
  if (actual == "numeric" && expected == "integer") return(TRUE)

  # Date to POSIXct
  if (actual == "Date" && expected == "POSIXct") return(TRUE)

  return(FALSE)
}

#' Generate validation report
#'
#' @description
#' Create a detailed markdown validation report.
#'
#' @param validation_results List. Results from validate_table().
#' @param output_file Character. Path to output markdown file.
#'
#' @return Invisible NULL.
#'
#' @export
generate_validation_report <- function(validation_results, output_file) {

  report_lines <- c(
    glue::glue("# Validation Report: {validation_results$table_name}"),
    "",
    glue::glue("**Generated:** {validation_results$timestamp}"),
    glue::glue("**Rows:** {validation_results$n_rows}"),
    glue::glue("**Columns:** {validation_results$n_cols}"),
    glue::glue("**Valid:** {validation_results$is_valid}"),
    "",
    "## Summary",
    ""
  )

  # Errors section
  n_errors <- length(unlist(validation_results$errors))
  if (n_errors > 0) {
    report_lines <- c(
      report_lines,
      glue::glue("### Errors ({n_errors})"),
      ""
    )

    for (error_type in names(validation_results$errors)) {
      if (length(validation_results$errors[[error_type]]) > 0) {
        report_lines <- c(
          report_lines,
          glue::glue("#### {error_type}"),
          "",
          paste("-", validation_results$errors[[error_type]]),
          ""
        )
      }
    }
  } else {
    report_lines <- c(report_lines, "No errors found.", "")
  }

  # Warnings section
  n_warnings <- length(unlist(validation_results$warnings))
  if (n_warnings > 0) {
    report_lines <- c(
      report_lines,
      glue::glue("### Warnings ({n_warnings})"),
      ""
    )

    for (warning_type in names(validation_results$warnings)) {
      if (length(validation_results$warnings[[warning_type]]) > 0) {
        report_lines <- c(
          report_lines,
          glue::glue("#### {warning_type}"),
          ""
        )
      }
    }
  }

  # Write report
  writeLines(report_lines, output_file)
  cli::cli_alert_success("Validation report written to {.file {output_file}}")

  invisible(NULL)
}
