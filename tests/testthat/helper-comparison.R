#' Cross-Language Validation Helpers
#'
#' Helper functions for comparing R outputs to Python baselines.

#' Compare numeric values with tolerance
#'
#' @param r_values Numeric vector from R
#' @param python_values Numeric vector from Python
#' @param tolerance Numeric tolerance for comparison (default: 1e-12)
#' @param label Character label for reporting
#'
#' @return List with comparison results
compare_numeric_values <- function(r_values, python_values,
                                    tolerance = 1e-12,
                                    label = "values") {

  # Check lengths match
  if (length(r_values) != length(python_values)) {
    stop(sprintf(
      "%s: Length mismatch (R: %d, Python: %d)",
      label, length(r_values), length(python_values)
    ))
  }

  # Handle NA values
  r_na <- is.na(r_values)
  python_na <- is.na(python_values)

  if (!identical(r_na, python_na)) {
    warning(sprintf(
      "%s: NA pattern mismatch (R: %d NAs, Python: %d NAs)",
      label, sum(r_na), sum(python_na)
    ))
  }

  # Compare non-NA values
  valid_idx <- !r_na & !python_na
  if (sum(valid_idx) == 0) {
    return(list(
      match = TRUE,
      max_diff = 0,
      mean_diff = 0,
      n_exceeds_tolerance = 0
    ))
  }

  diffs <- abs(r_values[valid_idx] - python_values[valid_idx])
  max_diff <- max(diffs)
  mean_diff <- mean(diffs)
  n_exceeds <- sum(diffs > tolerance)

  result <- list(
    match = n_exceeds == 0,
    max_diff = max_diff,
    mean_diff = mean_diff,
    n_exceeds_tolerance = n_exceeds,
    correlation = cor(r_values[valid_idx], python_values[valid_idx])
  )

  # Report
  if (result$match) {
    message(sprintf(
      "✓ %s match (max diff: %.2e)",
      label, max_diff
    ))
  } else {
    warning(sprintf(
      "✗ %s mismatch: %d/%d values exceed tolerance %.2e (max diff: %.2e)",
      label, n_exceeds, sum(valid_idx), tolerance, max_diff
    ))
  }

  return(result)
}

#' Compare validation results
#'
#' @param r_validation List from R validate_table()
#' @param python_validation_file Path to Python validation JSON
#'
#' @return List with comparison results
compare_validation_results <- function(r_validation, python_validation_file) {

  if (!file.exists(python_validation_file)) {
    stop(sprintf("Python baseline not found: %s", python_validation_file))
  }

  python_validation <- jsonlite::read_json(python_validation_file)

  results <- list()

  # Compare basic counts
  results$n_rows <- r_validation$n_rows == python_validation$n_rows
  results$n_cols <- r_validation$n_cols == python_validation$n_cols
  results$is_valid <- r_validation$is_valid == python_validation$is_valid

  # Compare error counts
  r_n_errors <- length(unlist(r_validation$errors))
  python_n_errors <- length(unlist(python_validation$errors))
  results$error_count_match <- r_n_errors == python_n_errors

  # Compare warning counts
  r_n_warnings <- length(unlist(r_validation$warnings))
  python_n_warnings <- length(unlist(python_validation$warnings))
  results$warning_count_match <- r_n_warnings == python_n_warnings

  # Summary
  all_match <- all(unlist(results))

  if (all_match) {
    message(sprintf(
      "✓ Validation results match for %s",
      r_validation$table_name
    ))
  } else {
    warning(sprintf(
      "✗ Validation results differ for %s",
      r_validation$table_name
    ))
    print(results)
  }

  return(results)
}

#' Compare summary statistics
#'
#' @param r_summary Named list of summary statistics from R
#' @param python_summary_file Path to Python summary JSON
#' @param tolerance Numeric tolerance (default: 1e-12)
#'
#' @return List with comparison results
compare_summary_stats <- function(r_summary, python_summary_file,
                                   tolerance = 1e-12) {

  if (!file.exists(python_summary_file)) {
    stop(sprintf("Python baseline not found: %s", python_summary_file))
  }

  python_summary <- jsonlite::read_json(python_summary_file)

  results <- list()

  # Compare each numeric field
  common_fields <- intersect(names(r_summary), names(python_summary))

  for (field in common_fields) {
    r_val <- r_summary[[field]]
    python_val <- python_summary[[field]]

    if (is.numeric(r_val) && is.numeric(python_val)) {
      diff <- abs(r_val - python_val)
      matches <- diff <= tolerance

      results[[field]] <- list(
        match = matches,
        r_value = r_val,
        python_value = python_val,
        diff = diff
      )

      if (matches) {
        message(sprintf("  ✓ %s: %.6f (diff: %.2e)", field, r_val, diff))
      } else {
        warning(sprintf(
          "  ✗ %s: R=%.6f, Python=%.6f (diff: %.2e > tolerance %.2e)",
          field, r_val, python_val, diff, tolerance
        ))
      }
    } else if (!identical(class(r_val), class(python_val))) {
      warning(sprintf(
        "  ✗ %s: Type mismatch (R: %s, Python: %s)",
        field, class(r_val), class(python_val)
      ))
      results[[field]] <- list(match = FALSE, note = "Type mismatch")
    }
  }

  # Check for missing fields
  r_only <- setdiff(names(r_summary), names(python_summary))
  python_only <- setdiff(names(python_summary), names(r_summary))

  if (length(r_only) > 0) {
    message("  Fields only in R: ", paste(r_only, collapse = ", "))
  }
  if (length(python_only) > 0) {
    message("  Fields only in Python: ", paste(python_only, collapse = ", "))
  }

  return(results)
}

#' Compare data frames
#'
#' @param r_df tibble or data.frame from R
#' @param python_file Path to Python CSV or Parquet file
#' @param key_cols Character vector of key columns for joining
#' @param tolerance Numeric tolerance (default: 1e-12)
#' @param ignore_row_order Logical (default: TRUE)
#'
#' @return List with detailed comparison results
compare_dataframes <- function(r_df, python_file,
                                key_cols = NULL,
                                tolerance = 1e-12,
                                ignore_row_order = TRUE) {

  if (!file.exists(python_file)) {
    stop(sprintf("Python baseline not found: %s", python_file))
  }

  # Load Python data
  if (grepl("\\.csv$", python_file)) {
    python_df <- readr::read_csv(python_file, show_col_types = FALSE)
  } else if (grepl("\\.parquet$", python_file)) {
    python_df <- arrow::read_parquet(python_file)
  } else {
    stop("Unsupported file type (must be .csv or .parquet)")
  }

  results <- list()

  # 1. Dimension comparison
  results$dimensions <- list(
    r_rows = nrow(r_df),
    python_rows = nrow(python_df),
    r_cols = ncol(r_df),
    python_cols = ncol(python_df),
    rows_match = nrow(r_df) == nrow(python_df),
    cols_match = ncol(r_df) == ncol(python_df)
  )

  message(sprintf(
    "Dimensions: R(%d x %d) vs Python(%d x %d)",
    nrow(r_df), ncol(r_df), nrow(python_df), ncol(python_df)
  ))

  # 2. Column name comparison
  r_cols <- names(r_df)
  python_cols <- names(python_df)
  common_cols <- intersect(r_cols, python_cols)

  results$columns <- list(
    common = common_cols,
    r_only = setdiff(r_cols, python_cols),
    python_only = setdiff(python_cols, r_cols)
  )

  if (length(results$columns$r_only) > 0) {
    message("Columns only in R: ", paste(results$columns$r_only, collapse = ", "))
  }
  if (length(results$columns$python_only) > 0) {
    message("Columns only in Python: ", paste(results$columns$python_only, collapse = ", "))
  }

  # 3. Sort for comparison if needed
  if (ignore_row_order && !is.null(key_cols)) {
    r_df <- r_df %>% dplyr::arrange(dplyr::across(dplyr::all_of(key_cols)))
    python_df <- python_df %>% dplyr::arrange(dplyr::across(dplyr::all_of(key_cols)))
  }

  # 4. Compare values column by column
  results$column_comparisons <- list()

  for (col in common_cols) {
    r_vals <- r_df[[col]]
    python_vals <- python_df[[col]]

    if (is.numeric(r_vals) && is.numeric(python_vals)) {
      comp <- compare_numeric_values(
        r_vals, python_vals,
        tolerance = tolerance,
        label = col
      )
      results$column_comparisons[[col]] <- comp
    } else {
      # Character/factor comparison
      matches <- identical(as.character(r_vals), as.character(python_vals))
      results$column_comparisons[[col]] <- list(
        match = matches,
        type = "character"
      )
      if (matches) {
        message(sprintf("  ✓ %s: exact match", col))
      } else {
        warning(sprintf("  ✗ %s: values differ", col))
      }
    }
  }

  # Overall result
  all_cols_match <- all(sapply(results$column_comparisons, function(x) x$match))
  results$overall_match <- all_cols_match && results$dimensions$rows_match &&
    results$dimensions$cols_match

  return(results)
}

#' Generate comparison report
#'
#' @param test_name Character name of test
#' @param comparison_results List of comparison results
#' @param output_file Character path to markdown report (default: NULL)
#'
#' @return Invisible NULL
generate_comparison_report <- function(test_name, comparison_results,
                                        output_file = NULL) {

  report_lines <- c(
    paste("#", test_name, "- Cross-Language Comparison Report"),
    "",
    paste("**Generated:**", Sys.time()),
    ""
  )

  # Add results
  for (component in names(comparison_results)) {
    report_lines <- c(
      report_lines,
      paste("##", component),
      ""
    )

    result <- comparison_results[[component]]

    if (is.list(result) && "match" %in% names(result)) {
      status <- if (result$match) "✓ PASS" else "✗ FAIL"
      report_lines <- c(report_lines, paste("Status:", status), "")

      if (!result$match && "max_diff" %in% names(result)) {
        report_lines <- c(
          report_lines,
          paste("- Max difference:", sprintf("%.2e", result$max_diff)),
          paste("- Mean difference:", sprintf("%.2e", result$mean_diff)),
          ""
        )
      }
    }
  }

  # Write to file if specified
  if (!is.null(output_file)) {
    writeLines(report_lines, output_file)
    message("Report written to: ", output_file)
  }

  invisible(NULL)
}
