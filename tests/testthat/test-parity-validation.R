# Validation parity. clifR's validate() must surface the same findings clifpy does:
# the same error types, on the same columns, in the same counts. The comparison is
# on the set of (type, column) pairs rather than on free-text descriptions, so
# wording differences do not cause failures but a missed or invented check does.

parity_error_signature <- function(error_records) {
  if (length(error_records) == 0) {
    return(character(0))
  }
  signatures <- vapply(error_records, function(error_record) {
    error_type <- error_record[["type"]] %||% NA_character_
    error_details <- error_record[["details"]]
    error_column <- if (is.list(error_details)) error_details[["column"]] %||% NA_character_ else NA_character_
    paste(as.character(error_type), as.character(error_column), sep = " | ")
  }, character(1))
  sort(signatures)
}

test_that("every fixture table loads with the row and column count clifpy saw", {
  skip_if_no_parity_fixture("validation_by_table")

  python_validation <- read_parity_baseline("validation_by_table")

  for (table_name in names(python_validation)) {
    python_result <- python_validation[[table_name]]
    if (!identical(python_result$status, "loaded")) next

    r_table <- parity_table(table_name)

    expect_equal(
      nrow(r_table$df), python_result$n_rows,
      info = sprintf("table '%s': row count differs from clifpy", table_name)
    )
    expect_setequal(names(r_table$df), unlist(python_result$columns))
  }
})

test_that("validation findings match clifpy per table", {
  skip_if_no_parity_fixture("validation_by_table")

  python_validation <- read_parity_baseline("validation_by_table")

  for (table_name in names(python_validation)) {
    python_result <- python_validation[[table_name]]
    if (!identical(python_result$status, "loaded")) next

    r_table <- parity_table(table_name)
    r_table$validate(verbose = FALSE)

    expect_equal(
      parity_error_signature(r_table$errors),
      parity_error_signature(python_result$errors),
      info = sprintf("table '%s': validation findings differ from clifpy", table_name)
    )

    expect_equal(
      r_table$isvalid(), python_result$is_valid,
      info = sprintf("table '%s': validity verdict differs from clifpy", table_name)
    )
  }
})

test_that("table summaries match clifpy", {
  skip_if_no_parity_fixture("table_summaries")

  python_summaries <- read_parity_baseline("table_summaries")

  for (table_name in names(python_summaries)) {
    python_summary <- python_summaries[[table_name]]
    r_summary <- parity_table(table_name)$get_summary()

    expect_equal(
      r_summary$num_rows, python_summary$num_rows,
      info = sprintf("table '%s': num_rows differs", table_name)
    )
    expect_equal(
      r_summary$num_columns, python_summary$num_columns,
      info = sprintf("table '%s': num_columns differs", table_name)
    )

    # Numeric summaries come from pandas describe(); R's quantile(type = 7) is the
    # matching definition, so these should agree to floating point.
    python_numeric_stats <- python_summary$numeric_stats
    if (!is.null(python_numeric_stats)) {
      for (column_name in intersect(names(python_numeric_stats), names(r_summary$numeric_stats))) {
        for (statistic_name in c("mean", "min", "max", "50%")) {
          python_statistic <- python_numeric_stats[[column_name]][[statistic_name]]
          r_statistic <- r_summary$numeric_stats[[column_name]][[statistic_name]]
          if (is.null(python_statistic) || is.null(r_statistic)) next
          if (is.na(python_statistic) && is.na(r_statistic)) next
          expect_equal(
            as.numeric(r_statistic), as.numeric(python_statistic),
            tolerance = PARITY_TOLERANCE_MODERATE,
            info = sprintf("table '%s', column '%s', statistic '%s' differs",
                           table_name, column_name, statistic_name)
          )
        }
      }
    }
  }
})
