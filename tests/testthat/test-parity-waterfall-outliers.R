# Respiratory support waterfall and outlier handling parity.
#
# The waterfall is the most order-sensitive port in the package: scaffold
# construction, episode identification and directional fill each depend on the
# previous step, so a subtle reordering shows up as scattered value differences
# rather than a clean structural failure. Both are compared here.

test_that("process_resp_support_waterfall matches clifpy", {
  skip_if_no_parity_fixture("resp_support_waterfall")

  respiratory_support <- parity_table("respiratory_support")

  respiratory_frame <- respiratory_support$df
  for (datetime_column in grep("dttm", names(respiratory_frame), value = TRUE)) {
    attr(respiratory_frame[[datetime_column]], "tzone") <- "UTC"
  }

  r_waterfall <- process_resp_support_waterfall(respiratory_frame, verbose = FALSE)
  python_waterfall <- read_parity_baseline("resp_support_waterfall")

  expect_parity(
    r_waterfall,
    python_waterfall,
    sort_columns = c("hospitalization_id", "recorded_dttm"),
    tolerance = PARITY_TOLERANCE_STRICT,
    label = "resp_support_waterfall"
  )
})

test_that("the waterfall emits one row per hospitalization and timestamp", {
  skip_if_no_parity_fixture("resp_support_waterfall")

  python_waterfall <- read_parity_baseline("resp_support_waterfall")

  # Deduplication to a single row per (id, timestamp) is a contract of the
  # waterfall; duplicates would multiply rows in any downstream join.
  duplicate_count <- python_waterfall |>
    dplyr::count(.data$hospitalization_id, .data$recorded_dttm) |>
    dplyr::filter(.data$n > 1) |>
    nrow()
  expect_equal(duplicate_count, 0)
})

test_that("apply_outlier_handling nullifies the same values clifpy does", {
  for (table_name in c("vitals", "labs", "respiratory_support")) {
    artifact_name <- paste0("outlier_applied_nonnull_", table_name)
    skip_if_no_parity_fixture(artifact_name)

    r_table <- parity_table(table_name)
    apply_outlier_handling(r_table)

    python_non_null_counts <- read_parity_baseline(artifact_name)

    for (column_name in names(python_non_null_counts)) {
      if (!column_name %in% names(r_table$df)) next
      expect_equal(
        sum(!is.na(r_table$df[[column_name]])),
        as.integer(python_non_null_counts[[column_name]]),
        info = sprintf(
          "table '%s', column '%s': non-null count after outlier handling differs from clifpy",
          table_name, column_name
        )
      )
    }
  }
})
