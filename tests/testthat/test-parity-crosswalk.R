# Crosswalk parity: CLIF 2.1 to 3.0 category value migration. The normalization
# rule is deterministic and string-only, so it is compared exactly, value by value.

test_that("normalize_category_value matches clifpy on every sampled input", {
  skip_if_no_parity_fixture("crosswalk_normalize_category_value")

  python_normalized <- read_parity_baseline("crosswalk_normalize_category_value")

  for (normalization_record in python_normalized) {
    input_value <- normalization_record$input
    expect_equal(
      normalize_category_value(input_value),
      normalization_record$output,
      info = sprintf("normalize_category_value('%s') differs from clifpy", input_value)
    )
  }
})

test_that("crosswalk_table_2_1_to_3_0 reports the same conversion counts", {
  skip_if_no_parity_fixture("crosswalk_reports")

  python_reports <- read_parity_baseline("crosswalk_reports")

  for (table_name in names(python_reports)) {
    python_report <- python_reports[[table_name]]

    r_table <- parity_table(table_name)
    r_result <- crosswalk_table_2_1_to_3_0(r_table$df, table_name)

    expect_equal(
      r_result$report$table, python_report$table,
      info = sprintf("crosswalk report table name differs for '%s'", table_name)
    )
    expect_equal(
      r_result$report$is_complete, python_report$is_complete,
      info = sprintf("crosswalk completeness differs for '%s'", table_name)
    )

    python_columns <- python_report$columns
    if (is.null(python_columns)) next

    for (column_name in names(python_columns)) {
      r_column_report <- r_result$report$columns[[column_name]]
      expect_false(
        is.null(r_column_report),
        info = sprintf("crosswalk report missing column '%s' for table '%s'", column_name, table_name)
      )
      if (is.null(r_column_report)) next

      expect_equal(
        r_column_report$n_values_converted,
        python_columns[[column_name]]$n_values_converted,
        info = sprintf("converted count differs for %s.%s", table_name, column_name)
      )
    }
  }
})

test_that("crosswalking does not modify the input frame", {
  skip_if_no_parity_fixture()

  r_table <- parity_table("patient")
  original_frame <- r_table$df

  crosswalk_table_2_1_to_3_0(r_table$df, "patient")

  # clifpy documents the crosswalk as non-mutating; R's copy-on-modify makes this
  # easy to get right, but an in-place duckdb write would break it.
  expect_identical(r_table$df, original_frame)
})
