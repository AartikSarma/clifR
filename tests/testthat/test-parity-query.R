# Query parity: extremal-value lookups in the fixture vitals table. The ranking
# and windowing run in DuckDB with clifpy's SQL, so pivoted min/max/latest values
# must match to floating-point tolerance and the pivoted column set must match
# exactly.

test_that("lookup_extremal_values_in_long_table matches clifpy on the fixture vitals", {
  skip_if_no_parity_fixture("query_extremal_values")
  skip_if_no_parity_fixture("query_windows")

  python_pivoted <- read_parity_baseline("query_extremal_values")
  ids_w_dttm <- read_parity_baseline("query_windows")

  vitals_data <- load_data(
    table_name = "vitals",
    table_path = parity_fixture_dir(),
    table_format_type = PARITY_FILETYPE,
    filters = list(hospitalization_id = unique(ids_w_dttm$hospitalization_id)),
    site_tz = PARITY_TIMEZONE
  )

  query_specification <- list(
    spo2 = c("max", "min"),
    heart_rate = c("max", "min", "latest"),
    weight_kg = "latest",
    temp_c = "latest"
  )

  r_pivoted <- lookup_extremal_values_in_long_table(
    ids_w_dttm, query_specification, "vitals", data = vitals_data
  )

  expect_setequal(names(r_pivoted), names(python_pivoted))
  expect_parity(
    r_pivoted,
    python_pivoted,
    sort_columns = c("hospitalization_id", "start_dttm", "end_dttm"),
    tolerance = PARITY_TOLERANCE_STRICT,
    label = "query_extremal_values"
  )
})

test_that("lookup_extremal_values_in_long_table validates its inputs", {
  skip_if_no_parity_fixture()

  windows_missing_end <- data.frame(
    hospitalization_id = "H1",
    start_dttm = as.POSIXct("2023-01-01", tz = "UTC")
  )
  expect_error(
    lookup_extremal_values_in_long_table(
      windows_missing_end, list(spo2 = "max"), "vitals", data = data.frame()
    ),
    "must have columns"
  )

  valid_windows <- data.frame(
    hospitalization_id = "H1",
    start_dttm = as.POSIXct("2023-01-01", tz = "UTC"),
    end_dttm = as.POSIXct("2023-01-02", tz = "UTC")
  )
  expect_error(
    lookup_extremal_values_in_long_table(
      valid_windows, list(spo2 = "max"), "not_a_table", data = data.frame()
    ),
    "must be one of the long tables"
  )
})
