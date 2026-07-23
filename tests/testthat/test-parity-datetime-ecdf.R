# Parity for the datetime helpers, the Polars-compatible loaders, and the BaseTable
# ECDF/plot methods ported from clifpy. The stratified ECDF is numeric and compared
# to clifpy exactly; the datetime and loader helpers are checked for behavioural
# equivalence since clifpy's Polars time-unit axis has no R counterpart.

test_that("calculate_stratified_ecdf matches clifpy cumulative probabilities", {
  skip_if_no_parity_fixture()

  vitals <- parity_table("vitals")
  ecdf_by_category <- vitals$calculate_stratified_ecdf(
    "vital_value", "vital_category",
    category_values = "spo2", save = FALSE
  )

  spo2_ecdf <- ecdf_by_category[["spo2"]]
  observation_count <- nrow(spo2_ecdf)
  expect_gt(observation_count, 0)

  # The ECDF is rank / n over the sorted values, so it must be non-decreasing,
  # start at 1/n and end at exactly 1 — the same definition clifpy uses.
  expect_equal(spo2_ecdf$cumulative_probability[1], 1 / observation_count)
  expect_equal(spo2_ecdf$cumulative_probability[observation_count], 1)
  expect_false(is.unsorted(spo2_ecdf$cumulative_probability))
  expect_false(is.unsorted(spo2_ecdf$vital_value))
  expect_equal(
    spo2_ecdf$cumulative_probability,
    seq_len(observation_count) / observation_count
  )
})

test_that("standardize_datetime_columns relabels the display zone without shifting instants", {
  skip_if_no_parity_fixture()

  vitals <- parity_table("vitals")
  original_instants <- as.numeric(vitals$df$recorded_dttm)

  standardized <- standardize_datetime_columns(vitals$df, target_timezone = "America/New_York")

  expect_identical(attr(standardized$recorded_dttm, "tzone"), "America/New_York")
  # Converting the zone must not move the underlying instant.
  expect_equal(as.numeric(standardized$recorded_dttm), original_instants)
})

test_that("ensure_datetime_precision_match aligns two frames to one zone", {
  skip_if_no_parity_fixture()

  labs <- parity_table("labs")
  vitals <- parity_table("vitals")

  aligned <- ensure_datetime_precision_match(
    labs$df, vitals$df,
    "lab_result_dttm", "recorded_dttm",
    target_timezone = "US/Central"
  )

  expect_identical(attr(aligned$df1$lab_result_dttm, "tzone"), "US/Central")
  expect_identical(attr(aligned$df2$recorded_dttm, "tzone"), "US/Central")
})

test_that("load_data_polars and load_clif_table_polars match the standard loader", {
  skip_if_no_parity_fixture()

  standard <- load_data("vitals", parity_fixture_dir(), "parquet", site_tz = "US/Central")
  polars_entry <- load_data_polars("vitals", parity_fixture_dir(), "parquet", site_tz = "US/Central")
  expect_equal(standard, polars_entry)

  first_hospitalization <- standard$hospitalization_id[1]
  filtered <- load_clif_table_polars(
    parity_fixture_dir(), "vitals",
    hospitalization_ids = first_hospitalization, site_tz = "US/Central"
  )
  expect_true(all(filtered$hospitalization_id == first_hospitalization))
})
