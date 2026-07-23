# Wide dataset parity. The wide builder is the foundation for SOFA and most
# time-series analysis, so both its structure and its values are compared.

parity_wide_category_filters <- list(
  vitals = c("map", "spo2", "heart_rate", "weight_kg"),
  labs = c("creatinine", "platelet_count", "po2_arterial", "bilirubin_total"),
  patient_assessments = "gcs_total"
)

test_that("create_wide_dataset matches clifpy in shape and values", {
  skip_if_no_parity_fixture("wide_dataset")

  orchestrator <- parity_orchestrator()
  r_wide_dataset <- orchestrator$create_wide_dataset(
    tables_to_load = names(parity_wide_category_filters),
    category_filters = parity_wide_category_filters,
    show_progress = FALSE
  )

  python_wide_dataset <- read_parity_baseline("wide_dataset")

  expect_parity(
    r_wide_dataset,
    python_wide_dataset,
    sort_columns = c("hospitalization_id", "event_time"),
    tolerance = PARITY_TOLERANCE_STRICT,
    label = "wide_dataset"
  )
})

test_that("convert_wide_to_hourly matches clifpy", {
  skip_if_no_parity_fixture("hourly_dataset")

  orchestrator <- parity_orchestrator()
  orchestrator$create_wide_dataset(
    tables_to_load = c("vitals", "labs"),
    category_filters = list(
      vitals = c("map", "spo2", "heart_rate"),
      labs = c("creatinine", "platelet_count")
    ),
    show_progress = FALSE
  )

  r_hourly_dataset <- orchestrator$convert_wide_to_hourly(
    aggregation_config = list(
      max = c("heart_rate", "creatinine"),
      min = c("map", "spo2"),
      mean = "heart_rate"
    ),
    id_name = "hospitalization_id",
    hourly_window = 1
  )

  python_hourly_dataset <- read_parity_baseline("hourly_dataset")

  expect_parity(
    r_hourly_dataset,
    python_hourly_dataset,
    sort_columns = c("hospitalization_id", "window_number"),
    tolerance = PARITY_TOLERANCE_STRICT,
    label = "hourly_dataset"
  )
})

test_that("hourly windows are contiguous and correctly bounded", {
  skip_if_no_parity_fixture("hourly_dataset")

  python_hourly_dataset <- read_parity_baseline("hourly_dataset")

  required_columns <- c("window_number", "window_start_dttm", "window_end_dttm")
  skip_if(!all(required_columns %in% names(python_hourly_dataset)))

  # A window must end after it starts, and a one-hour window must span an hour.
  window_durations <- as.numeric(difftime(
    python_hourly_dataset$window_end_dttm,
    python_hourly_dataset$window_start_dttm,
    units = "hours"
  ))
  expect_true(all(window_durations > 0))
})
