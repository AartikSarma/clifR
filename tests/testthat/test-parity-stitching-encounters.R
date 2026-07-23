# Cross-language parity: encounter stitching
#
# Compares clifR's stitch_encounters() against the clifpy 0.5.0 baseline in
# tests/baseline/encounter_mapping.parquet, computed on the shared cohort
# fixture with the default 6-hour window. Encounter block assignment is exact.

cohort_fixture_directory <- testthat::test_path("..", "fixtures", "cohort")
baseline_directory <- testthat::test_path("..", "baseline")

skip_without_stitching_baseline <- function() {
  testthat::skip_if_not(
    dir.exists(cohort_fixture_directory) &&
      file.exists(file.path(baseline_directory, "encounter_mapping.parquet")),
    "Cohort fixture or encounter mapping baseline not available"
  )
}

load_cohort_stitching_inputs <- function() {
  list(
    hospitalization = load_data("hospitalization", cohort_fixture_directory, "parquet"),
    adt = load_data("adt", cohort_fixture_directory, "parquet")
  )
}

test_that("stitch_encounters matches the clifpy encounter mapping exactly", {
  skip_without_stitching_baseline()

  stitching_inputs <- load_cohort_stitching_inputs()
  stitching_result <- stitch_encounters(
    stitching_inputs$hospitalization,
    stitching_inputs$adt,
    time_interval = 6
  )

  clifr_mapping <- stitching_result$encounter_mapping
  clifr_mapping <- clifr_mapping[order(clifr_mapping$hospitalization_id), , drop = FALSE]

  baseline_mapping <- arrow::read_parquet(
    file.path(baseline_directory, "encounter_mapping.parquet")
  )
  baseline_mapping <- baseline_mapping[order(baseline_mapping$hospitalization_id), , drop = FALSE]

  expect_equal(nrow(clifr_mapping), nrow(baseline_mapping))
  expect_identical(
    as.character(clifr_mapping$hospitalization_id),
    as.character(baseline_mapping$hospitalization_id)
  )
  expect_identical(
    as.integer(clifr_mapping$encounter_block),
    as.integer(baseline_mapping$encounter_block)
  )
})

test_that("stitch_encounters returns the three named clifpy outputs", {
  skip_without_stitching_baseline()

  stitching_inputs <- load_cohort_stitching_inputs()
  stitching_result <- stitch_encounters(stitching_inputs$hospitalization, stitching_inputs$adt)

  expect_named(
    stitching_result,
    c("hospitalization", "adt", "encounter_mapping")
  )
  expect_equal(nrow(stitching_result$hospitalization), nrow(stitching_inputs$hospitalization))
  expect_equal(nrow(stitching_result$adt), nrow(stitching_inputs$adt))
  expect_true("encounter_block" %in% names(stitching_result$hospitalization))
  expect_true("encounter_block" %in% names(stitching_result$adt))
})

test_that("linked hospitalizations share an encounter block", {
  # P1: two stays with a 6-hour gap (exactly at the default boundary) plus a
  # third linked 3 hours later; P2 has a single unlinked stay.
  hospitalization_data <- dplyr::tibble(
    patient_id = c("P1", "P1", "P1", "P2"),
    hospitalization_id = c("A1", "A2", "A3", "B1"),
    admission_dttm = as.POSIXct(
      c("2024-01-01 00:00", "2024-01-05 10:00", "2024-01-08 03:00", "2024-02-01 00:00"),
      tz = "UTC"
    ),
    discharge_dttm = as.POSIXct(
      c("2024-01-05 04:00", "2024-01-08 00:00", "2024-01-20 00:00", "2024-02-10 00:00"),
      tz = "UTC"
    ),
    age_at_admission = c(60, 60, 60, 45),
    admission_type_category = "emergency",
    discharge_category = "home"
  )
  adt_data <- dplyr::tibble(
    hospitalization_id = c("A1", "A2", "A3", "B1"),
    in_dttm = hospitalization_data$admission_dttm,
    out_dttm = hospitalization_data$discharge_dttm,
    location_category = c("ed", "icu", "ward", "ward"),
    hospital_id = "HOSP_A"
  )

  encounter_mapping <- stitch_encounters(
    hospitalization_data, adt_data, time_interval = 6
  )$encounter_mapping
  encounter_blocks <- stats::setNames(
    encounter_mapping$encounter_block, encounter_mapping$hospitalization_id
  )

  expect_equal(unname(encounter_blocks[["A1"]]), unname(encounter_blocks[["A2"]]))
  expect_equal(unname(encounter_blocks[["A2"]]), unname(encounter_blocks[["A3"]]))
  expect_false(encounter_blocks[["A1"]] == encounter_blocks[["B1"]])

  # A 5-hour window is below the 6-hour A1 -> A2 gap, so A1 splits off while
  # the 3-hour A2 -> A3 gap still links.
  narrow_window_mapping <- stitch_encounters(
    hospitalization_data, adt_data, time_interval = 5
  )$encounter_mapping
  narrow_window_blocks <- stats::setNames(
    narrow_window_mapping$encounter_block, narrow_window_mapping$hospitalization_id
  )
  expect_false(narrow_window_blocks[["A1"]] == narrow_window_blocks[["A2"]])
  expect_equal(unname(narrow_window_blocks[["A2"]]), unname(narrow_window_blocks[["A3"]]))
})

test_that("stitch_encounters rejects inputs missing required columns", {
  skip_without_stitching_baseline()

  stitching_inputs <- load_cohort_stitching_inputs()
  expect_error(
    stitch_encounters(
      dplyr::select(stitching_inputs$hospitalization, -"age_at_admission"),
      stitching_inputs$adt
    ),
    "age_at_admission"
  )
  expect_error(
    stitch_encounters(
      stitching_inputs$hospitalization,
      dplyr::select(stitching_inputs$adt, -"hospital_id")
    ),
    "hospital_id"
  )
})
