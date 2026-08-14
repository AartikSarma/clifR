# Cross-language parity for CDC Adult Sepsis Event (ASE).
#
# Baseline comes from tests/baseline_py/generate_ase_baseline.py, which runs
# clifpy.utils.ase.compute_ase on the shared cohort fixture and writes
# tests/baseline/ase.parquet (columns written in sorted order).
#
# ASE sepsis flags are 0/1 integers and must match clifpy exactly. Every
# datetime, id, and categorical column must match too. The only exceptions are
# the three quality-control columns built with DuckDB `string_agg(DISTINCT ...)`
# (anchor_meds_in_window, anchor_parenteral_meds_in_window, run_meds): DuckDB
# does not guarantee concatenation order for string_agg without ORDER BY, so the
# same medication *set* can serialize in a different order between clifpy's and
# clifR's DuckDB runs. These are compared as sets.

baseline_directory <- testthat::test_path("..", "baseline")
fixture_directory <- testthat::test_path("..", "fixtures", "cohort")

# clifpy's native compute_ase column order (the baseline parquet stores columns
# alphabetically, so the native order is asserted directly).
ASE_EXPECTED_COLUMNS <- c(
  "hospitalization_id", "bc_id", "episode_id", "type", "presumed_infection",
  "sepsis", "sepsis_wo_lactate", "no_sepsis_reason", "blood_culture_dttm",
  "total_qad", "qad_start_date", "qad_end_date", "first_qad_dttm",
  "presumed_infection_onset_dttm", "ase_onset_w_lactate_dttm",
  "ase_first_criteria_w_lactate", "ase_onset_wo_lactate_dttm",
  "ase_first_criteria_wo_lactate", "vasopressor_dttm", "vasopressor_name",
  "imv_dttm", "aki_dttm", "hyperbilirubinemia_dttm", "thrombocytopenia_dttm",
  "lactate_dttm", "has_esrd", "anchor_meds_in_window",
  "anchor_parenteral_meds_in_window", "run_meds", "final_qad_status"
)

ASE_INTEGER_COLUMNS <- c("presumed_infection", "sepsis", "sepsis_wo_lactate", "has_esrd")
ASE_NUMERIC_COLUMNS <- c("bc_id", "episode_id", "total_qad")
ASE_DATETIME_COLUMNS <- c(
  "blood_culture_dttm", "qad_start_date", "qad_end_date", "first_qad_dttm",
  "presumed_infection_onset_dttm", "ase_onset_w_lactate_dttm",
  "ase_onset_wo_lactate_dttm", "vasopressor_dttm", "imv_dttm", "aki_dttm",
  "hyperbilirubinemia_dttm", "thrombocytopenia_dttm", "lactate_dttm"
)
ASE_STRING_COLUMNS <- c(
  "type", "no_sepsis_reason", "ase_first_criteria_w_lactate",
  "ase_first_criteria_wo_lactate", "vasopressor_name", "final_qad_status"
)
ASE_SET_COLUMNS <- c(
  "anchor_meds_in_window", "anchor_parenteral_meds_in_window", "run_meds"
)

read_ase_baseline <- function() {
  artifact_path <- file.path(baseline_directory, "ase.parquet")
  skip_if_not(file.exists(artifact_path), "missing baseline: ase")
  dplyr::as_tibble(arrow::read_parquet(artifact_path))
}

# Deterministic sort by (hospitalization_id, bc_id) with NA bc_id last.
sort_ase <- function(frame) {
  bc_key <- ifelse(is.na(frame$bc_id), "", sprintf("%09d", as.integer(frame$bc_id)))
  frame[order(as.character(frame$hospitalization_id), bc_key), , drop = FALSE]
}

as_utc_numeric <- function(column_values) {
  as.numeric(as.POSIXct(column_values, tz = "UTC"))
}

as_plain_numeric <- function(column_values) {
  if (inherits(column_values, "integer64")) {
    return(as.numeric(column_values))
  }
  as.numeric(column_values)
}

# Split a "a, b, c" cell into a sorted, order-independent canonical string.
canonicalize_set <- function(cells) {
  vapply(
    cells,
    function(cell) {
      if (is.na(cell)) {
        return(NA_character_)
      }
      paste(sort(trimws(strsplit(cell, ",")[[1]])), collapse = ", ")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

test_that("compute_ase matches clifpy on the cohort fixture, row for row", {
  python_ase <- read_ase_baseline()
  r_ase <- compute_ase(
    data_directory = fixture_directory,
    filetype = "parquet",
    timezone = "UTC",
    verbose = FALSE
  )

  # Native column order matches clifpy; the baseline stores the same set sorted.
  expect_identical(names(r_ase), ASE_EXPECTED_COLUMNS)
  expect_setequal(names(r_ase), names(python_ase))
  expect_equal(nrow(r_ase), nrow(python_ase))

  r_sorted <- sort_ase(r_ase)
  python_sorted <- sort_ase(python_ase)

  # Sepsis flags are integers and must match exactly.
  for (column_name in ASE_INTEGER_COLUMNS) {
    expect_identical(
      as.integer(r_sorted[[column_name]]),
      as.integer(python_sorted[[column_name]]),
      info = column_name
    )
  }

  for (column_name in ASE_NUMERIC_COLUMNS) {
    expect_equal(
      as_plain_numeric(r_sorted[[column_name]]),
      as_plain_numeric(python_sorted[[column_name]]),
      info = column_name
    )
  }

  for (column_name in ASE_DATETIME_COLUMNS) {
    expect_equal(
      as_utc_numeric(r_sorted[[column_name]]),
      as_utc_numeric(python_sorted[[column_name]]),
      info = column_name
    )
  }

  for (column_name in ASE_STRING_COLUMNS) {
    expect_identical(
      as.character(r_sorted[[column_name]]),
      as.character(python_sorted[[column_name]]),
      info = column_name
    )
  }

  # string_agg QC columns: identical medication sets (order not guaranteed).
  for (column_name in ASE_SET_COLUMNS) {
    expect_identical(
      canonicalize_set(as.character(r_sorted[[column_name]])),
      canonicalize_set(as.character(python_sorted[[column_name]])),
      info = column_name
    )
  }
})

test_that("compute_ase identifies the expected sepsis and episode structure", {
  python_ase <- read_ase_baseline()
  r_ase <- compute_ase(
    data_directory = fixture_directory,
    filetype = "parquet",
    timezone = "UTC",
    verbose = FALSE
  )

  # The fixture yields real positive cases; guard against an all-zero regression.
  expect_gt(sum(r_ase$sepsis == 1, na.rm = TRUE), 0)
  expect_equal(
    sum(r_ase$sepsis == 1, na.rm = TRUE),
    sum(python_ase$sepsis == 1, na.rm = TRUE)
  )

  # No-blood-culture rows carry the documented reason and null bc_id/episode_id.
  no_bc <- r_ase[is.na(r_ase$bc_id), ]
  expect_true(all(no_bc$no_sepsis_reason == "no_blood_culture"))
  expect_true(all(no_bc$sepsis == 0))
  expect_true(all(is.na(no_bc$episode_id)))

  # episode_id is assigned only to surviving sepsis rows.
  expect_true(all(r_ase$sepsis[!is.na(r_ase$episode_id)] == 1))
  expect_identical(
    sum(!is.na(r_ase$episode_id)),
    sum(r_ase$sepsis == 1, na.rm = TRUE)
  )
})
