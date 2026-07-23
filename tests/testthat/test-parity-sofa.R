# Cross-language parity for SOFA scoring.
#
# Baselines come from tests/baseline_py:
#   generate_baselines.py --only sofa   -> sofa_scores, sofa_wide_input
#   generate_sofa_baseline.py           -> sofa_wide_*, sofa_direct_*, sofa_cohort_*
#
# SOFA component scores are integers and must match clifpy exactly; there is no
# tolerance budget for them.

baseline_directory <- testthat::test_path("..", "baseline")
fixture_directory <- testthat::test_path("..", "fixtures", "cohort")

read_baseline <- function(artifact_name) {
  artifact_path <- file.path(baseline_directory, paste0(artifact_name, ".parquet"))
  skip_if_not(file.exists(artifact_path), paste("missing baseline:", artifact_name))
  dplyr::as_tibble(arrow::read_parquet(artifact_path))
}

SOFA_SCORE_COLUMNS <- c(
  "sofa_cv_97", "sofa_coag", "sofa_liver", "sofa_resp", "sofa_cns", "sofa_renal", "sofa_total"
)

expect_sofa_parity <- function(r_scores, python_scores, id_column,
                               numeric_columns = c("p_f", "p_f_imputed")) {
  expect_setequal(names(r_scores), names(python_scores))
  expect_equal(nrow(r_scores), nrow(python_scores))

  comparison <- dplyr::inner_join(
    r_scores, python_scores,
    by = id_column, suffix = c("_r", "_python")
  )
  expect_equal(nrow(comparison), nrow(python_scores))

  for (score_column in SOFA_SCORE_COLUMNS) {
    expect_identical(
      as.integer(comparison[[paste0(score_column, "_r")]]),
      as.integer(comparison[[paste0(score_column, "_python")]]),
      info = score_column
    )
  }

  for (numeric_column in intersect(numeric_columns, names(r_scores))) {
    expect_equal(
      as.numeric(comparison[[paste0(numeric_column, "_r")]]),
      as.numeric(comparison[[paste0(numeric_column, "_python")]]),
      tolerance = 1e-12,
      info = numeric_column
    )
  }
}

# --------------------------------------------------------------------------------
# compute_sofa: scoring an already-built wide dataset
# --------------------------------------------------------------------------------

test_that("compute_sofa matches clifpy on the orchestrator's wide dataset", {
  wide_input <- read_baseline("sofa_wide_input")
  python_scores <- read_baseline("sofa_scores")

  r_scores <- compute_sofa(wide_input, id_name = "hospitalization_id")

  expect_sofa_parity(r_scores, python_scores, "hospitalization_id")
})

test_that("compute_sofa matches clifpy on a wide dataset with vitals populated", {
  wide_input <- read_baseline("sofa_wide_full_input")
  python_scores <- read_baseline("sofa_wide_default")

  r_scores <- compute_sofa(wide_input, id_name = "hospitalization_id")

  expect_sofa_parity(r_scores, python_scores, "hospitalization_id")
  # This input, unlike sofa_wide_input, actually exercises the CV component.
  expect_gt(sum(r_scores$sofa_cv_97 > 0), 0)
})

test_that("compute_sofa matches clifpy without filling missing components", {
  wide_input <- read_baseline("sofa_wide_full_input")
  python_scores <- read_baseline("sofa_wide_nofill")

  r_scores <- compute_sofa(
    wide_input,
    id_name = "hospitalization_id", fill_na_scores_with_zero = FALSE
  )

  expect_sofa_parity(r_scores, python_scores, "hospitalization_id")
  expect_true(anyNA(r_scores$sofa_resp))
})

test_that("compute_sofa matches clifpy with outlier removal disabled", {
  wide_input <- read_baseline("sofa_wide_full_input")
  python_scores <- read_baseline("sofa_wide_keep_outliers")

  r_scores <- compute_sofa(
    wide_input,
    id_name = "hospitalization_id", remove_outliers = FALSE
  )

  expect_sofa_parity(r_scores, python_scores, "hospitalization_id")
})

test_that("compute_sofa matches clifpy when a cohort time window is applied", {
  wide_input <- read_baseline("sofa_wide_full_input")
  python_scores <- read_baseline("sofa_wide_cohort48h")
  cohort <- read_baseline("sofa_cohort_48h") |>
    dplyr::rename(start_time = "start_dttm", end_time = "end_dttm")

  r_scores <- compute_sofa(wide_input, cohort_df = cohort, id_name = "hospitalization_id")

  expect_sofa_parity(r_scores, python_scores, "hospitalization_id")
})

test_that("compute_sofa rejects invalid arguments", {
  wide_input <- read_baseline("sofa_wide_full_input")

  expect_error(compute_sofa(wide_input, id_name = "not_a_column"), "not found")
  expect_error(
    compute_sofa(wide_input, id_name = "hospitalization_id", extremal_type = "median"),
    "extremal_type"
  )
})

# --------------------------------------------------------------------------------
# compute_sofa_direct: scoring straight from the raw tables
# --------------------------------------------------------------------------------

test_that("compute_sofa_direct matches clifpy over full hospitalizations", {
  cohort <- read_baseline("sofa_cohort_full")
  python_scores <- read_baseline("sofa_direct_full")

  r_scores <- compute_sofa_direct(fixture_directory, cohort, timezone = "UTC")

  expect_sofa_parity(
    r_scores, python_scores, "hospitalization_id",
    numeric_columns = c(
      "p_f", "p_f_imputed", "pao2_imputed", "po2_arterial", "po2_arterial_right",
      "fio2_set", "map", "spo2", "creatinine", "bilirubin_total", "platelet_count",
      "gcs_total", "weight_kg", "device_rank",
      "norepinephrine_mcg_kg_min", "epinephrine_mcg_kg_min",
      "dopamine_mcg_kg_min", "dobutamine_mcg_kg_min"
    )
  )
})

test_that("compute_sofa_direct matches clifpy over 48-hour windows", {
  cohort <- read_baseline("sofa_cohort_48h")
  python_scores <- read_baseline("sofa_direct_48h")

  r_scores <- compute_sofa_direct(fixture_directory, cohort, timezone = "UTC")

  expect_sofa_parity(r_scores, python_scores, "hospitalization_id")
})

test_that("compute_sofa_direct matches clifpy without filling or outlier removal", {
  cohort <- read_baseline("sofa_cohort_48h")
  python_scores <- read_baseline("sofa_direct_nofill")

  r_scores <- compute_sofa_direct(
    fixture_directory, cohort,
    timezone = "UTC", fill_na_scores_with_zero = FALSE, remove_outliers = FALSE
  )

  expect_sofa_parity(r_scores, python_scores, "hospitalization_id")
})

test_that("compute_sofa_direct matches clifpy when grouping by encounter_block", {
  cohort <- read_baseline("sofa_cohort_blocks")
  python_scores <- read_baseline("sofa_direct_blocks")

  r_scores <- compute_sofa_direct(
    fixture_directory, cohort,
    id_name = "encounter_block", timezone = "UTC"
  )

  expect_sofa_parity(r_scores, python_scores, "encounter_block")
})

test_that("compute_sofa_direct rejects an incomplete cohort", {
  cohort <- read_baseline("sofa_cohort_full")

  expect_error(
    compute_sofa_direct(fixture_directory, dplyr::select(cohort, "hospitalization_id")),
    "must contain columns"
  )
  expect_error(
    compute_sofa_direct(fixture_directory, cohort, id_name = "encounter_block"),
    "not found"
  )
})

# --------------------------------------------------------------------------------
# Ported constants
# --------------------------------------------------------------------------------

test_that("SOFA module constants match clifpy", {
  expect_identical(
    REQUIRED_SOFA_CATEGORIES_BY_TABLE$labs,
    c("creatinine", "platelet_count", "po2_arterial", "bilirubin_total")
  )
  expect_identical(REQUIRED_SOFA_CATEGORIES_BY_TABLE$vitals, c("map", "spo2"))
  expect_identical(names(DEVICE_RANK_DICT)[1], "IMV")
  expect_identical(unname(DEVICE_RANK_DICT[["Room Air"]]), 9L)
  expect_identical(nrow(DEVICE_RANK_MAPPING), 9L)
  expect_length(MAX_ITEMS, 7)
  expect_length(MIN_ITEMS, 6)
})
