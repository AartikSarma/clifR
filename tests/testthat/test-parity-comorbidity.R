# Cross-language parity: comorbidity indices (CCI and Elixhauser)
#
# Compares clifR's calculate_cci() / calculate_elix() against the clifpy 0.5.0
# baselines in tests/baseline/, both computed on the shared cohort fixture.
# Comorbidity scores are integers, so the comparison is exact.

cohort_fixture_directory <- testthat::test_path("..", "fixtures", "cohort")
baseline_directory <- testthat::test_path("..", "baseline")

skip_without_comorbidity_baselines <- function() {
  testthat::skip_if_not(
    dir.exists(cohort_fixture_directory) &&
      file.exists(file.path(baseline_directory, "cci_scores.parquet")) &&
      file.exists(file.path(baseline_directory, "elix_scores.parquet")),
    "Cohort fixture or comorbidity baselines not available"
  )
}

load_cohort_hospital_diagnosis <- function() {
  load_data("hospital_diagnosis", cohort_fixture_directory, "parquet")
}

expect_comorbidity_parity <- function(clifr_scores, baseline_scores) {
  # The baseline parquet stores columns alphabetically, so compare the column
  # set here; clifpy's native column order is asserted separately against the
  # YAML condition order.
  expect_setequal(names(clifr_scores), names(baseline_scores))
  expect_equal(nrow(clifr_scores), nrow(baseline_scores))

  clifr_sorted <- clifr_scores[order(clifr_scores$hospitalization_id), , drop = FALSE]
  baseline_sorted <- baseline_scores[order(baseline_scores$hospitalization_id), , drop = FALSE]

  expect_identical(
    as.character(clifr_sorted$hospitalization_id),
    as.character(baseline_sorted$hospitalization_id)
  )

  for (column_name in setdiff(names(baseline_sorted), "hospitalization_id")) {
    expect_identical(
      as.integer(clifr_sorted[[column_name]]),
      as.integer(baseline_sorted[[column_name]]),
      info = paste("column:", column_name)
    )
  }
}

test_that("calculate_cci matches the clifpy baseline exactly", {
  skip_without_comorbidity_baselines()

  clifr_cci_scores <- calculate_cci(load_cohort_hospital_diagnosis(), hierarchy = TRUE)
  baseline_cci_scores <- arrow::read_parquet(file.path(baseline_directory, "cci_scores.parquet"))

  expect_comorbidity_parity(clifr_cci_scores, baseline_cci_scores)

  # clifpy emits hospitalization_id, then conditions in YAML order, then score.
  cci_configuration <- yaml::read_yaml(
    clif_extdata_path("comorbidity", "cci.yaml"),
    readLines.warn = FALSE
  )
  cci_condition_names <- names(cci_configuration$diagnosis_code_mappings$ICD10CM)
  expect_identical(
    names(clifr_cci_scores),
    c("hospitalization_id", cci_condition_names, "cci_score")
  )
})

test_that("calculate_elix matches the clifpy baseline exactly", {
  skip_without_comorbidity_baselines()

  clifr_elix_scores <- calculate_elix(load_cohort_hospital_diagnosis(), hierarchy = TRUE)
  baseline_elix_scores <- arrow::read_parquet(file.path(baseline_directory, "elix_scores.parquet"))

  expect_comorbidity_parity(clifr_elix_scores, baseline_elix_scores)

  elixhauser_configuration <- yaml::read_yaml(
    clif_extdata_path("comorbidity", "elixhauser.yaml"),
    readLines.warn = FALSE
  )
  elixhauser_condition_names <- names(
    elixhauser_configuration$diagnosis_code_mappings$ICD10CM
  )
  # 31 Elixhauser conditions plus hospitalization_id and elix_score.
  expect_equal(length(elixhauser_condition_names), 31L)
  expect_identical(
    names(clifr_elix_scores),
    c("hospitalization_id", elixhauser_condition_names, "elix_score")
  )
})

test_that("comorbidity functions accept a HospitalDiagnosis table object", {
  skip_without_comorbidity_baselines()

  hospital_diagnosis_data <- load_cohort_hospital_diagnosis()
  hospital_diagnosis_table <- HospitalDiagnosis$new()
  hospital_diagnosis_table$df <- hospital_diagnosis_data

  expect_identical(
    calculate_cci(hospital_diagnosis_table),
    calculate_cci(hospital_diagnosis_data)
  )
  expect_identical(
    calculate_elix(hospital_diagnosis_table),
    calculate_elix(hospital_diagnosis_data)
  )
})

test_that("hierarchy logic zeroes the milder form of a condition", {
  # Both diabetes forms and both liver-disease forms present for the same stay.
  diagnosis_data <- dplyr::tibble(
    hospitalization_id = c("H1", "H1", "H1", "H1", "H2"),
    diagnosis_code = c("E11.9", "E11.22", "K70.30", "K70.4", "C78.00"),
    diagnosis_code_format = c("ICD10CM", "icd10cm", "ICD10CM", "ICD10CM", "Icd10Cm")
  )

  with_hierarchy <- calculate_cci(diagnosis_data, hierarchy = TRUE)
  without_hierarchy <- calculate_cci(diagnosis_data, hierarchy = FALSE)

  first_stay_with <- with_hierarchy[with_hierarchy$hospitalization_id == "H1", ]
  first_stay_without <- without_hierarchy[without_hierarchy$hospitalization_id == "H1", ]

  expect_identical(first_stay_without$diabetes_uncomplicated, 1L)
  expect_identical(first_stay_without$diabetes_with_complications, 1L)
  expect_identical(first_stay_with$diabetes_uncomplicated, 0L)
  expect_identical(first_stay_with$diabetes_with_complications, 1L)

  expect_identical(first_stay_without$mild_liver_disease, 1L)
  expect_identical(first_stay_with$mild_liver_disease, 0L)
  expect_identical(first_stay_with$moderate_severe_liver_disease, 1L)
})

test_that("non-ICD10CM diagnosis rows are discarded", {
  diagnosis_data <- dplyr::tibble(
    hospitalization_id = c("H1", "H2"),
    diagnosis_code = c("I21.45", "410.9"),
    diagnosis_code_format = c("ICD10CM", "ICD9CM")
  )

  cci_scores <- calculate_cci(diagnosis_data)

  # The ICD-9 stay contributes no rows and so is absent from the output.
  expect_identical(cci_scores$hospitalization_id, "H1")
  expect_identical(cci_scores$myocardial_infarction, 1L)
})

test_that("comorbidity functions reject malformed input", {
  expect_error(calculate_cci(list(not_a_data_frame = TRUE)))
  expect_error(
    calculate_cci(dplyr::tibble(hospitalization_id = "H1", diagnosis_code = "I21"))
  )
})
