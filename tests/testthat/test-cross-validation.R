#' Cross-Language Validation Tests
#'
#' Compare R (clifR) outputs to Python (clifpy) baselines
#' to ensure cross-language compatibility.

# Setup
test_data_dir <- "tests/fixtures/synthetic_data"
baseline_dir <- "tests/baseline"

# Skip if synthetic data not yet generated
skip_if_no_data <- function() {
  if (!dir.exists(test_data_dir) || length(list.files(test_data_dir)) == 0) {
    skip("Synthetic data not generated. Run tests/fixtures/generate_synthetic_data.R first")
  }
}

# Skip if Python baselines not yet generated
skip_if_no_baseline <- function(baseline_file) {
  if (!file.exists(baseline_file)) {
    skip(sprintf("Python baseline not found: %s. Run tests/generate_baselines.py first", basename(baseline_file)))
  }
}

# ===========================================================================
# VALIDATION TESTS
# ===========================================================================

test_that("Patient validation matches Python", {
  skip_if_no_data()

  # Load patient data with R
  patient_r <- Patient$new(
    data_directory = test_data_dir,
    filetype = "csv",
    timezone = "America/New_York"
  )

  # Validate in R
  validation_r <- patient_r$validate(verbose = FALSE)

  # Compare to Python baseline
  baseline_file <- file.path(baseline_dir, "patient_validation_python.json")
  skip_if_no_baseline(baseline_file)

  comparison <- compare_validation_results(validation_r, baseline_file)

  expect_true(comparison$n_rows, label = "Row count matches")
  expect_true(comparison$n_cols, label = "Column count matches")
  expect_true(comparison$is_valid, label = "Validation status matches")
})

test_that("Hospitalization validation matches Python", {
  skip_if_no_data()

  hosp_r <- Hospitalization$new(
    data_directory = test_data_dir,
    filetype = "csv",
    timezone = "America/New_York"
  )

  validation_r <- hosp_r$validate(verbose = FALSE)

  baseline_file <- file.path(baseline_dir, "hospitalization_validation_python.json")
  skip_if_no_baseline(baseline_file)

  comparison <- compare_validation_results(validation_r, baseline_file)

  expect_true(comparison$n_rows)
  expect_true(comparison$n_cols)
})

# ===========================================================================
# SUMMARY STATISTICS TESTS
# ===========================================================================

test_that("Hospitalization summary stats match Python", {
  skip_if_no_data()

  hosp_r <- Hospitalization$new(
    data_directory = test_data_dir,
    filetype = "csv",
    timezone = "America/New_York"
  )

  # Calculate summary stats in R
  los_data <- hosp_r$calculate_length_of_stay(units = "days")

  summary_r <- list(
    n_hospitalizations = nrow(hosp_r$df),
    los_mean = mean(los_data$length_of_stay, na.rm = TRUE),
    los_median = median(los_data$length_of_stay, na.rm = TRUE),
    los_std = sd(los_data$length_of_stay, na.rm = TRUE),
    age_mean = mean(hosp_r$df$age_at_admission, na.rm = TRUE),
    age_median = median(hosp_r$df$age_at_admission, na.rm = TRUE)
  )

  # Compare to Python
  baseline_file <- file.path(baseline_dir, "hospitalization_summary_python.json")
  skip_if_no_baseline(baseline_file)

  comparison <- compare_summary_stats(
    summary_r,
    baseline_file,
    tolerance = 1e-10  # Strict tolerance for summary stats
  )

  # Check key metrics
  expect_true(comparison$los_mean$match, label = "LOS mean matches")
  expect_true(comparison$age_mean$match, label = "Age mean matches")
})

test_that("Vitals summary stats match Python", {
  skip_if_no_data()

  vitals_r <- Vitals$new(
    data_directory = test_data_dir,
    filetype = "csv",
    timezone = "America/New_York"
  )

  # Get summary for each vital category
  vital_categories <- unique(vitals_r$df$vital_category)

  baseline_file <- file.path(baseline_dir, "vitals_summary_python.json")
  skip_if_no_baseline(baseline_file)

  python_summary <- jsonlite::read_json(baseline_file)

  for (vital_cat in vital_categories) {
    vital_data <- vitals_r$filter_by_category(vital_cat)

    r_summary <- list(
      n = nrow(vital_data),
      mean = mean(vital_data$vital_value, na.rm = TRUE),
      median = median(vital_data$vital_value, na.rm = TRUE),
      std = sd(vital_data$vital_value, na.rm = TRUE)
    )

    if (vital_cat %in% names(python_summary)) {
      python_cat_summary <- python_summary[[vital_cat]]

      # Compare means (most important)
      diff <- abs(r_summary$mean - python_cat_summary$mean)
      expect_lt(diff, 1e-10, label = sprintf("%s mean matches", vital_cat))
    }
  }
})

# ===========================================================================
# FUTURE TESTS (to be implemented as features are added)
# ===========================================================================

# test_that("SOFA scores match Python", {
#   skip("SOFA score calculation not yet implemented")
#
#   # Will compare:
#   # - Individual component scores (exact match)
#   # - Total SOFA scores (exact match)
#   # - Score distributions
# })

# test_that("Wide dataset matches Python", {
#   skip("Wide dataset transformation not yet implemented")
#
#   # Will compare:
#   # - Dimensions (exact match)
#   # - Column names (exact match)
#   # - Values (tolerance 1e-12)
# })

# test_that("Encounter stitching matches Python", {
#   skip("Encounter stitching not yet implemented")
#
#   # Will compare:
#   # - Stitched IDs (exact match)
#   # - Groupings (exact match)
# })

# test_that("Unit conversions match Python", {
#   skip("Unit converter not yet implemented")
#
#   # Will compare:
#   # - Converted values (exact match for deterministic conversions)
# })

# test_that("CCI calculations match Python", {
#   skip("CCI calculation not yet implemented")
#
#   # Will compare:
#   # - CCI scores (exact match - integer)
# })
