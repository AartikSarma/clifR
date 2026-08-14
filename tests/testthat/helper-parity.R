# Shared infrastructure for cross-language parity tests.
#
# Every parity test runs a clifR function on the fixture cohort in
# tests/fixtures/cohort/ and compares the result against the artifact clifpy wrote
# for the same input, under tests/baseline/. Regenerate the baselines with:
#
#     cd tests/baseline_py && uv run python generate_baselines.py
#
# The fixture is CLIF 3.0 data in a fixed timezone; both sides must use the same
# settings or the comparison is meaningless.

PARITY_CLIF_VERSION <- "3.0"
PARITY_TIMEZONE <- "US/Central"
PARITY_FILETYPE <- "parquet"

# Tolerances by result kind. Integer clinical scores get no budget at all: a
# one-point SOFA difference is a real disagreement, not a rounding artifact.
PARITY_TOLERANCE_EXACT <- 0
PARITY_TOLERANCE_STRICT <- 1e-12
PARITY_TOLERANCE_MODERATE <- 1e-6

#' Locate the test fixture and baseline directories
#'
#' testthat runs with the working directory set to tests/testthat, but the same
#' helpers are useful from the project root during interactive development, so the
#' path is resolved by searching upward for the known directory.
parity_project_root <- function() {
  candidate_directories <- c(".", "..", "../..", "../../..")
  for (candidate in candidate_directories) {
    if (dir.exists(file.path(candidate, "tests", "fixtures", "cohort"))) {
      return(normalizePath(candidate))
    }
  }
  NA_character_
}

parity_fixture_dir <- function() {
  project_root <- parity_project_root()
  if (is.na(project_root)) return(NA_character_)
  file.path(project_root, "tests", "fixtures", "cohort")
}

parity_baseline_dir <- function() {
  project_root <- parity_project_root()
  if (is.na(project_root)) return(NA_character_)
  file.path(project_root, "tests", "baseline")
}

#' Skip a test when the fixture or a specific baseline artifact is unavailable
#'
#' The fixture is large and gitignored, so a fresh checkout will not have it until
#' build_cohort.py has been run. Skipping keeps `R CMD check` green in that case
#' rather than reporting spurious failures.
skip_if_no_parity_fixture <- function(artifact_name = NULL) {
  fixture_directory <- parity_fixture_dir()
  if (is.na(fixture_directory) || !dir.exists(fixture_directory)) {
    testthat::skip("Parity fixture not built; run tests/baseline_py/build_cohort.py")
  }
  if (!is.null(artifact_name)) {
    if (!file.exists(parity_baseline_path(artifact_name))) {
      testthat::skip(sprintf(
        "Baseline artifact '%s' not generated; run tests/baseline_py/generate_baselines.py",
        artifact_name
      ))
    }
  }
  invisible(TRUE)
}

#' Path to a baseline artifact, inferring the extension
parity_baseline_path <- function(artifact_name) {
  baseline_directory <- parity_baseline_dir()
  if (is.na(baseline_directory)) return(NA_character_)

  if (grepl("\\.(parquet|json)$", artifact_name)) {
    return(file.path(baseline_directory, artifact_name))
  }
  parquet_path <- file.path(baseline_directory, paste0(artifact_name, ".parquet"))
  if (file.exists(parquet_path)) {
    return(parquet_path)
  }
  file.path(baseline_directory, paste0(artifact_name, ".json"))
}

#' Read a clifpy baseline artifact
read_parity_baseline <- function(artifact_name) {
  artifact_path <- parity_baseline_path(artifact_name)
  if (grepl("\\.json$", artifact_path)) {
    return(jsonlite::fromJSON(artifact_path, simplifyVector = FALSE))
  }
  dplyr::as_tibble(arrow::read_parquet(artifact_path))
}

#' A stable scratch directory for test output artifacts
#'
#' `withr::local_tempdir()` cleans up when the *calling* frame exits, so calling it
#' inside a helper deletes the directory the moment the helper returns — before the
#' table's own methods (validate(), which writes an errors CSV) ever run. A single
#' session-scoped directory avoids that lifetime trap and is cleaned up by the OS.
parity_output_dir <- function() {
  scratch_directory <- file.path(tempdir(), "clifR-parity-output")
  if (!dir.exists(scratch_directory)) {
    dir.create(scratch_directory, recursive = TRUE, showWarnings = FALSE)
  }
  scratch_directory
}

#' Build an orchestrator pointed at the fixture cohort
parity_orchestrator <- function(...) {
  ClifOrchestrator$new(
    data_directory = parity_fixture_dir(),
    filetype = PARITY_FILETYPE,
    timezone = PARITY_TIMEZONE,
    output_directory = parity_output_dir(),
    clif_version = PARITY_CLIF_VERSION,
    ...
  )
}

#' Load one fixture table as a clifR table object
parity_table <- function(table_name) {
  clif_table_from_file(
    table_name = table_name,
    data_directory = parity_fixture_dir(),
    filetype = PARITY_FILETYPE,
    timezone = PARITY_TIMEZONE,
    output_directory = parity_output_dir(),
    clif_version = PARITY_CLIF_VERSION
  )
}

#' Normalize a frame so two implementations can be compared row for row
#'
#' Sorts rows by the given key columns, orders columns alphabetically, and drops
#' row names. Both sides go through this before any value comparison, so a
#' difference in natural ordering never masquerades as a difference in values.
normalize_for_comparison <- function(data, sort_columns = NULL) {
  data <- dplyr::as_tibble(data)

  if (!is.null(sort_columns)) {
    present_sort_columns <- intersect(sort_columns, names(data))
    if (length(present_sort_columns) > 0) {
      data <- dplyr::arrange(data, dplyr::across(dplyr::all_of(present_sort_columns)))
    }
  }

  data[, sort(names(data)), drop = FALSE]
}

#' Compare the shape and column names of two frames
expect_same_shape <- function(r_result, python_result, label = "result") {
  testthat::expect_equal(
    nrow(r_result), nrow(python_result),
    info = sprintf("%s: row count differs from clifpy", label)
  )
  testthat::expect_setequal(names(r_result), names(python_result))
}

#' Compare every shared column of two normalized frames
#'
#' Numeric columns are compared with `tolerance`; everything else must match after
#' coercion to character, which sidesteps factor-versus-character and integer-versus-
#' double representation differences that carry no semantic weight.
expect_columns_match <- function(r_result,
                                 python_result,
                                 tolerance = PARITY_TOLERANCE_STRICT,
                                 ignore_columns = character(0),
                                 label = "result") {
  shared_columns <- setdiff(
    intersect(names(r_result), names(python_result)),
    ignore_columns
  )
  testthat::expect_gt(length(shared_columns), 0)

  for (column_name in shared_columns) {
    r_values <- r_result[[column_name]]
    python_values <- python_result[[column_name]]

    if (is.numeric(r_values) && is.numeric(python_values)) {
      # Compare as doubles rather than requiring identical storage types. R and
      # pandas make different integer-versus-double choices for the same quantity
      # (a count is integer in pandas, double in R), which carries no semantic
      # weight; integers are represented exactly as doubles at these magnitudes,
      # so an exact comparison stays exact.
      testthat::expect_equal(
        as.numeric(r_values), as.numeric(python_values),
        tolerance = if (tolerance == 0) NULL else tolerance,
        info = sprintf("%s: numeric column '%s' differs from clifpy", label, column_name)
      )
    } else if (inherits(r_values, "POSIXct") || inherits(python_values, "POSIXct")) {
      testthat::expect_equal(
        as.numeric(as.POSIXct(r_values, tz = "UTC")),
        as.numeric(as.POSIXct(python_values, tz = "UTC")),
        info = sprintf("%s: datetime column '%s' differs from clifpy", label, column_name)
      )
    } else {
      testthat::expect_equal(
        as.character(r_values), as.character(python_values),
        info = sprintf("%s: column '%s' differs from clifpy", label, column_name)
      )
    }
  }
  invisible(TRUE)
}

#' Full frame comparison: shape, then values
expect_parity <- function(r_result,
                          python_result,
                          sort_columns = NULL,
                          tolerance = PARITY_TOLERANCE_STRICT,
                          ignore_columns = character(0),
                          label = "result") {
  r_normalized <- normalize_for_comparison(r_result, sort_columns)
  python_normalized <- normalize_for_comparison(python_result, sort_columns)

  expect_same_shape(r_normalized, python_normalized, label)
  expect_columns_match(r_normalized, python_normalized, tolerance, ignore_columns, label)
  invisible(TRUE)
}

#' Summarize how far apart two numeric vectors are
#'
#' Used in failure messages so a report says how badly a column disagrees rather
#' than only that it does.
numeric_difference_summary <- function(r_values, python_values) {
  comparable <- !is.na(r_values) & !is.na(python_values)
  if (!any(comparable)) {
    return(list(n_compared = 0, max_abs_diff = NA_real_, n_mismatched_na = sum(is.na(r_values) != is.na(python_values))))
  }
  absolute_differences <- abs(r_values[comparable] - python_values[comparable])
  list(
    n_compared = sum(comparable),
    max_abs_diff = max(absolute_differences),
    mean_abs_diff = mean(absolute_differences),
    n_mismatched_na = sum(is.na(r_values) != is.na(python_values))
  )
}
