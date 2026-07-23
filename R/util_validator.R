#' Data Quality Assessment (DQA) engine
#'
#' Port of `clifpy/utils/validator.py` (clifpy 0.5.0). Implements the three
#' DQA families clifpy defines:
#'
#' * **Conformance** — table presence, required columns, dtypes, datetime
#'   format, lab reference units, categorical values, category-group mapping.
#' * **Completeness** — missingness, conditional requirements, mCIDE value
#'   coverage, relational integrity.
#' * **Plausibility** — chronological order, numeric ranges, field rules,
#'   medication dose units, cross-table temporal bounds, overlapping periods,
#'   category temporal consistency, duplicate composite keys.
#'
#' clifpy ships a Polars and a DuckDB backend that produce identical messages;
#' only one implementation is ported here. All rule content is read from
#' `validation_rules.yaml` and `outlier_config.yaml` via [load_shared_config()]
#' rather than being restated in R, so the two implementations cannot drift.
#'
#' @section Case normalization:
#' clifpy lowercases column *names* and lowercases + trims every string
#' *value* before validating, keeping a `__orig_<col>` sidecar so error
#' payloads still cite what the site actually submitted. [normalize_for_validation()]
#' reproduces that, and every check that compares strings depends on it.
#'
#' @name clif-validator
NULL

# Sidecar column prefix preserving the pre-normalization (original-case) value
# of every string column during validation.
ORIG_PREFIX <- "__orig_"

# Default plausibility thresholds: list(check_name = list(error_threshold, warning_threshold)).
# warning_threshold: percent above which a warning is raised.
# error_threshold: percent above which an error is raised.
DEFAULT_PLAUSIBILITY_THRESHOLDS <- list(
  chronological_order = list(error_threshold = 10.0, warning_threshold = 0.0),
  numeric_range_plausibility = list(error_threshold = 10.0, warning_threshold = 0.0),
  medication_dose_unit_consistency = list(error_threshold = 10.0, warning_threshold = 0.0),
  cross_table_temporal = list(error_threshold = 10.0, warning_threshold = 0.0),
  duplicate_composite_keys = list(error_threshold = 10.0, warning_threshold = 0.0)
)

# Category column mapping for numeric range checks. A length-2 character
# vector means (category_col, unit_col) — the two-level form.
CATEGORY_COLUMN_MAP <- list(
  labs = list(lab_value_numeric = "lab_category"),
  vitals = list(vital_value = "vital_category"),
  patient_assessments = list(numerical_value = "assessment_category"),
  medication_admin_continuous = list(med_dose = c("med_category", "med_dose_unit")),
  medication_admin_intermittent = list(med_dose = c("med_category", "med_dose_unit")),
  ecmo_mcs = list(sweep = "device_category", flow = "device_category", fdO2 = "device_category")
)

# Maps (table_name, category_col) -> unit column present in the data.
CATEGORY_UNIT_COL_MAP <- list(
  `labs|lab_category` = "reference_unit",
  `medication_admin_continuous|med_category` = "med_dose_unit",
  `medication_admin_intermittent|med_category` = "med_dose_unit"
)

# Datetime columns per table for cross-table temporal checks.
CROSS_TABLE_TIME_COLUMNS <- list(
  adt = c("in_dttm", "out_dttm"),
  labs = c("lab_order_dttm", "lab_collect_dttm", "lab_result_dttm"),
  vitals = "recorded_dttm",
  respiratory_support = "recorded_dttm",
  medication_admin_continuous = "admin_dttm",
  medication_admin_intermittent = "admin_dttm",
  patient_assessments = "recorded_dttm",
  position = "recorded_dttm",
  microbiology_culture = c("order_dttm", "collect_dttm", "result_dttm"),
  microbiology_nonculture = c("order_dttm", "collect_dttm", "result_dttm"),
  crrt_therapy = "recorded_dttm",
  ecmo_mcs = "recorded_dttm"
)

# Time-denominator patterns for medication dose unit checks.
TIME_DENOMINATOR_PATTERNS <- c("/sec", "/min", "/hr", "/hour", "/day")


# ---------------------------------------------------------------------------
# Formatting helpers (Python-compatible string rendering)
# ---------------------------------------------------------------------------

# Render a number the way Python's str(float) does for values already rounded
# to two decimals: 50.0 -> "50.0", 66.67 -> "66.67", 17.8 -> "17.8".
py_num_str <- function(value) {
  if (length(value) != 1 || is.na(value)) {
    return("nan")
  }
  if (value == round(value)) {
    return(sprintf("%.0f.0", value))
  }
  rendered <- sprintf("%.2f", value)
  sub("0$", "", rendered)
}

# Render an integer with thousands separators, matching Python's f"{n:,}".
py_int_comma <- function(value) {
  formatC(as.numeric(value), format = "d", big.mark = ",")
}

# Render an integer without separators, matching Python's str(int).
py_int_str <- function(value) {
  formatC(as.numeric(value), format = "d")
}

# Python-style percentage with one decimal, matching f"{pct:.1f}".
py_pct1 <- function(value) {
  sprintf("%.1f", value)
}

# Python's round() uses round-half-to-even, as does R's round().
py_round <- function(value, digits = 2) {
  round(value, digits)
}


# ---------------------------------------------------------------------------
# Configuration loading
# ---------------------------------------------------------------------------

# Cached read of validation_rules.yaml. Returns an empty list when absent.
load_validation_rules <- function() {
  tryCatch(load_shared_config("validation_rules.yaml"), error = function(condition) list())
}

# Read of outlier_config.yaml for the validator's numeric-range checks. Named
# distinctly from outlier_handler's load_outlier_config() so the two internal
# helpers do not collide when the package sources every R file into one namespace
# (they have different signatures, and whichever sourced last would otherwise win).
# Returns an empty list when absent.
load_validator_outlier_config <- function() {
  tryCatch(load_shared_config("outlier_config.yaml"), error = function(condition) list())
}

get_default_conditions <- function(table_name) {
  load_validation_rules()$conditional_requirements[[table_name]] %||% list()
}

get_chronological_order_rules <- function(table_name) {
  load_validation_rules()$chronological_order[[table_name]] %||% list()
}

get_field_plausibility_rules <- function(table_name) {
  load_validation_rules()$field_plausibility_rules[[table_name]] %||% list()
}

get_composite_keys <- function(table_name, schema = NULL) {
  if (!is.null(schema) && !is.null(schema$composite_keys)) {
    return(unlist(schema$composite_keys, use.names = FALSE))
  }
  entry <- load_validation_rules()$composite_keys[[table_name]] %||% list()
  unlist(entry$keys %||% character(0), use.names = FALSE)
}


# ---------------------------------------------------------------------------
# Case normalization
# ---------------------------------------------------------------------------

#' Case-normalize a data frame for DQA validation
#'
#' Lowercases column names, lowercases and trims every character column, and
#' attaches a `__orig_<col>` sidecar preserving the original value for error
#' reporting. Safe to call repeatedly — a frame that already carries sidecars
#' is returned unchanged. Port of clifpy's `_normalize_for_validation`.
#'
#' Scope is validation only; callers elsewhere keep case-sensitive semantics.
#'
#' @param data A data frame.
#' @return The normalized data frame, as a tibble.
#' @export
#'
#' @examples
#' normalize_for_validation(data.frame(Race_Category = " White "))
normalize_for_validation <- function(data) {
  if (any(startsWith(names(data), ORIG_PREFIX))) {
    return(data)
  }
  normalized <- data
  names(normalized) <- tolower(names(normalized))

  character_columns <- names(normalized)[vapply(normalized, is.character, logical(1))]
  for (column_name in character_columns) {
    column_values <- normalized[[column_name]]
    normalized[[paste0(ORIG_PREFIX, column_name)]] <- column_values
    normalized[[column_name]] <- trimws(tolower(column_values))
  }
  dplyr::as_tibble(normalized)
}

# Drop __orig_* sidecar columns from a column-name vector, so presence and
# dtype checks do not see them in actual-column counts.
strip_sidecars <- function(column_names) {
  column_names[!startsWith(column_names, ORIG_PREFIX)]
}


# ---------------------------------------------------------------------------
# Data-type description
# ---------------------------------------------------------------------------

#' Describe an R column the way clifpy's Polars backend does
#'
#' clifpy renders the observed dtype into its error messages using Polars'
#' `repr`, e.g. `Datetime(time_unit='us', time_zone=None)`. This function
#' produces the equivalent string for an R vector so error descriptions match
#' verbatim.
#'
#' @section Timezone approximation:
#' R stores a single `tzone` attribute per POSIXct vector and the DuckDB reader
#' labels every timestamp `UTC`, so a naive source column is indistinguishable
#' from a UTC-aware one once loaded. CLIF stores every timezone-aware timestamp
#' in a `*_dttm` column, so a POSIXct column whose name lacks `dttm` is
#' reported as timezone-naive, matching what clifpy sees.
#'
#' @param column_values A vector.
#' @param column_name The column's name, used for the timezone heuristic.
#' @return A single dtype string.
#' @keywords internal
polars_dtype_string <- function(column_values, column_name = "") {
  if (inherits(column_values, "POSIXct")) {
    timezone_attribute <- attr(column_values, "tzone")
    is_timezone_aware <- grepl("dttm", column_name, fixed = TRUE) &&
      !is.null(timezone_attribute) && !identical(timezone_attribute, "")
    timezone_repr <- if (is_timezone_aware) sprintf("'%s'", timezone_attribute) else "None"
    return(sprintf("Datetime(time_unit='us', time_zone=%s)", timezone_repr))
  }
  if (inherits(column_values, "Date")) {
    return("Date")
  }
  if (inherits(column_values, "difftime")) {
    return("Duration(time_unit='us')")
  }
  if (is.factor(column_values)) {
    return("Categorical(ordering='physical')")
  }
  if (is.character(column_values)) {
    return("String")
  }
  if (is.logical(column_values)) {
    return("Boolean")
  }
  if (inherits(column_values, "integer64")) {
    return("Int64")
  }
  if (is.integer(column_values)) {
    return("Int32")
  }
  if (is.numeric(column_values)) {
    return("Float64")
  }
  "Object"
}

# Whether an R column satisfies an mCIDE data_type, using the same families
# clifpy's type_mapping defines.
#
# The DuckDB R driver materializes 64-bit integers as doubles, so a column that
# Polars would report as Int64 arrives here as a fraction-free double. Treating
# whole-valued doubles as integers keeps INTEGER columns matching, without
# disturbing FLOAT columns, which still match their own family.
dtype_matches_expected <- function(column_values, expected_type) {
  switch(expected_type,
    VARCHAR = is.character(column_values) || is.factor(column_values),
    DATETIME = inherits(column_values, "POSIXct"),
    DATE = inherits(column_values, "Date"),
    INTEGER = ,
    INT = is.integer(column_values) || inherits(column_values, "integer64") ||
      is_whole_valued_double(column_values),
    FLOAT = ,
    DOUBLE = is.numeric(column_values) && !is.integer(column_values),
    FALSE
  )
}

is_whole_valued_double <- function(column_values) {
  if (!is.double(column_values) || inherits(column_values, "POSIXct") || inherits(column_values, "Date")) {
    return(FALSE)
  }
  non_missing_values <- column_values[!is.na(column_values)]
  length(non_missing_values) > 0 && all(non_missing_values == trunc(non_missing_values))
}

# Whether the first 100 non-null values can be cast to the target type.
# Mirrors clifpy's `_check_castable_polars`: VARCHAR always succeeds, DATE is
# never castable, and the numeric/datetime casts are strict.
is_castable_to <- function(column_values, target_type) {
  sample_values <- utils::head(column_values[!is.na(column_values)], 100)
  if (length(sample_values) == 0) {
    return(target_type %in% c("INTEGER", "INT", "FLOAT", "DOUBLE", "DATETIME", "VARCHAR"))
  }
  tryCatch(
    switch(target_type,
      INTEGER = ,
      INT = all(!is.na(suppressWarnings(as.integer(as.character(sample_values))))),
      FLOAT = ,
      DOUBLE = all(!is.na(suppressWarnings(as.numeric(as.character(sample_values))))),
      DATETIME = {
        if (inherits(sample_values, "POSIXct") || inherits(sample_values, "Date")) {
          TRUE
        } else {
          all(!is.na(suppressWarnings(as.POSIXct(as.character(sample_values), tz = "UTC"))))
        }
      },
      VARCHAR = TRUE,
      FALSE
    ),
    error = function(condition) FALSE
  )
}


# ---------------------------------------------------------------------------
# Result containers
# ---------------------------------------------------------------------------

new_dqa_result <- function(check_type, table_name, result_class) {
  structure(
    list(
      check_type = check_type,
      table_name = table_name,
      passed = TRUE,
      errors = list(),
      warnings = list(),
      info = list(),
      metrics = list(),
      # Atomic-granularity scoring. When a check examines N atomic units but
      # rolls up to fewer messages, it sets these so downstream scores reflect
      # real work done rather than message count. NULL means "fall back to
      # message count".
      atomic_total = NULL,
      atomic_passed = NULL
    ),
    class = c(result_class, "dqa_result", "list")
  )
}

#' Create a DQA conformance result container
#'
#' @param check_type Name of the check, e.g. `"required_columns"`.
#' @param table_name Name of the table being checked.
#' @return A `dqa_conformance_result` object (a named list).
#' @export
#'
#' @examples
#' dqa_conformance_result("required_columns", "patient")
dqa_conformance_result <- function(check_type, table_name) {
  new_dqa_result(check_type, table_name, "dqa_conformance_result")
}

#' Create a DQA completeness result container
#'
#' @inheritParams dqa_conformance_result
#' @return A `dqa_completeness_result` object (a named list).
#' @export
#'
#' @examples
#' dqa_completeness_result("missingness", "patient")
dqa_completeness_result <- function(check_type, table_name) {
  new_dqa_result(check_type, table_name, "dqa_completeness_result")
}

#' Create a DQA plausibility result container
#'
#' @inheritParams dqa_conformance_result
#' @return A `dqa_plausibility_result` object (a named list).
#' @export
#'
#' @examples
#' dqa_plausibility_result("chronological_order", "adt")
dqa_plausibility_result <- function(check_type, table_name) {
  new_dqa_result(check_type, table_name, "dqa_plausibility_result")
}

#' Record an error on a DQA result
#'
#' @param result A DQA result container.
#' @param message Human-readable error message.
#' @param details Optional named list of structured detail fields.
#' @return The result with the error appended and `passed` set to `FALSE`.
#' @export
#'
#' @examples
#' add_error(dqa_conformance_result("required_columns", "patient"),
#'           "Column 'patient_id': missing from data")
add_error <- function(result, message, details = NULL) {
  result$passed <- FALSE
  result$errors <- c(result$errors, list(list(message = message, details = details %||% list())))
  result
}

#' Record a warning on a DQA result
#'
#' @inheritParams add_error
#' @return The result with the warning appended.
#' @export
#'
#' @examples
#' add_warning(dqa_conformance_result("categorical_values", "patient"),
#'             "2 invalid categorical values")
add_warning <- function(result, message, details = NULL) {
  result$warnings <- c(result$warnings, list(list(message = message, details = details %||% list())))
  result
}

#' Record an informational message on a DQA result
#'
#' @inheritParams add_error
#' @return The result with the info message appended.
#' @export
#'
#' @examples
#' add_info(dqa_conformance_result("required_columns", "patient"),
#'          "Column 'patient_id': present")
add_info <- function(result, message, details = NULL) {
  result$info <- c(result$info, list(list(message = message, details = details %||% list())))
  result
}

#' Convert a DQA result to a plain named list
#'
#' Matches the key set clifpy's `to_dict()` produces, so serialized output is
#' directly comparable across the two implementations.
#'
#' @param result A DQA result container.
#' @return A named list with `check_type`, `table_name`, `passed`, `errors`,
#'   `warnings`, `info`, `metrics`, `atomic_total` and `atomic_passed`.
#' @export
#'
#' @examples
#' to_list(dqa_conformance_result("required_columns", "patient"))
to_list <- function(result) {
  list(
    check_type = result$check_type,
    table_name = result$table_name,
    passed = result$passed,
    errors = result$errors,
    warnings = result$warnings,
    info = result$info,
    metrics = result$metrics,
    atomic_total = result$atomic_total,
    atomic_passed = result$atomic_passed
  )
}


# ---------------------------------------------------------------------------
# CONFORMANCE CHECKS
# ---------------------------------------------------------------------------

#' C.1 Check that a table file exists on disk
#'
#' @param table_path Directory containing the table files.
#' @param table_name Name of the table to check.
#' @param filetype File extension, e.g. `"parquet"`.
#' @return A `dqa_conformance_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_table_exists("data/clif", "patient", "parquet")
#' }
check_table_exists <- function(table_path, table_name, filetype = "parquet") {
  result <- dqa_conformance_result("table_exists", table_name)
  expected_file <- file.path(table_path, paste0(table_name, ".", filetype))

  if (file.exists(expected_file)) {
    result <- add_info(result, sprintf("Table file found: %s", expected_file))
    result$metrics$file_path <- expected_file
    result$metrics$file_size_mb <- file.info(expected_file)$size / (1024 * 1024)
  } else {
    result <- add_error(
      result,
      sprintf("Table file not found: %s", expected_file),
      list(expected_path = expected_file)
    )
  }

  result$atomic_total <- 1L
  result$atomic_passed <- if (length(result$errors) == 0) 1L else 0L
  result
}

#' C.1b Check that a loaded table has rows and columns
#'
#' @param df Data to validate.
#' @param table_name Name of the table.
#' @return A `dqa_conformance_result`.
#' @export
#'
#' @examples
#' check_table_presence(data.frame(a = 1), "patient")
check_table_presence <- function(df, table_name) {
  result <- dqa_conformance_result("table_presence", table_name)

  row_count <- nrow(df)
  column_count <- length(strip_sidecars(names(df)))

  result$metrics$row_count <- row_count
  result$metrics$column_count <- column_count

  if (column_count == 0) {
    result <- add_error(
      result,
      sprintf("Table '%s' has no columns", table_name),
      list(column_count = column_count)
    )
  }
  if (row_count == 0) {
    result <- add_error(
      result,
      sprintf("Table '%s' has 0 rows", table_name),
      list(row_count = row_count)
    )
  }
  if (result$passed) {
    result <- add_info(result, sprintf(
      "Table '%s' present with %d rows and %d columns",
      table_name, row_count, column_count
    ))
  }

  result$atomic_total <- 1L
  result$atomic_passed <- if (length(result$errors) == 0) 1L else 0L
  result
}

#' C.2 Check that all schema-required columns are present
#'
#' @param df Data to validate.
#' @param schema Parsed table schema containing `required_columns`.
#' @param table_name Name of the table.
#' @return A `dqa_conformance_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_required_columns(patient_data, load_schema("patient"), "patient")
#' }
check_required_columns <- function(df, schema, table_name) {
  result <- dqa_conformance_result("required_columns", table_name)

  actual_columns <- names(df)
  required_columns <- unlist(schema$required_columns %||% character(0), use.names = FALSE)
  missing_columns <- setdiff(required_columns, actual_columns)

  result$metrics$total_required <- length(required_columns)
  result$metrics$total_present <- length(required_columns) - length(missing_columns)
  result$metrics$total_missing <- length(missing_columns)

  for (column_name in required_columns) {
    if (column_name %in% missing_columns) {
      result <- add_error(
        result,
        sprintf("Column '%s': missing from data", column_name),
        list(column = column_name)
      )
    } else {
      result <- add_info(
        result,
        sprintf("Column '%s': present", column_name),
        list(column = column_name)
      )
    }
  }

  result$atomic_total <- length(required_columns)
  result$atomic_passed <- length(required_columns) - length(missing_columns)
  result
}

#' C.3 Check that columns have the schema-declared data types
#'
#' Columns whose observed type differs but can be cast produce a warning;
#' non-castable mismatches produce an error. All-null columns are skipped,
#' since type inference is unreliable without data.
#'
#' @inheritParams check_required_columns
#' @return A `dqa_conformance_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_column_dtypes(patient_data, load_schema("patient"), "patient")
#' }
check_column_dtypes <- function(df, schema, table_name) {
  result <- dqa_conformance_result("column_dtypes", table_name)

  column_specs <- schema$columns %||% list()
  dtype_errors <- list()
  dtype_warnings <- list()

  for (column_spec in column_specs) {
    column_name <- column_spec$name
    expected_type <- column_spec$data_type
    if (is.null(expected_type) || !column_name %in% names(df)) {
      next
    }

    column_values <- df[[column_name]]
    if (dtype_matches_expected(column_values, expected_type)) {
      next
    }

    actual_dtype <- polars_dtype_string(column_values, column_name)

    # Polars' loose string checks, reproduced verbatim.
    if (expected_type == "DATETIME" && grepl("Datetime", actual_dtype, fixed = TRUE)) {
      next
    }
    if (expected_type == "VARCHAR" &&
        (grepl("Utf8", actual_dtype, fixed = TRUE) || grepl("String", actual_dtype, fixed = TRUE))) {
      next
    }
    if (all(is.na(column_values))) {
      next
    }

    mismatch_record <- list(
      column = column_name,
      expected = expected_type,
      actual = actual_dtype,
      castable = is_castable_to(column_values, expected_type)
    )
    if (isTRUE(mismatch_record$castable)) {
      dtype_warnings <- c(dtype_warnings, list(mismatch_record))
    } else {
      dtype_errors <- c(dtype_errors, list(mismatch_record))
    }
  }

  result$metrics$columns_checked <- length(column_specs)
  result$metrics$dtype_errors <- length(dtype_errors)
  result$metrics$dtype_warnings <- length(dtype_warnings)

  columns_with_issues <- c(
    vapply(dtype_errors, function(record) record$column, character(1)),
    vapply(dtype_warnings, function(record) record$column, character(1))
  )

  for (record in dtype_errors) {
    result <- add_error(result, sprintf(
      "Column '%s' has type %s, cannot cast to %s",
      record$column, record$actual, record$expected
    ), record)
  }
  for (record in dtype_warnings) {
    result <- add_warning(result, sprintf(
      "Column '%s' has type %s, can be cast to %s",
      record$column, record$actual, record$expected
    ), record)
  }

  for (column_spec in column_specs) {
    column_name <- column_spec$name
    expected_type <- column_spec$data_type
    if (is.null(expected_type)) {
      next
    }
    if (!column_name %in% names(df)) {
      result <- add_info(
        result,
        sprintf("Column '%s': not present in data (dtype check skipped)", column_name),
        list(column = column_name, expected = expected_type)
      )
    } else if (!column_name %in% columns_with_issues) {
      result <- add_info(
        result,
        sprintf("Column '%s': dtype matches schema (%s)", column_name, expected_type),
        list(column = column_name, expected = expected_type)
      )
    }
  }

  result$atomic_total <- length(column_specs)
  result$atomic_passed <- result$atomic_total - length(dtype_errors)
  result
}

#' C.4 Validate datetime columns are stored as datetimes
#'
#' @inheritParams check_required_columns
#' @param expected_tz Timezone the site is expected to use.
#' @return A `dqa_conformance_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_datetime_format(labs_data, load_schema("labs"), "labs")
#' }
check_datetime_format <- function(df, schema, table_name, expected_tz = "UTC") {
  result <- dqa_conformance_result("datetime_format", table_name)

  column_specs <- schema$columns %||% list()
  datetime_columns <- vapply(
    Filter(function(spec) !is.null(spec$data_type) && spec$data_type %in% c("DATETIME", "DATE"), column_specs),
    function(spec) spec$name,
    character(1)
  )

  result$metrics$datetime_columns_checked <- length(datetime_columns)
  columns_with_messages <- character(0)

  for (column_name in datetime_columns) {
    if (!column_name %in% names(df)) {
      result <- add_info(
        result,
        sprintf("Column '%s': not present in data (datetime check skipped)", column_name),
        list(column = column_name)
      )
      columns_with_messages <- c(columns_with_messages, column_name)
      next
    }

    column_dtype <- polars_dtype_string(df[[column_name]], column_name)

    if (!grepl("Datetime", column_dtype, fixed = TRUE) && !grepl("Date", column_dtype, fixed = TRUE)) {
      result <- add_warning(
        result,
        sprintf("Column '%s' should be DATETIME but is %s", column_name, column_dtype),
        list(column = column_name, actual_type = column_dtype)
      )
      columns_with_messages <- c(columns_with_messages, column_name)
      next
    }

    if (!is.null(expected_tz) && nzchar(expected_tz) && grepl("Datetime", column_dtype, fixed = TRUE)) {
      if (!grepl(expected_tz, column_dtype, fixed = TRUE) &&
          !grepl("time_zone", column_dtype, fixed = TRUE)) {
        result <- add_info(
          result,
          sprintf("Column '%s' may be timezone-naive, expected %s", column_name, expected_tz),
          list(column = column_name, expected_tz = expected_tz)
        )
        columns_with_messages <- c(columns_with_messages, column_name)
      }
    }
  }

  for (column_name in setdiff(datetime_columns, columns_with_messages)) {
    result <- add_info(
      result,
      sprintf("Column '%s': datetime format valid", column_name),
      list(column = column_name)
    )
  }

  result$atomic_total <- length(datetime_columns)
  result$atomic_passed <- length(datetime_columns) - length(result$errors)
  result
}

# Accepted reference-unit spellings (lowercased, stripped) for one
# lab_reference_units entry, expanded against allowed_unit_variants.
resolve_accepted_units <- function(schema, ref_unit_entry) {
  if (is.list(ref_unit_entry) || length(ref_unit_entry) > 1) {
    unit_values <- unlist(ref_unit_entry, use.names = FALSE)
    return(sort(unique(trimws(tolower(as.character(unit_values[!is.na(unit_values)]))))))
  }
  if (!is.character(ref_unit_entry)) {
    return(character(0))
  }
  canonical_unit <- trimws(tolower(ref_unit_entry))
  variants_map <- schema$allowed_unit_variants %||% list()
  names(variants_map) <- trimws(tolower(names(variants_map)))
  variants <- variants_map[[canonical_unit]]
  if (!is.null(variants) && length(variants) > 0) {
    variant_values <- unlist(variants, use.names = FALSE)
    return(sort(unique(c(
      trimws(tolower(as.character(variant_values[!is.na(variant_values)]))),
      canonical_unit
    ))))
  }
  canonical_unit
}

# Evaluate one lab category's observed reference units against the accepted
# spelling set. Returns list(details, is_valid, bad).
evaluate_lab_category_units <- function(schema, lab_category_key, expected_units_entry, actual_pairs) {
  accepted_units <- resolve_accepted_units(schema, expected_units_entry)
  canonical_unit <- if (is.character(expected_units_entry) && length(expected_units_entry) == 1) {
    trimws(tolower(expected_units_entry))
  } else if (length(expected_units_entry) > 0) {
    trimws(tolower(as.character(unlist(expected_units_entry, use.names = FALSE)[1])))
  } else if (length(accepted_units) > 0) {
    accepted_units[1]
  } else {
    ""
  }

  variants_map <- schema$allowed_unit_variants %||% list()
  variant_lookup_used <- length(variants_map) > 0 &&
    is.character(expected_units_entry) && length(expected_units_entry) == 1
  accepts_no_units <- "(no units)" %in% accepted_units

  matched_canonical <- 0
  matched_via_variant <- 0
  bad_units <- list()

  for (pair in actual_pairs) {
    observed_unit <- pair$reference_unit
    observed_count <- pair$count
    if (accepts_no_units && (is.na(observed_unit) || !nzchar(observed_unit))) {
      matched_canonical <- matched_canonical + observed_count
      next
    }
    if (!is.na(observed_unit) && identical(observed_unit, canonical_unit)) {
      matched_canonical <- matched_canonical + observed_count
    } else if (!is.na(observed_unit) && observed_unit %in% accepted_units) {
      matched_via_variant <- matched_via_variant + observed_count
    } else {
      bad_units <- c(bad_units, list(list(
        reference_unit = pair$reference_unit_original,
        expected_units = accepted_units,
        count = observed_count
      )))
    }
  }

  if (length(bad_units) > 0) {
    bad_counts <- vapply(bad_units, function(entry) entry$count, numeric(1))
    bad_units <- bad_units[order(-bad_counts, method = "radix")]
  }
  invalid_records <- sum(vapply(bad_units, function(entry) entry$count, numeric(1)), 0)

  details <- list(
    column = lab_category_key,
    canonical_unit = canonical_unit,
    variant_lookup_used = variant_lookup_used,
    accepted_variant_count = length(accepted_units),
    accepted_variants_sample = utils::head(accepted_units, 5),
    matched_canonical_records = matched_canonical,
    matched_via_variant_records = matched_via_variant,
    invalid_records = invalid_records
  )
  if (length(bad_units) > 0) {
    details$top_invalid_units <- utils::head(bad_units, 10)
  }

  list(details = details, is_valid = length(bad_units) == 0, bad = bad_units)
}

#' C.7 Check lab reference units against the schema
#'
#' @param df Labs data to validate.
#' @param schema Parsed labs schema with `lab_reference_units`.
#' @param table_name Name of the table (default `"labs"`).
#' @return A `dqa_conformance_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_lab_reference_units(labs_data, load_schema("labs"), "labs")
#' }
check_lab_reference_units <- function(df, schema, table_name = "labs") {
  result <- dqa_conformance_result("lab_reference_units", table_name)

  lab_units <- schema$lab_reference_units %||% list()
  if (length(lab_units) == 0) {
    result <- add_info(result, "No lab reference units defined in schema")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  df <- normalize_for_validation(df)

  if (!"lab_category" %in% names(df) || !"reference_unit" %in% names(df)) {
    result <- add_error(result, "Missing required columns: lab_category and/or reference_unit")
    result$atomic_total <- 1L
    result$atomic_passed <- 0L
    return(result)
  }

  original_category_column <- paste0(ORIG_PREFIX, "lab_category")
  original_unit_column <- paste0(ORIG_PREFIX, "reference_unit")
  has_original_category <- original_category_column %in% names(df)
  has_original_unit <- original_unit_column %in% names(df)

  unit_counts <- df |>
    dplyr::group_by(.data$lab_category, .data$reference_unit) |>
    dplyr::summarise(
      count = dplyr::n(),
      category_original = if (has_original_category) dplyr::first(.data[[original_category_column]]) else dplyr::first(.data$lab_category),
      unit_original = if (has_original_unit) dplyr::first(.data[[original_unit_column]]) else dplyr::first(.data$reference_unit),
      .groups = "drop"
    )

  actual_units <- list()
  for (row_index in seq_len(nrow(unit_counts))) {
    category_key <- unit_counts$lab_category[row_index]
    if (is.na(category_key)) {
      category_key <- "NA"
    }
    actual_units[[category_key]] <- c(
      actual_units[[category_key]] %||% list(),
      list(list(
        reference_unit = unit_counts$reference_unit[row_index],
        reference_unit_original = unit_counts$unit_original[row_index],
        lab_category_original = unit_counts$category_original[row_index],
        count = unit_counts$count[row_index]
      ))
    )
  }

  result$metrics$total_records <- sum(unit_counts$count)
  invalid_category_count <- 0

  normalized_keys <- trimws(tolower(names(lab_units)))
  for (entry_index in seq_along(lab_units)) {
    normalized_key <- normalized_keys[entry_index]
    original_key <- names(lab_units)[entry_index]
    expected_units <- lab_units[[entry_index]]
    observed_pairs <- actual_units[[normalized_key]]

    if (is.null(observed_pairs)) {
      result <- add_info(
        result,
        sprintf("Lab category '%s': not present in data", original_key),
        list(column = original_key)
      )
      next
    }

    evaluation <- evaluate_lab_category_units(schema, original_key, expected_units, observed_pairs)
    if (evaluation$is_valid) {
      result <- add_info(
        result,
        sprintf("Lab category '%s': reference units match schema", original_key),
        evaluation$details
      )
    } else {
      invalid_category_count <- invalid_category_count + 1
      result <- add_warning(
        result,
        sprintf("Lab category '%s': non-standard units found", original_key),
        evaluation$details
      )
    }
  }

  result$metrics$invalid_unit_categories <- invalid_category_count
  result$atomic_total <- length(lab_units)
  result$atomic_passed <- length(lab_units) - length(result$errors)
  result
}

#' C.5 Check categorical values against mCIDE permissible values
#'
#' @inheritParams check_required_columns
#' @return A `dqa_conformance_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_categorical_values(patient_data, load_schema("patient"), "patient")
#' }
check_categorical_values <- function(df, schema, table_name) {
  result <- dqa_conformance_result("categorical_values", table_name)

  df <- normalize_for_validation(df)
  category_columns <- unlist(schema$category_columns %||% character(0), use.names = FALSE)

  invalid_values_by_column <- list()
  columns_checked <- character(0)
  columns_missing <- character(0)

  for (column_spec in schema$columns %||% list()) {
    column_name <- column_spec$name
    permissible_values <- unlist(column_spec$permissible_values %||% character(0), use.names = FALSE)

    if (length(permissible_values) == 0 || !column_name %in% category_columns) {
      next
    }
    columns_checked <- c(columns_checked, column_name)

    if (!column_name %in% names(df)) {
      columns_missing <- c(columns_missing, column_name)
      next
    }

    original_column <- paste0(ORIG_PREFIX, column_name)
    has_original <- original_column %in% names(df)

    value_counts <- df |>
      dplyr::filter(!is.na(.data[[column_name]])) |>
      dplyr::group_by(.data[[column_name]]) |>
      dplyr::summarise(
        count = dplyr::n(),
        display_value = if (has_original) dplyr::first(.data[[original_column]]) else dplyr::first(.data[[column_name]]),
        .groups = "drop"
      )

    permissible_lower <- unique(trimws(tolower(as.character(permissible_values))))
    invalid_rows <- which(
      !(trimws(tolower(as.character(value_counts[[column_name]]))) %in% permissible_lower) &
        !(value_counts[[column_name]] %in% permissible_values)
    )

    if (length(invalid_rows) > 0) {
      invalid_rows <- invalid_rows[order(-value_counts$count[invalid_rows], method = "radix")]
      invalid_entries <- lapply(invalid_rows, function(row_index) {
        list(value = value_counts$display_value[row_index], count = value_counts$count[row_index])
      })
      invalid_values_by_column[[column_name]] <- list(
        invalid_values = utils::head(invalid_entries, 20),
        total_invalid_unique = length(invalid_entries),
        permissible_values = permissible_values
      )
    }
  }

  result$metrics$category_columns_checked <- length(columns_checked)
  result$metrics$columns_with_invalid_values <- length(invalid_values_by_column)

  for (column_name in columns_checked) {
    if (column_name %in% columns_missing) {
      result <- add_info(
        result,
        sprintf("Column '%s': not present in data (categorical check skipped)", column_name),
        list(column = column_name)
      )
    } else if (!is.null(invalid_values_by_column[[column_name]])) {
      details <- invalid_values_by_column[[column_name]]
      result <- add_warning(
        result,
        sprintf("%d invalid categorical values", details$total_invalid_unique),
        list(
          column = column_name,
          top_invalid = utils::head(details$invalid_values, 10),
          permissible_values = details$permissible_values
        )
      )
    } else {
      result <- add_info(
        result,
        sprintf("Column '%s': all values match mCIDE permissible values", column_name),
        list(column = column_name)
      )
    }
  }

  result$atomic_total <- length(columns_checked)
  result$atomic_passed <- length(columns_checked) - length(result$errors)
  result
}

#' C.6 Check category-to-group mappings against the schema
#'
#' Discovers every `*_category_to_group_mapping` key in the schema and verifies
#' that each observed category maps to a permitted group.
#'
#' @inheritParams check_required_columns
#' @return A `dqa_conformance_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_category_group_mapping(med_data, load_schema("medication_admin_continuous"),
#'                              "medication_admin_continuous")
#' }
check_category_group_mapping <- function(df, schema, table_name) {
  result <- dqa_conformance_result("category_group_mapping", table_name)

  mapping_keys <- grep("_category_to_group_mapping$", names(schema), value = TRUE)
  if (length(mapping_keys) == 0) {
    result <- add_info(result, "No category-to-group mappings defined in schema")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  df <- normalize_for_validation(df)
  column_names <- names(df)
  total_pairs <- 0
  mismatch_total <- 0

  for (mapping_key in mapping_keys) {
    mapping <- schema[[mapping_key]]
    if (length(mapping) == 0) {
      next
    }
    total_pairs <- total_pairs + length(mapping)

    category_column <- sub("_to_group_mapping$", "", mapping_key)
    group_column <- sub("_category$", "_group", category_column)

    if (!category_column %in% column_names || !group_column %in% column_names) {
      for (category_value in names(mapping)) {
        result <- add_info(
          result,
          sprintf("Category '%s': columns not in data (mapping check skipped)", category_value),
          list(column = category_value, category_column = category_column, group_column = group_column)
        )
      }
      next
    }

    original_group_column <- paste0(ORIG_PREFIX, group_column)
    has_original_group <- original_group_column %in% column_names

    pair_counts <- df |>
      dplyr::filter(!is.na(.data[[category_column]]), !is.na(.data[[group_column]])) |>
      dplyr::group_by(.data[[category_column]], .data[[group_column]]) |>
      dplyr::summarise(
        count = dplyr::n(),
        group_original = if (has_original_group) dplyr::first(.data[[original_group_column]]) else dplyr::first(.data[[group_column]]),
        .groups = "drop"
      )

    actual_groups <- list()
    for (row_index in seq_len(nrow(pair_counts))) {
      category_key <- pair_counts[[category_column]][row_index]
      actual_groups[[category_key]] <- c(
        actual_groups[[category_key]] %||% list(),
        list(list(
          group = pair_counts[[group_column]][row_index],
          group_original = pair_counts$group_original[row_index],
          count = pair_counts$count[row_index]
        ))
      )
    }

    result$metrics[[paste0(mapping_key, "_total_records")]] <- sum(pair_counts$count)
    mismatch_count <- 0

    normalized_keys <- trimws(tolower(names(mapping)))
    for (entry_index in seq_along(mapping)) {
      normalized_key <- normalized_keys[entry_index]
      original_key <- names(mapping)[entry_index]
      expected_groups <- unlist(mapping[[entry_index]], use.names = FALSE)
      observed_pairs <- actual_groups[[normalized_key]]

      if (is.null(observed_pairs)) {
        result <- add_info(
          result,
          sprintf("Category '%s': not present in data", original_key),
          list(column = original_key, category_column = category_column, group_column = group_column)
        )
        next
      }

      allowed_lowered <- unique(trimws(tolower(as.character(expected_groups))))
      expected_display <- if (length(expected_groups) > 1) {
        paste(sprintf("'%s'", expected_groups), collapse = " or ")
      } else {
        sprintf("'%s'", expected_groups[1])
      }

      bad_pairs <- list()
      for (observed in observed_pairs) {
        if (!observed$group %in% allowed_lowered) {
          bad_pairs <- c(bad_pairs, list(list(
            category = original_key,
            actual_group = observed$group_original,
            expected_group = if (length(expected_groups) > 1) as.list(expected_groups) else expected_groups[1],
            count = observed$count
          )))
        }
      }

      if (length(bad_pairs) > 0) {
        mismatch_count <- mismatch_count + 1
        result <- add_warning(
          result,
          sprintf("Category '%s': group mismatch (expected %s)", original_key, expected_display),
          list(
            column = original_key, category_column = category_column,
            group_column = group_column, mismatched_pairs = bad_pairs
          )
        )
      } else {
        result <- add_info(
          result,
          sprintf("Category '%s': group mapping correct", original_key),
          list(column = original_key, category_column = category_column, group_column = group_column)
        )
      }
    }

    result$metrics[[paste0(mapping_key, "_mismatch_count")]] <- mismatch_count
    mismatch_total <- mismatch_total + mismatch_count
  }

  result$atomic_total <- total_pairs
  result$atomic_passed <- total_pairs - mismatch_total
  result
}


# ---------------------------------------------------------------------------
# COMPLETENESS CHECKS
# ---------------------------------------------------------------------------

#' K.1 Check missingness in required columns
#'
#' Columns covered by conditional requirements (K.2) and columns the schema
#' marks nullable are skipped; columns marked `allow_missing` cap their
#' severity at warning.
#'
#' @inheritParams check_required_columns
#' @param error_threshold Percent missing at or above which an error is raised.
#' @param warning_threshold Percent missing at or above which a warning is raised.
#' @return A `dqa_completeness_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_missingness(patient_data, load_schema("patient"), "patient")
#' }
check_missingness <- function(df, schema, table_name,
                              error_threshold = 50.0, warning_threshold = 10.0) {
  result <- dqa_completeness_result("missingness", table_name)

  required_columns <- unlist(schema$required_columns %||% character(0), use.names = FALSE)
  required_not_in_df <- setdiff(required_columns, names(df))
  required_in_df <- intersect(required_columns, names(df))

  conditions <- get_default_conditions(table_name)
  conditional_columns <- unique(unlist(
    lapply(conditions, function(condition) condition$then_required),
    use.names = FALSE
  ))
  required_in_df <- setdiff(required_in_df, conditional_columns)

  nullable_columns <- vapply(
    Filter(function(spec) isTRUE(spec$nullable), schema$columns %||% list()),
    function(spec) spec$name, character(1)
  )
  required_in_df <- setdiff(required_in_df, nullable_columns)

  allow_missing_columns <- vapply(
    Filter(function(spec) isTRUE(spec$allow_missing), schema$columns %||% list()),
    function(spec) spec$name, character(1)
  )

  total_rows <- nrow(df)
  if (total_rows == 0) {
    result <- add_error(result, "DataFrame is empty")
    result$atomic_total <- max(length(required_columns), 1L)
    result$atomic_passed <- 0L
    return(result)
  }

  missingness_stats <- list()
  high_missingness <- list()

  for (column_name in required_in_df) {
    null_count <- sum(is.na(df[[column_name]]))
    percent_missing <- (null_count / total_rows) * 100

    missingness_stats <- c(missingness_stats, list(list(
      column = column_name,
      null_count = as.integer(null_count),
      total_rows = as.integer(total_rows),
      percent_missing = py_round(percent_missing, 2)
    )))

    if (column_name %in% allow_missing_columns) {
      if (percent_missing >= warning_threshold) {
        high_missingness <- c(high_missingness, list(list(
          column = column_name, percent_missing = py_round(percent_missing, 2), severity = "warning"
        )))
      }
    } else if (percent_missing >= error_threshold) {
      high_missingness <- c(high_missingness, list(list(
        column = column_name, percent_missing = py_round(percent_missing, 2), severity = "error"
      )))
    } else if (percent_missing >= warning_threshold) {
      high_missingness <- c(high_missingness, list(list(
        column = column_name, percent_missing = py_round(percent_missing, 2), severity = "warning"
      )))
    }
  }

  if (length(missingness_stats) > 0) {
    stat_percents <- vapply(missingness_stats, function(stat) stat$percent_missing, numeric(1))
    missingness_stats <- missingness_stats[order(-stat_percents, method = "radix")]
  }

  result$metrics$total_rows <- as.integer(total_rows)
  result$metrics$required_columns_checked <- length(required_in_df)
  result$metrics$missingness_stats <- missingness_stats

  high_missingness_columns <- vapply(high_missingness, function(item) item$column, character(1))

  for (item in high_missingness) {
    message_text <- sprintf(
      "Column '%s' has %s%% missing values",
      item$column, py_num_str(item$percent_missing)
    )
    if (identical(item$severity, "error")) {
      result <- add_error(result, message_text, item)
    } else {
      result <- add_warning(result, message_text, item)
    }
  }

  for (stat in missingness_stats) {
    if (!stat$column %in% high_missingness_columns) {
      result <- add_info(
        result,
        sprintf("Column '%s': %s%% missing (below thresholds)", stat$column, py_num_str(stat$percent_missing)),
        list(
          column = stat$column, percent_missing = stat$percent_missing,
          error_threshold = error_threshold, warning_threshold = warning_threshold
        )
      )
    }
  }

  for (column_name in required_not_in_df) {
    if (!column_name %in% conditional_columns) {
      result <- add_info(
        result,
        sprintf("Column '%s': not present in data (missingness check skipped)", column_name),
        list(column = column_name)
      )
    }
  }

  # Lenient scoring: only errors (above error_threshold, or absent outside K.2
  # coverage) reduce atomic_passed.
  result$atomic_total <- length(required_columns)
  failed_atom_count <- sum(vapply(high_missingness, function(item) identical(item$severity, "error"), logical(1))) +
    length(setdiff(required_not_in_df, conditional_columns))
  result$atomic_passed <- result$atomic_total - failed_atom_count
  result
}

#' K.2 Check conditional required fields
#'
#' Each rule names a `when_column`/`when_value` (optionally compounded with
#' `and_column`/`and_value`) and the columns that must then be populated.
#'
#' @param df Data to validate.
#' @param table_name Name of the table.
#' @param conditions Optional list of condition rules; defaults to those
#'   defined for `table_name` in `validation_rules.yaml`.
#' @return A `dqa_completeness_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_conditional_requirements(adt_data, "adt")
#' }
check_conditional_requirements <- function(df, table_name, conditions = NULL) {
  result <- dqa_completeness_result("conditional_requirements", table_name)

  if (is.null(conditions)) {
    conditions <- get_default_conditions(table_name)
  }
  if (length(conditions) == 0) {
    result <- add_info(result, "No conditional requirements defined for this table")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  column_names <- names(df)
  atomic_total <- 0

  for (condition in conditions) {
    when_column <- condition$when_column
    when_values <- unlist(condition$when_value, use.names = FALSE)
    then_required <- unlist(condition$then_required, use.names = FALSE)
    description <- condition$description %||% ""

    # All then_required columns present in the data count toward the atomic
    # total, whether or not the condition itself can be evaluated.
    atomic_total <- atomic_total + sum(then_required %in% column_names)

    if (!when_column %in% column_names) {
      next
    }

    row_mask <- trimws(tolower(as.character(df[[when_column]]))) %in% trimws(tolower(as.character(when_values)))
    row_mask[is.na(row_mask)] <- FALSE

    and_column <- condition$and_column
    and_values <- condition$and_value
    if (!is.null(and_column) && !is.null(and_values)) {
      if (!and_column %in% column_names) {
        next
      }
      and_values <- unlist(and_values, use.names = FALSE)
      and_mask <- trimws(tolower(as.character(df[[and_column]]))) %in% trimws(tolower(as.character(and_values)))
      and_mask[is.na(and_mask)] <- FALSE
      row_mask <- row_mask & and_mask
    }

    condition_label <- sprintf("%s IN %s", when_column, py_list_repr(when_values))
    if (!is.null(and_column) && !is.null(and_values)) {
      condition_label <- paste0(condition_label, sprintf(" AND %s IN %s", and_column, py_list_repr(and_values)))
    }

    for (required_column in then_required) {
      if (!required_column %in% column_names) {
        next
      }
      total_matching <- sum(row_mask)
      null_count <- if (total_matching > 0) sum(is.na(df[[required_column]][row_mask])) else 0

      if (total_matching > 0 && null_count > 0) {
        percent_missing <- (null_count / total_matching) * 100
        result <- add_warning(
          result,
          sprintf("Conditional requirement violated: %s", description),
          list(
            condition = condition_label,
            required_column = required_column,
            rows_meeting_condition = as.integer(total_matching),
            rows_with_missing = as.integer(null_count),
            percent_missing = py_round(percent_missing, 2)
          )
        )
      } else if (total_matching > 0) {
        result <- add_info(
          result,
          sprintf(
            "Conditional requirement satisfied: %s — %s/%s rows present (100%%)",
            description, py_int_comma(total_matching), py_int_comma(total_matching)
          ),
          list(
            column = required_column, condition = condition_label,
            rows_meeting_condition = as.integer(total_matching),
            rows_present = as.integer(total_matching),
            percent_present = 100.0
          )
        )
      }
    }
  }

  result$atomic_total <- atomic_total
  result$atomic_passed <- atomic_total
  result
}

# Render a character vector the way Python renders a list of strings, since
# condition labels embed the raw repr.
py_list_repr <- function(values) {
  paste0("[", paste(sprintf("'%s'", as.character(values)), collapse = ", "), "]")
}

#' K.3 Check mCIDE value coverage
#'
#' Reports which permissible values for each category column never appear in
#' the data.
#'
#' @inheritParams check_required_columns
#' @return A `dqa_completeness_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_mcide_value_coverage(patient_data, load_schema("patient"), "patient")
#' }
check_mcide_value_coverage <- function(df, schema, table_name) {
  result <- dqa_completeness_result("mcide_value_coverage", table_name)

  category_columns <- unlist(schema$category_columns %||% character(0), use.names = FALSE)
  coverage_by_column <- list()
  columns_missing <- character(0)
  expected_total <- 0

  for (column_spec in schema$columns %||% list()) {
    column_name <- column_spec$name
    permissible_values <- unlist(column_spec$permissible_values %||% character(0), use.names = FALSE)

    if (length(permissible_values) == 0 || !column_name %in% category_columns) {
      next
    }
    expected_total <- expected_total + length(permissible_values)

    if (!column_name %in% names(df)) {
      columns_missing <- c(columns_missing, column_name)
      next
    }

    observed_values <- df[[column_name]]
    observed_lower <- unique(trimws(tolower(as.character(observed_values[!is.na(observed_values)]))))
    missing_values <- permissible_values[
      !(trimws(tolower(as.character(permissible_values))) %in% observed_lower)
    ]

    coverage_percent <- ((length(permissible_values) - length(missing_values)) / length(permissible_values)) * 100
    coverage_by_column[[column_name]] <- list(
      expected_values = length(permissible_values),
      found_values = length(permissible_values) - length(missing_values),
      missing_values = as.list(missing_values),
      coverage_percent = py_round(coverage_percent, 2)
    )
  }

  result$metrics$category_columns_checked <- length(coverage_by_column) + length(columns_missing)
  result$metrics$coverage_by_column <- coverage_by_column
  result$atomic_total <- expected_total
  result$atomic_passed <- sum(vapply(coverage_by_column, function(entry) entry$found_values, numeric(1)), 0)

  for (column_name in names(coverage_by_column)) {
    details <- coverage_by_column[[column_name]]
    if (length(details$missing_values) > 0) {
      result <- add_error(
        result,
        sprintf(
          "Missing %d mCIDE values: %s",
          length(details$missing_values),
          paste(vapply(details$missing_values, as.character, character(1)), collapse = ", ")
        ),
        list(
          column = column_name,
          missing_values = details$missing_values,
          coverage_percent = details$coverage_percent
        )
      )
    }
    if (details$found_values > 0) {
      result <- add_info(
        result,
        sprintf(
          "Column '%s': %d/%d mCIDE values present",
          column_name, details$found_values, details$expected_values
        ),
        list(
          column = column_name,
          found_values = details$found_values,
          expected_values = details$expected_values,
          coverage_percent = details$coverage_percent,
          atomic_count = details$found_values
        )
      )
    }
  }

  for (column_name in columns_missing) {
    result <- add_info(
      result,
      sprintf("Column '%s': not present in data (coverage check skipped)", column_name),
      list(column = column_name)
    )
  }

  result
}

# One direction of the relational integrity check: what share of the source's
# key values resolve in the reference table?
check_relational_integrity_directional <- function(source_df, reference_df,
                                                   source_table, reference_table, key_column) {
  result <- dqa_completeness_result("relational_integrity", paste0(source_table, "->", reference_table))

  source_ids <- unique(source_df[[key_column]][!is.na(source_df[[key_column]])])
  reference_ids <- unique(reference_df[[key_column]][!is.na(reference_df[[key_column]])])
  orphan_ids <- setdiff(source_ids, reference_ids)

  total_source <- length(source_ids)
  total_orphan <- length(orphan_ids)
  coverage_percent <- if (total_source > 0) (total_source - total_orphan) / total_source * 100 else 100

  result$metrics$source_unique_ids <- total_source
  result$metrics$reference_unique_ids <- length(reference_ids)
  result$metrics$orphan_ids <- total_orphan
  result$metrics$coverage_percent <- py_round(coverage_percent, 2)

  if (total_orphan > 0) {
    result <- add_warning(
      result,
      sprintf(
        "%d/%d %s values in %s not found in %s (%s%% coverage)",
        total_orphan, total_source, key_column, source_table, reference_table,
        py_num_str(py_round(coverage_percent, 1))
      ),
      list(orphan_count = total_orphan, coverage_percent = py_round(coverage_percent, 2))
    )
  } else {
    result <- add_info(result, sprintf(
      "All %s values in %s exist in %s", key_column, source_table, reference_table
    ))
  }

  result$atomic_total <- 1L
  result$atomic_passed <- if (length(result$errors) == 0) 1L else 0L
  result
}

#' K.4 Check bidirectional relational integrity between two tables
#'
#' Runs the directional check both ways:
#'
#' * **Forward** (reference to target) — what share of reference IDs appear in
#'   the target table? This routinely fails legitimately on inpatient data, so
#'   it is always surfaced as a warning with `atomic_count = 0`.
#' * **Reverse** (target to reference) — the true foreign-key question. Above
#'   10% orphans it becomes an error, above 1% a warning, otherwise it passes
#'   silently.
#'
#' @param target_df The target table (e.g. labs).
#' @param reference_df The reference table (e.g. hospitalization).
#' @param target_table Name of the target table.
#' @param reference_table Name of the reference table.
#' @param key_column The shared key column, e.g. `"hospitalization_id"`.
#' @return A `dqa_completeness_result` with forward and reverse coverage metrics.
#' @export
#'
#' @examples
#' \dontrun{
#' check_relational_integrity(labs_data, hosp_data, "labs", "hospitalization",
#'                            "hospitalization_id")
#' }
check_relational_integrity <- function(target_df, reference_df, target_table,
                                       reference_table, key_column) {
  result <- dqa_completeness_result(
    "relational_integrity",
    paste0(target_table, "<->", reference_table)
  )

  forward <- check_relational_integrity_directional(
    reference_df, target_df, reference_table, target_table, key_column
  )
  reverse <- check_relational_integrity_directional(
    target_df, reference_df, target_table, reference_table, key_column
  )

  result$metrics$forward_coverage_percent <- forward$metrics$coverage_percent %||% 0
  result$metrics$forward_orphan_ids <- forward$metrics$orphan_ids %||% 0
  result$metrics$forward_reference_unique_ids <- forward$metrics$source_unique_ids %||% 0
  result$metrics$reverse_coverage_percent <- reverse$metrics$coverage_percent %||% 0
  result$metrics$reverse_orphan_ids <- reverse$metrics$orphan_ids %||% 0
  result$metrics$reverse_target_unique_ids <- reverse$metrics$source_unique_ids %||% 0

  for (warning_entry in forward$warnings) {
    forward_details <- warning_entry$details %||% list()
    if (is.null(forward_details$atomic_count)) {
      forward_details$atomic_count <- 0L
    }
    result <- add_warning(result, warning_entry$message, forward_details)
  }
  for (error_entry in forward$errors) {
    result <- add_error(result, error_entry$message, error_entry$details %||% list())
  }

  reverse_orphan_percent <- 100 - result$metrics$reverse_coverage_percent
  for (warning_entry in reverse$warnings) {
    if (reverse_orphan_percent > 10.0) {
      result <- add_error(result, warning_entry$message, warning_entry$details %||% list())
    } else if (reverse_orphan_percent > 1.0) {
      result <- add_warning(result, warning_entry$message, warning_entry$details %||% list())
    }
  }
  for (error_entry in reverse$errors) {
    result <- add_error(result, error_entry$message, error_entry$details %||% list())
  }

  if (forward$passed && reverse$passed &&
      length(forward$warnings) == 0 && length(reverse$warnings) == 0) {
    result <- add_info(result, sprintf(
      "Full bidirectional coverage for %s between %s and %s",
      key_column, target_table, reference_table
    ))
  }

  result$atomic_total <- 1L
  result$atomic_passed <- if (length(result$errors) == 0) 1L else 0L
  result
}


# ---------------------------------------------------------------------------
# PLAUSIBILITY CHECKS
# ---------------------------------------------------------------------------

#' P.1 Check that datetime pairs follow the expected chronological order
#'
#' @param df Data to validate.
#' @param table_name Name of the table.
#' @param chronological_rules Optional list of rules; defaults to those defined
#'   for `table_name` in `validation_rules.yaml`.
#' @param warning_threshold Violation percent above which a warning is raised.
#' @param error_threshold Violation percent above which an error is raised.
#' @return A `dqa_plausibility_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_chronological_order(adt_data, "adt")
#' }
check_chronological_order <- function(df, table_name, chronological_rules = NULL,
                                      warning_threshold = 0.0, error_threshold = 10.0) {
  result <- dqa_plausibility_result("chronological_order", table_name)

  if (is.null(chronological_rules)) {
    chronological_rules <- get_chronological_order_rules(table_name)
  }
  if (length(chronological_rules) == 0) {
    result <- add_info(result, "No chronological order rules defined for this table")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  violations_by_pair <- list()

  for (rule in chronological_rules) {
    earlier_column <- rule$earlier
    later_column <- rule$later
    is_strict <- isTRUE(rule$strict)
    description <- rule$description %||%
      sprintf("%s %s %s", earlier_column, if (is_strict) "<" else "<=", later_column)

    if (!earlier_column %in% names(df) || !later_column %in% names(df)) {
      next
    }

    applicable_rows <- !is.na(df[[earlier_column]]) & !is.na(df[[later_column]])
    total_applicable <- sum(applicable_rows)
    violation_count <- if (total_applicable > 0) {
      earlier_values <- df[[earlier_column]][applicable_rows]
      later_values <- df[[later_column]][applicable_rows]
      if (is_strict) sum(earlier_values >= later_values) else sum(earlier_values > later_values)
    } else {
      0
    }
    violation_percent <- if (total_applicable > 0) violation_count / total_applicable * 100 else 0

    violations_by_pair[[paste0(earlier_column, "->", later_column)]] <- list(
      total_applicable = as.integer(total_applicable),
      violations = as.integer(violation_count),
      violation_percent = py_round(violation_percent, 2),
      description = description
    )

    violation_message <- sprintf(
      "Chronological order violation: %s — %s/%s rows (%s%%)",
      description, py_int_str(violation_count), py_int_str(total_applicable), py_pct1(violation_percent)
    )
    violation_details <- list(
      column = paste0(earlier_column, ", ", later_column),
      pair = paste0(earlier_column, "->", later_column),
      violations = as.integer(violation_count),
      total = as.integer(total_applicable),
      percent = py_round(violation_percent, 2)
    )

    if (violation_percent > error_threshold) {
      result <- add_error(result, violation_message, violation_details)
    } else if (violation_percent > warning_threshold) {
      result <- add_warning(result, violation_message, violation_details)
    } else if (total_applicable > 0) {
      result <- add_info(
        result,
        sprintf(
          "Chronological order satisfied: %s — %s/%s rows valid (100%%)",
          description, py_int_comma(total_applicable), py_int_comma(total_applicable)
        ),
        list(
          column = paste0(earlier_column, ", ", later_column),
          rows_checked = as.integer(total_applicable),
          rows_valid = as.integer(total_applicable),
          percent_valid = 100.0
        )
      )
    }
  }

  result$metrics$pairs_checked <- length(violations_by_pair)
  result$metrics$violations_by_pair <- violations_by_pair
  result$atomic_total <- length(chronological_rules)
  result$atomic_passed <- length(chronological_rules) - length(result$errors)
  result
}

#' P.2 Check numeric values against plausible ranges
#'
#' Ranges come from `outlier_config.yaml` and may be simple (`column` to
#' min/max), category-dependent, or category-and-unit-dependent.
#'
#' @param df Data to validate.
#' @param table_name Name of the table.
#' @param outlier_config Optional parsed outlier config; defaults to the
#'   packaged `outlier_config.yaml`.
#' @param warning_threshold Out-of-range percent above which a warning is raised.
#' @param error_threshold Out-of-range percent above which an error is raised.
#' @return A `dqa_plausibility_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_numeric_range_plausibility(vitals_data, "vitals")
#' }
check_numeric_range_plausibility <- function(df, table_name, outlier_config = NULL,
                                             warning_threshold = 0.0, error_threshold = 10.0) {
  result <- dqa_plausibility_result("numeric_range_plausibility", table_name)

  if (is.null(outlier_config)) {
    outlier_config <- load_validator_outlier_config()
  }
  table_config <- outlier_config$tables[[table_name]] %||% list()
  if (length(table_config) == 0) {
    result <- add_info(result, "No numeric range configuration for this table")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  column_names <- names(df)
  out_of_range_summary <- list()
  category_map <- CATEGORY_COLUMN_MAP[[table_name]] %||% list()
  atomic_total <- 0
  atomic_passed <- 0

  # Pre-cast configured columns that arrived as strings.
  for (configured_column in names(table_config)) {
    if (configured_column %in% column_names && is.character(df[[configured_column]])) {
      df[[configured_column]] <- suppressWarnings(as.numeric(df[[configured_column]]))
    }
  }

  for (column_name in names(table_config)) {
    column_ranges <- table_config[[column_name]]
    if (!column_name %in% column_names) {
      next
    }

    if (is.list(column_ranges) && !is.null(column_ranges$min) && !is.null(column_ranges$max)) {
      atomic_total <- atomic_total + 1
      range_min <- column_ranges$min
      range_max <- column_ranges$max
      column_values <- df[[column_name]]
      non_missing_values <- column_values[!is.na(column_values)]

      total_non_null <- length(non_missing_values)
      below_min <- sum(non_missing_values < range_min)
      above_max <- sum(non_missing_values > range_max)
      out_of_range <- sum(non_missing_values < range_min | non_missing_values > range_max)
      out_of_range_percent <- if (total_non_null > 0) out_of_range / total_non_null * 100 else 0

      out_of_range_summary[[column_name]] <- list(
        total_non_null = as.integer(total_non_null), out_of_range = as.integer(out_of_range),
        out_of_range_percent = py_round(out_of_range_percent, 2),
        below_min = as.integer(below_min), above_max = as.integer(above_max),
        min = range_min, max = range_max
      )

      range_message <- sprintf(
        "Column '%s': %s/%s values (%s%%) outside range [%s, %s]",
        column_name, py_int_str(out_of_range), py_int_str(total_non_null),
        py_pct1(out_of_range_percent), py_num_repr(range_min), py_num_repr(range_max)
      )
      range_details <- list(column = column_name, percent = py_round(out_of_range_percent, 2))

      if (out_of_range_percent > error_threshold) {
        result <- add_error(result, range_message, range_details)
      } else {
        if (out_of_range_percent > warning_threshold) {
          result <- add_warning(result, range_message, range_details)
        } else {
          result <- add_info(
            result,
            sprintf(
              "Column '%s': all %s values within range [%s, %s]",
              column_name, py_int_comma(total_non_null), py_num_repr(range_min), py_num_repr(range_max)
            ),
            list(column = column_name, total = as.integer(total_non_null), atomic_count = 1L)
          )
        }
        atomic_passed <- atomic_passed + 1
      }
      next
    }

    if (!is.list(column_ranges)) {
      next
    }

    # Category-dependent ranges
    category_info <- category_map[[column_name]]
    if (is.null(category_info)) {
      next
    }

    is_two_level <- length(category_info) == 2
    category_column <- category_info[1]
    unit_column <- if (is_two_level) category_info[2] else NULL

    if (!category_column %in% column_names) {
      next
    }
    if (is_two_level && !unit_column %in% column_names) {
      next
    }

    normalized_categories <- trimws(tolower(as.character(df[[category_column]])))
    normalized_units <- if (is_two_level) trimws(tolower(as.character(df[[unit_column]]))) else NULL
    measurement_values <- df[[column_name]]

    combinations <- list()
    for (category_value in names(column_ranges)) {
      inner <- column_ranges[[category_value]]
      if (!is.list(inner)) {
        next
      }
      if (is_two_level) {
        for (unit_value in names(inner)) {
          ranges <- inner[[unit_value]]
          if (is.list(ranges) && !is.null(ranges$min)) {
            combinations <- c(combinations, list(list(
              category = category_value, unit = unit_value,
              min = ranges$min, max = ranges$max
            )))
          }
        }
      } else if (!is.null(inner$min)) {
        combinations <- c(combinations, list(list(
          category = category_value, unit = NULL, min = inner$min, max = inner$max
        )))
      }
    }

    if (length(combinations) == 0) {
      next
    }
    atomic_total <- atomic_total + length(combinations)

    total_count <- 0
    total_out_of_range <- 0

    for (combination in combinations) {
      selection_mask <- normalized_categories == trimws(tolower(combination$category)) & !is.na(measurement_values)
      if (is_two_level) {
        selection_mask <- selection_mask & normalized_units == trimws(tolower(combination$unit))
      }
      selection_mask[is.na(selection_mask)] <- FALSE

      category_total <- sum(selection_mask)
      selected_values <- measurement_values[selection_mask]
      category_out_of_range <- sum(selected_values < combination$min | selected_values > combination$max)
      category_percent <- if (category_total > 0) category_out_of_range / category_total * 100 else 0
      total_count <- total_count + category_total
      total_out_of_range <- total_out_of_range + category_out_of_range

      label <- if (is_two_level) {
        sprintf("%s, %s", combination$category, combination$unit)
      } else {
        combination$category
      }
      combination_message <- sprintf(
        "Column '%s' (%s): %s/%s values (%s%%) outside range [%s, %s]",
        column_name, label, py_int_str(category_out_of_range), py_int_str(category_total),
        py_pct1(category_percent), py_num_repr(combination$min), py_num_repr(combination$max)
      )
      combination_details <- if (is_two_level) {
        list(column = column_name, category = combination$category, unit = combination$unit,
             percent = py_round(category_percent, 2))
      } else {
        list(column = column_name, category = combination$category, percent = py_round(category_percent, 2))
      }

      if (category_out_of_range > 0 && category_percent > error_threshold) {
        result <- add_error(result, combination_message, combination_details)
      } else {
        if (category_out_of_range > 0 && category_percent > warning_threshold) {
          result <- add_warning(result, combination_message, combination_details)
        }
        atomic_passed <- atomic_passed + 1
      }
    }

    overall_percent <- if (total_count > 0) total_out_of_range / total_count * 100 else 0
    out_of_range_summary[[column_name]] <- list(
      total_non_null = as.integer(total_count),
      out_of_range = as.integer(total_out_of_range),
      out_of_range_percent = py_round(overall_percent, 2)
    )
  }

  result$metrics$columns_checked <- length(out_of_range_summary)
  result$metrics$out_of_range_summary <- out_of_range_summary
  result$atomic_total <- atomic_total
  result$atomic_passed <- atomic_passed

  if (length(result$warnings) == 0 && length(result$errors) == 0) {
    if (length(out_of_range_summary) > 0) {
      checked_columns <- sort(names(out_of_range_summary))
      result <- add_info(
        result,
        sprintf("All numeric values within plausible ranges (%s)", paste(checked_columns, collapse = ", ")),
        list(columns_checked = as.list(checked_columns))
      )
    } else {
      result <- add_info(result, "No numeric columns with range configuration to check")
    }
  }

  result
}

# Render a YAML-sourced numeric bound the way Python's str() does, so range
# messages read "[0.0, 15.0]" for floats and "[0, 5000]" for integers.
py_num_repr <- function(value) {
  if (is.integer(value)) {
    return(as.character(value))
  }
  if (is.numeric(value) && value == round(value) && abs(value) < 1e15) {
    # YAML distinguishes 0 from 0.0; R does not, so fall back on how the value
    # was parsed: yaml::read_yaml gives integer for 0 and double for 0.0.
    return(sprintf("%.1f", value))
  }
  format(value, scientific = FALSE, trim = TRUE)
}

#' P.3 Check field-level plausibility rules
#'
#' Supports both rule shapes clifpy defines: `when_not_null` +
#' `then_column`/`then_not_value`, and `when_not_value` + `then_null_or_absent`.
#'
#' @param df Data to validate.
#' @param table_name Name of the table.
#' @param rules Optional list of rules; defaults to those defined for
#'   `table_name` in `validation_rules.yaml`.
#' @return A `dqa_plausibility_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_field_plausibility(adt_data, "adt")
#' }
check_field_plausibility <- function(df, table_name, rules = NULL) {
  result <- dqa_plausibility_result("field_plausibility", table_name)

  if (is.null(rules)) {
    rules <- get_field_plausibility_rules(table_name)
  }
  if (length(rules) == 0) {
    result <- add_info(result, "No field plausibility rules defined for this table")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  column_names <- names(df)
  violations_by_rule <- list()

  for (rule in rules) {
    when_column <- rule$when_column
    description <- rule$description %||% ""

    if (!when_column %in% column_names) {
      next
    }

    if (isTRUE(rule$when_not_null)) {
      then_column <- rule$then_column
      forbidden_values <- unlist(rule$then_not_value, use.names = FALSE)
      if (!then_column %in% column_names) {
        next
      }

      applicable_rows <- !is.na(df[[when_column]])
      total_applicable <- sum(applicable_rows)
      violation_count <- if (total_applicable > 0) {
        sum(trimws(tolower(as.character(df[[then_column]][applicable_rows]))) %in%
              trimws(tolower(as.character(forbidden_values))), na.rm = TRUE)
      } else {
        0
      }
      violation_percent <- if (total_applicable > 0) violation_count / total_applicable * 100 else 0

      if (violation_count > 0) {
        violations_by_rule[[description]] <- list(
          total_applicable = as.integer(total_applicable),
          violations = as.integer(violation_count),
          violation_percent = py_round(violation_percent, 2)
        )
        result <- add_warning(
          result,
          sprintf(
            "Field plausibility violation: %s — %s/%s rows (%s%%)",
            description, py_int_str(violation_count), py_int_str(total_applicable), py_pct1(violation_percent)
          ),
          list(rule = description, violations = as.integer(violation_count),
               total = as.integer(total_applicable), percent = py_round(violation_percent, 2))
        )
      } else if (total_applicable > 0) {
        result <- add_info(
          result,
          sprintf(
            "Field plausibility satisfied: %s — %s/%s rows valid (100%%)",
            description, py_int_comma(total_applicable), py_int_comma(total_applicable)
          ),
          list(column = then_column, rows_checked = as.integer(total_applicable),
               rows_valid = as.integer(total_applicable), percent_valid = 100.0)
        )
      }
      next
    }

    forbidden_when_values <- unlist(rule$when_not_value, use.names = FALSE)
    then_null_columns <- unlist(rule$then_null_or_absent, use.names = FALSE)

    applicable_rows <- !(trimws(tolower(as.character(df[[when_column]]))) %in%
                           trimws(tolower(as.character(forbidden_when_values))))
    applicable_rows[is.na(applicable_rows)] <- TRUE

    for (check_column in then_null_columns) {
      if (!check_column %in% column_names) {
        next
      }
      total_applicable <- sum(applicable_rows)
      non_null_count <- if (total_applicable > 0) sum(!is.na(df[[check_column]][applicable_rows])) else 0
      violation_percent <- if (total_applicable > 0) non_null_count / total_applicable * 100 else 0

      if (non_null_count > 0) {
        violations_by_rule[[description]] <- list(
          total_applicable = as.integer(total_applicable),
          violations = as.integer(non_null_count),
          violation_percent = py_round(violation_percent, 2)
        )
        result <- add_warning(
          result,
          sprintf(
            "Field plausibility violation: %s — %s/%s rows (%s%%)",
            description, py_int_str(non_null_count), py_int_str(total_applicable), py_pct1(violation_percent)
          ),
          list(rule = description, violations = as.integer(non_null_count),
               total = as.integer(total_applicable), percent = py_round(violation_percent, 2))
        )
      } else if (total_applicable > 0) {
        result <- add_info(
          result,
          sprintf(
            "Field plausibility satisfied: %s — %s/%s rows valid (100%%)",
            description, py_int_comma(total_applicable), py_int_comma(total_applicable)
          ),
          list(column = check_column, rows_checked = as.integer(total_applicable),
               rows_valid = as.integer(total_applicable), percent_valid = 100.0)
        )
      }
    }
  }

  result$metrics$rules_checked <- length(rules)
  result$metrics$violations_by_rule <- violations_by_rule
  result$atomic_total <- length(rules)
  result$atomic_passed <- length(rules) - length(result$errors)
  result
}

#' P.4 Check medication dose-unit consistency
#'
#' Continuous administrations are expected to carry rate-based units (a time
#' denominator); intermittent administrations are expected not to.
#'
#' @param df Data to validate.
#' @param table_name Name of the table.
#' @param warning_threshold Violation percent above which a warning is raised.
#' @param error_threshold Violation percent above which an error is raised.
#' @return A `dqa_plausibility_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_medication_dose_unit_consistency(med_data, "medication_admin_continuous")
#' }
check_medication_dose_unit_consistency <- function(df, table_name,
                                                   warning_threshold = 0.0, error_threshold = 10.0) {
  result <- dqa_plausibility_result("medication_dose_unit_consistency", table_name)

  if (!table_name %in% c("medication_admin_continuous", "medication_admin_intermittent")) {
    result <- add_info(result, "Medication dose unit check not applicable to this table")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  if (!"med_dose_unit" %in% names(df)) {
    result <- add_info(result, "Column 'med_dose_unit' not found in table")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  rules <- load_validation_rules()$medication_dose_unit_rules[[table_name]] %||% list()
  expectation <- rules$expect %||%
    (if (grepl("continuous", table_name, fixed = TRUE)) "per_time" else "discrete")

  dose_units <- as.character(df$med_dose_unit)
  non_null_units <- dose_units[!is.na(dose_units)]
  total_rows <- length(non_null_units)

  if (total_rows == 0) {
    result <- add_info(result, "No non-null med_dose_unit values to check")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  has_time_denominator <- Reduce(
    `|`,
    lapply(TIME_DENOMINATOR_PATTERNS, function(pattern) grepl(pattern, non_null_units, fixed = TRUE))
  )
  violation_count <- if (identical(expectation, "per_time")) {
    sum(!has_time_denominator)
  } else {
    sum(has_time_denominator)
  }
  violation_percent <- if (total_rows > 0) violation_count / total_rows * 100 else 0

  result$metrics$total_rows <- as.integer(total_rows)
  result$metrics$unit_pattern_violations <- as.integer(violation_count)
  result$metrics$violation_percent <- py_round(violation_percent, 2)

  if (violation_count > 0) {
    violation_message <- if (identical(expectation, "per_time")) {
      sprintf(
        "Medication dose unit inconsistency: %s/%s rows (%s%%) use non-rate-based units unexpected for continuous administration",
        py_int_str(violation_count), py_int_str(total_rows), py_pct1(violation_percent)
      )
    } else {
      sprintf(
        "Medication dose unit inconsistency: %s/%s rows (%s%%) use rate-based units unexpected for intermittent administration",
        py_int_str(violation_count), py_int_str(total_rows), py_pct1(violation_percent)
      )
    }
    violation_details <- list(
      violations = as.integer(violation_count), total = as.integer(total_rows),
      percent = py_round(violation_percent, 2)
    )
    if (violation_percent > error_threshold) {
      result <- add_error(result, violation_message, violation_details)
    } else if (violation_percent > warning_threshold) {
      result <- add_warning(result, violation_message, violation_details)
    }
  } else if (identical(expectation, "per_time")) {
    result <- add_info(
      result,
      "All med_dose_unit values use rate-based units (e.g. mcg/kg/min) appropriate for continuous administration"
    )
  } else {
    result <- add_info(
      result,
      "All med_dose_unit values use dose-based units (e.g. mg, mL) appropriate for intermittent administration"
    )
  }

  result$atomic_total <- 1L
  result$atomic_passed <- if (length(result$errors) == 0) 1L else 0L
  result
}

#' P.8 Check that event times fall within hospitalization bounds
#'
#' @param target_df The table whose timestamps are being checked.
#' @param hospitalization_df The hospitalization table supplying admission and
#'   discharge bounds.
#' @param target_table Name of the target table.
#' @param time_columns Character vector of datetime columns to check.
#' @param warning_threshold Violation percent above which a warning is raised.
#' @param error_threshold Violation percent above which an error is raised.
#' @return A `dqa_plausibility_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_cross_table_temporal_plausibility(labs_data, hosp_data, "labs",
#'                                         c("lab_collect_dttm"))
#' }
check_cross_table_temporal_plausibility <- function(target_df, hospitalization_df, target_table,
                                                    time_columns,
                                                    warning_threshold = 0.0, error_threshold = 10.0) {
  result <- dqa_plausibility_result("cross_table_temporal", target_table)

  if (!"hospitalization_id" %in% names(target_df) ||
      !"hospitalization_id" %in% names(hospitalization_df)) {
    result <- add_info(result, "Missing hospitalization_id column; skipping cross-table check")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }
  if (!"admission_dttm" %in% names(hospitalization_df) ||
      !"discharge_dttm" %in% names(hospitalization_df)) {
    result <- add_info(result, "Missing admission/discharge columns in hospitalization table")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  hospitalization_bounds <- hospitalization_df |>
    dplyr::select("hospitalization_id", "admission_dttm", "discharge_dttm")
  joined <- dplyr::inner_join(target_df, hospitalization_bounds, by = "hospitalization_id")

  violations_by_column <- list()

  for (time_column in time_columns) {
    if (!time_column %in% names(target_df)) {
      next
    }
    applicable_rows <- !is.na(joined[[time_column]]) &
      !is.na(joined$admission_dttm) & !is.na(joined$discharge_dttm)

    total_joined <- sum(applicable_rows)
    before_admission <- sum(joined[[time_column]][applicable_rows] < joined$admission_dttm[applicable_rows])
    after_discharge <- sum(joined[[time_column]][applicable_rows] > joined$discharge_dttm[applicable_rows])
    violation_total <- before_admission + after_discharge
    violation_percent <- if (total_joined > 0) violation_total / total_joined * 100 else 0

    violations_by_column[[time_column]] <- list(
      total_joined = as.integer(total_joined),
      before_admission = as.integer(before_admission),
      after_discharge = as.integer(after_discharge),
      violation_count = as.integer(violation_total),
      violation_percent = py_round(violation_percent, 2)
    )

    violation_message <- sprintf(
      "Column '%s': %s/%s records (%s%%) outside admission-to-discharge window (%s before admission, %s after discharge)",
      time_column, py_int_str(violation_total), py_int_str(total_joined), py_pct1(violation_percent),
      py_int_str(before_admission), py_int_str(after_discharge)
    )
    violation_details <- list(
      column = time_column, before_admission = as.integer(before_admission),
      after_discharge = as.integer(after_discharge), percent = py_round(violation_percent, 2)
    )

    if (violation_percent > error_threshold) {
      result <- add_error(result, violation_message, violation_details)
    } else if (violation_percent > warning_threshold) {
      result <- add_warning(result, violation_message, violation_details)
    } else {
      result <- add_info(
        result,
        sprintf("Column '%s': all records within admission-to-discharge window", time_column),
        list(column = time_column, total_joined = as.integer(total_joined))
      )
    }
  }

  result$metrics$time_columns_checked <- as.list(names(violations_by_column))
  result$metrics$violations_by_column <- violations_by_column
  result$atomic_total <- max(1L, length(violations_by_column))
  result$atomic_passed <- result$atomic_total - length(result$errors)
  result
}

#' P.5 Check for overlapping time periods within an entity
#'
#' @param df Data to validate.
#' @param table_name Name of the table.
#' @param entity_col Column identifying the entity, e.g. `"hospitalization_id"`.
#' @param start_col Period start column.
#' @param end_col Period end column.
#' @return A `dqa_plausibility_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_overlapping_periods(adt_data, "adt")
#' }
check_overlapping_periods <- function(df, table_name, entity_col = "hospitalization_id",
                                      start_col = "in_dttm", end_col = "out_dttm") {
  result <- dqa_plausibility_result("overlapping_periods", table_name)

  if (!entity_col %in% names(df) || !start_col %in% names(df) || !end_col %in% names(df)) {
    result <- add_info(result, sprintf(
      "Required columns (%s, %s, %s) not all present", entity_col, start_col, end_col
    ))
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  filtered <- df |>
    dplyr::filter(!is.na(.data[[start_col]]), !is.na(.data[[end_col]])) |>
    dplyr::arrange(.data[[entity_col]], .data[[start_col]])

  total_records <- nrow(filtered)
  entities_checked <- dplyr::n_distinct(filtered[[entity_col]])

  overlap_count <- 0
  if (total_records > 0) {
    with_previous <- filtered |>
      dplyr::group_by(.data[[entity_col]]) |>
      dplyr::mutate(previous_end = dplyr::lag(.data[[end_col]])) |>
      dplyr::ungroup()
    overlap_count <- sum(
      !is.na(with_previous$previous_end) & with_previous[[start_col]] < with_previous$previous_end
    )
  }
  overlap_percent <- if (total_records > 0) overlap_count / total_records * 100 else 0

  result$metrics$total_records <- as.integer(total_records)
  result$metrics$entities_checked <- as.integer(entities_checked)
  result$metrics$overlapping_records <- as.integer(overlap_count)
  result$metrics$overlap_percent <- py_round(overlap_percent, 2)

  if (overlap_count > 0) {
    result <- add_warning(
      result,
      sprintf(
        "%s overlapping time periods detected (%s%% of records)",
        py_int_str(overlap_count), py_pct1(overlap_percent)
      ),
      list(column = paste0(start_col, ", ", end_col),
           overlapping_records = as.integer(overlap_count), percent = py_round(overlap_percent, 2))
    )
  } else {
    result <- add_info(
      result,
      sprintf(
        "No overlapping time periods detected for %s on %s/%s — %s records checked",
        entity_col, start_col, end_col, py_int_comma(total_records)
      ),
      list(column = paste0(start_col, ", ", end_col), entity_col = entity_col,
           records_checked = as.integer(total_records), entities_checked = as.integer(entities_checked))
    )
  }

  result$atomic_total <- 1L
  result$atomic_passed <- if (length(result$errors) == 0) 1L else 0L
  result
}

# Auto-detect the primary datetime column for a table.
detect_time_column <- function(column_names, table_name) {
  candidates <- c(
    "recorded_dttm", "admin_dttm", "admission_dttm", "lab_result_dttm",
    "in_dttm", "procedure_billed_dttm", "result_dttm", "start_dttm"
  )
  matched <- candidates[candidates %in% column_names]
  if (length(matched) == 0) NULL else matched[1]
}

# Extract a {category_value: unit} map from schema *_units keys.
get_schema_units <- function(schema, category_column) {
  unit_keys <- grep("_units$", names(schema), value = TRUE)
  for (unit_key in unit_keys) {
    mapping <- schema[[unit_key]]
    if (!is.list(mapping) || length(mapping) == 0) {
      next
    }
    unit_map <- lapply(mapping, function(unit_value) {
      if (length(unit_value) > 1) as.character(unlist(unit_value)[1]) else as.character(unit_value)
    })
    if (length(unit_map) > 0) {
      return(unit_map)
    }
  }
  NULL
}

#' P.6 Check category distribution consistency over time
#'
#' Emits one row per (category column, distinct value): a warning when the
#' value is absent from some years, an info row when it is present in all of
#' them. `organism_category` stays informational even for absences, since it
#' would otherwise flood the warning list.
#'
#' @inheritParams check_required_columns
#' @param time_column Optional datetime column to bucket by; auto-detected when
#'   `NULL`.
#' @param hosp_years Optional set of hospitalization years for context.
#' @return A `dqa_plausibility_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_category_temporal_consistency(labs_data, load_schema("labs"), "labs")
#' }
check_category_temporal_consistency <- function(df, schema, table_name,
                                                time_column = NULL, hosp_years = NULL) {
  result <- dqa_plausibility_result("category_temporal_consistency", table_name)

  column_names <- names(df)
  if (is.null(time_column)) {
    time_column <- detect_time_column(column_names, table_name)
  }
  if (is.null(time_column) || !time_column %in% column_names) {
    result <- add_info(result, "No suitable datetime column found for temporal consistency check")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  category_columns <- unlist(schema$category_columns %||% character(0), use.names = FALSE)
  present_category_columns <- intersect(category_columns, column_names)
  if (length(present_category_columns) == 0) {
    result <- add_info(result, "No category columns found for temporal consistency check")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  id_column <- if ("hospitalization_id" %in% column_names) {
    "hospitalization_id"
  } else if ("patient_id" %in% column_names) {
    "patient_id"
  } else {
    NULL
  }

  yearly_distributions <- list()
  missing_in_years <- list()
  monthly_trends <- list()
  # clifpy keys its absent-value map by str(value) but looks it up with the raw
  # value, so on a non-string category column the lookup never hits and every
  # value is reported as present. Tracked here to reproduce that behaviour.
  category_column_is_character <- list()

  for (category_column in present_category_columns) {
    category_column_is_character[[category_column]] <- is.character(df[[category_column]])
    applicable <- df |>
      dplyr::filter(!is.na(.data[[time_column]]), !is.na(.data[[category_column]]))
    if (nrow(applicable) == 0) {
      next
    }

    applicable <- applicable |>
      dplyr::mutate(
        observation_year = as.integer(format(.data[[time_column]], "%Y")),
        observation_month = format(.data[[time_column]], "%Y-%m")
      )

    yearly <- if (!is.null(id_column)) {
      applicable |>
        dplyr::group_by(.data$observation_year, .data[[category_column]]) |>
        dplyr::summarise(unique_ids = dplyr::n_distinct(.data[[id_column]]), .groups = "drop")
    } else {
      applicable |>
        dplyr::group_by(.data$observation_year, .data[[category_column]]) |>
        dplyr::summarise(unique_ids = dplyr::n(), .groups = "drop")
    }

    distribution <- list()
    for (row_index in seq_len(nrow(yearly))) {
      year_key <- as.character(yearly$observation_year[row_index])
      distribution[[year_key]] <- c(
        distribution[[year_key]] %||% list(),
        stats::setNames(list(yearly$unique_ids[row_index]), yearly[[category_column]][row_index])
      )
    }
    yearly_distributions[[category_column]] <- distribution

    all_years <- sort(as.integer(names(distribution)))
    all_values <- unique(unlist(lapply(distribution, names), use.names = FALSE))

    if (length(all_years) >= 2) {
      absent_values <- list()
      for (category_value in all_values) {
        absent_years <- all_years[vapply(
          as.character(all_years),
          function(year_key) is.null(distribution[[year_key]][[category_value]]),
          logical(1)
        )]
        if (length(absent_years) > 0 && length(absent_years) < length(all_years)) {
          absent_values[[category_value]] <- as.list(absent_years)
        }
      }
      if (length(absent_values) > 0) {
        missing_in_years[[category_column]] <- absent_values
      }
    }

    # Monthly trends, exported by the report generators.
    numeric_column <- category_to_numeric_column(table_name, category_column)
    unit_column <- CATEGORY_UNIT_COL_MAP[[paste0(table_name, "|", category_column)]]
    grouping_columns <- c("observation_month", category_column)
    if (!is.null(unit_column) && unit_column %in% column_names) {
      grouping_columns <- c(grouping_columns, unit_column)
    }

    monthly <- applicable |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_columns))) |>
      dplyr::summarise(
        n = if (!is.null(id_column)) dplyr::n_distinct(.data[[id_column]]) else dplyr::n(),
        avg = if (!is.null(numeric_column) && numeric_column %in% column_names) {
          mean(.data[[numeric_column]], na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(grouping_columns))) |>
      dplyr::rename(month_year = "observation_month")

    if (all(is.na(monthly$avg))) {
      monthly$avg <- NULL
    }

    if (!is.null(numeric_column) && numeric_column %in% column_names &&
        !(!is.null(unit_column) && unit_column %in% column_names)) {
      schema_unit_map <- get_schema_units(schema, category_column)
      if (!is.null(schema_unit_map)) {
        monthly$unit <- unlist(lapply(monthly[[category_column]], function(category_value) {
          schema_unit_map[[category_value]] %||% NA_character_
        }), use.names = FALSE)
      }
    }

    monthly_trends[[category_column]] <- monthly
  }

  result$metrics$category_columns_checked <- length(present_category_columns)
  result$metrics$yearly_distributions <- yearly_distributions
  result$metrics$missing_in_years <- missing_in_years
  result$metrics$monthly_trends <- monthly_trends

  atomic_total <- 0
  for (category_column in present_category_columns) {
    distribution <- yearly_distributions[[category_column]] %||% list()
    all_years <- sort(as.integer(names(distribution)))
    total_years <- length(all_years)
    all_values <- unique(unlist(lapply(distribution, names), use.names = FALSE))
    atomic_total <- atomic_total + length(all_values)

    if (total_years == 0 || length(all_values) == 0) {
      result <- add_info(result, sprintf("No temporal data for %s", category_column),
                         list(column = category_column))
      next
    }

    year_range <- sprintf("%d-%d", all_years[1], all_years[total_years])
    absent_for_column <- if (isTRUE(category_column_is_character[[category_column]])) {
      missing_in_years[[category_column]] %||% list()
    } else {
      list()
    }

    # Python's sorted(..., key=str) orders by code point; radix sorting gives
    # the same C-locale ordering regardless of the session's collation.
    for (category_value in sort(all_values, method = "radix")) {
      yearly_counts <- stats::setNames(
        lapply(as.character(all_years), function(year_key) {
          distribution[[year_key]][[category_value]] %||% 0L
        }),
        as.character(all_years)
      )
      present_year_count <- sum(vapply(yearly_counts, function(count) count > 0, logical(1)))

      if (!is.null(absent_for_column[[category_value]])) {
        absent_year_count <- total_years - present_year_count
        message_text <- sprintf(
          "%s: %s absent in %d/%d years (%s)",
          category_column, category_value, absent_year_count, total_years, year_range
        )
        message_details <- list(
          column = category_column, value = category_value,
          absent_years = absent_for_column[[category_value]],
          total_years = total_years, yearly_counts = yearly_counts
        )
        if (identical(category_column, "organism_category")) {
          result <- add_info(result, message_text, message_details)
        } else {
          result <- add_warning(result, message_text, message_details)
        }
      } else {
        result <- add_info(
          result,
          sprintf(
            "%s: %s present in %d/%d years (%s)",
            category_column, category_value, present_year_count, total_years, year_range
          ),
          list(column = category_column, value = category_value, yearly_counts = yearly_counts)
        )
      }
    }
  }

  result$atomic_total <- atomic_total
  result$atomic_passed <- atomic_total
  result
}

# Reverse of CATEGORY_COLUMN_MAP: (table, category_col) -> numeric col, or NULL
# when several numeric columns share the same category column (averaging is
# then meaningless).
category_to_numeric_column <- function(table_name, category_column) {
  column_map <- CATEGORY_COLUMN_MAP[[table_name]] %||% list()
  matched_numeric_columns <- character(0)
  for (numeric_column in names(column_map)) {
    category_info <- column_map[[numeric_column]]
    if (identical(category_info[1], category_column)) {
      matched_numeric_columns <- c(matched_numeric_columns, numeric_column)
    }
  }
  if (length(matched_numeric_columns) == 1) matched_numeric_columns else NULL
}

#' P.7 Check for duplicate composite keys
#'
#' @param df Data to validate.
#' @param table_name Name of the table.
#' @param composite_keys Optional character vector of key columns; defaults to
#'   the schema's or `validation_rules.yaml`'s definition.
#' @param schema Optional parsed schema, consulted for `composite_keys`.
#' @param warning_threshold Duplicate percent above which a warning is raised.
#' @param error_threshold Duplicate percent above which an error is raised.
#' @return A `dqa_plausibility_result`.
#' @export
#'
#' @examples
#' \dontrun{
#' check_duplicate_composite_keys(labs_data, "labs")
#' }
check_duplicate_composite_keys <- function(df, table_name, composite_keys = NULL, schema = NULL,
                                           warning_threshold = 0.0, error_threshold = 10.0) {
  result <- dqa_plausibility_result("duplicate_composite_keys", table_name)

  if (is.null(composite_keys)) {
    composite_keys <- get_composite_keys(table_name, schema)
  }
  if (length(composite_keys) == 0) {
    result <- add_info(result, "No composite keys defined for this table")
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  missing_keys <- setdiff(composite_keys, names(df))
  if (length(missing_keys) > 0) {
    result <- add_info(result, sprintf("Composite key columns missing: %s", py_list_repr(missing_keys)))
    result$atomic_total <- 0L
    result$atomic_passed <- 0L
    return(result)
  }

  total_records <- nrow(df)
  unique_records <- nrow(dplyr::distinct(dplyr::select(df, dplyr::all_of(composite_keys))))
  duplicate_records <- total_records - unique_records
  duplicate_percent <- if (total_records > 0) duplicate_records / total_records * 100 else 0

  result$metrics$composite_keys <- as.list(composite_keys)
  result$metrics$total_records <- as.integer(total_records)
  result$metrics$unique_records <- as.integer(unique_records)
  result$metrics$duplicate_records <- as.integer(duplicate_records)
  result$metrics$duplicate_percent <- py_round(duplicate_percent, 2)

  duplicate_message <- sprintf(
    "%s duplicate records (%s%%) on composite key (%s) in %s",
    py_int_str(duplicate_records), py_pct1(duplicate_percent),
    paste(composite_keys, collapse = ", "), table_name
  )
  duplicate_details <- list(
    duplicate_records = as.integer(duplicate_records),
    percent = py_round(duplicate_percent, 2),
    keys = as.list(composite_keys)
  )

  if (duplicate_percent > error_threshold) {
    result <- add_error(result, duplicate_message, duplicate_details)
  } else if (duplicate_percent > warning_threshold) {
    result <- add_warning(result, duplicate_message, duplicate_details)
  } else {
    result <- add_info(result, "No duplicate composite keys found")
  }

  result$atomic_total <- 1L
  result$atomic_passed <- if (length(result$errors) == 0) 1L else 0L
  result
}


# ---------------------------------------------------------------------------
# ORCHESTRATION
# ---------------------------------------------------------------------------

# Merge caller-supplied plausibility thresholds over the defaults.
merge_plausibility_thresholds <- function(plausibility_thresholds = NULL) {
  thresholds <- DEFAULT_PLAUSIBILITY_THRESHOLDS
  if (!is.null(plausibility_thresholds)) {
    for (check_name in names(plausibility_thresholds)) {
      overrides <- plausibility_thresholds[[check_name]]
      if (!is.null(thresholds[[check_name]])) {
        thresholds[[check_name]][names(overrides)] <- overrides
      } else {
        thresholds[[check_name]] <- overrides
      }
    }
  }
  thresholds
}

#' Run all conformance checks on a table
#'
#' @inheritParams check_required_columns
#' @return A named list of `dqa_conformance_result` objects, keyed by check type.
#' @export
#'
#' @examples
#' \dontrun{
#' run_conformance_checks(patient_data, load_schema("patient"), "patient")
#' }
run_conformance_checks <- function(df, schema, table_name) {
  df <- normalize_for_validation(df)

  results <- list()
  results$table_presence <- check_table_presence(df, table_name)
  results$required_columns <- check_required_columns(df, schema, table_name)
  results$column_dtypes <- check_column_dtypes(df, schema, table_name)
  results$datetime_format <- check_datetime_format(df, schema, table_name)
  if (identical(table_name, "labs")) {
    results$lab_reference_units <- check_lab_reference_units(df, schema, table_name)
  }
  results$categorical_values <- check_categorical_values(df, schema, table_name)
  results$category_group_mapping <- check_category_group_mapping(df, schema, table_name)
  results
}

#' Run all completeness checks on a table
#'
#' @inheritParams check_missingness
#' @return A named list of `dqa_completeness_result` objects, keyed by check type.
#' @export
#'
#' @examples
#' \dontrun{
#' run_completeness_checks(patient_data, load_schema("patient"), "patient")
#' }
run_completeness_checks <- function(df, schema, table_name,
                                    error_threshold = 50.0, warning_threshold = 10.0) {
  df <- normalize_for_validation(df)

  results <- list()
  results$missingness <- check_missingness(df, schema, table_name, error_threshold, warning_threshold)
  results$conditional_requirements <- check_conditional_requirements(df, table_name)
  results$mcide_value_coverage <- check_mcide_value_coverage(df, schema, table_name)
  results
}

#' Run relational integrity checks across loaded tables
#'
#' Reads the foreign-key rules from `validation_rules.yaml` and runs
#' [check_relational_integrity()] for every applicable (table, FK column) pair.
#' Self-references and unloaded reference tables are skipped.
#'
#' @param tables A list of objects with `table_name`, `df` and (optionally)
#'   `schema` fields — typically [BaseTable] instances.
#' @return A nested named list: `results[[table_name]][[fk_column]]`.
#' @export
#'
#' @examples
#' \dontrun{
#' run_relational_integrity_checks(list(labs_table, hospitalization_table))
#' }
run_relational_integrity_checks <- function(tables) {
  lookup <- list()
  for (table_object in tables) {
    normalized_data <- normalize_for_validation(table_object$df)
    table_schema <- table_object$schema %||% list()
    schema_columns <- if (!is.null(table_schema$columns)) {
      vapply(table_schema$columns, function(spec) spec$name, character(1))
    } else {
      NULL
    }
    lookup[[table_object$table_name]] <- list(df = normalized_data, schema_columns = schema_columns)
  }

  foreign_key_rules <- load_validation_rules()$relational_integrity %||% list()
  results <- list()

  for (table_name in names(lookup)) {
    entry <- lookup[[table_name]]
    column_names <- entry$schema_columns %||% names(entry$df)

    for (fk_column in names(foreign_key_rules)) {
      if (!fk_column %in% column_names) {
        next
      }
      reference_table_name <- foreign_key_rules[[fk_column]]$references_table
      if (identical(reference_table_name, table_name)) {
        next
      }
      if (is.null(lookup[[reference_table_name]])) {
        next
      }

      results[[table_name]][[fk_column]] <- check_relational_integrity(
        target_df = entry$df,
        reference_df = lookup[[reference_table_name]]$df,
        target_table = table_name,
        reference_table = reference_table_name,
        key_column = fk_column
      )
    }
  }

  results
}

#' Run all single-table plausibility checks
#'
#' @inheritParams check_required_columns
#' @param hosp_years Optional set of hospitalization years for P.6 context.
#' @param plausibility_thresholds Optional named list of per-check threshold
#'   overrides, e.g. `list(chronological_order = list(error_threshold = 5))`.
#' @return A named list of `dqa_plausibility_result` objects, keyed by check type.
#' @export
#'
#' @examples
#' \dontrun{
#' run_plausibility_checks(adt_data, load_schema("adt"), "adt")
#' }
run_plausibility_checks <- function(df, schema, table_name,
                                    hosp_years = NULL, plausibility_thresholds = NULL) {
  thresholds <- merge_plausibility_thresholds(plausibility_thresholds)
  df <- normalize_for_validation(df)

  results <- list()

  results$chronological_order <- check_chronological_order(
    df, table_name,
    warning_threshold = thresholds$chronological_order$warning_threshold,
    error_threshold = thresholds$chronological_order$error_threshold
  )
  results$numeric_range_plausibility <- check_numeric_range_plausibility(
    df, table_name,
    warning_threshold = thresholds$numeric_range_plausibility$warning_threshold,
    error_threshold = thresholds$numeric_range_plausibility$error_threshold
  )
  results$field_plausibility <- check_field_plausibility(df, table_name)

  if (table_name %in% c("medication_admin_continuous", "medication_admin_intermittent")) {
    results$medication_dose_unit_consistency <- check_medication_dose_unit_consistency(
      df, table_name,
      warning_threshold = thresholds$medication_dose_unit_consistency$warning_threshold,
      error_threshold = thresholds$medication_dose_unit_consistency$error_threshold
    )
  }

  overlap_rules <- load_validation_rules()$overlapping_periods[[table_name]]
  if (!is.null(overlap_rules) && length(overlap_rules) > 0) {
    results$overlapping_periods <- check_overlapping_periods(
      df, table_name,
      entity_col = overlap_rules$entity_column %||% "hospitalization_id",
      start_col = overlap_rules$start_column %||% "in_dttm",
      end_col = overlap_rules$end_column %||% "out_dttm"
    )
  }

  results$category_temporal_consistency <- check_category_temporal_consistency(
    df, schema, table_name, hosp_years = hosp_years
  )
  results$duplicate_composite_keys <- check_duplicate_composite_keys(
    df, table_name, schema = schema,
    warning_threshold = thresholds$duplicate_composite_keys$warning_threshold,
    error_threshold = thresholds$duplicate_composite_keys$error_threshold
  )

  results
}

#' Run cross-table plausibility checks
#'
#' @param tables A list of objects with `table_name` and `df` fields.
#' @param plausibility_thresholds Optional named list of per-check threshold overrides.
#' @return A nested named list: `results[[table_name]]$cross_table_temporal`.
#' @export
#'
#' @examples
#' \dontrun{
#' run_cross_table_plausibility_checks(list(labs_table, hospitalization_table))
#' }
run_cross_table_plausibility_checks <- function(tables, plausibility_thresholds = NULL) {
  thresholds <- merge_plausibility_thresholds(plausibility_thresholds)

  lookup <- list()
  for (table_object in tables) {
    lookup[[table_object$table_name]] <- normalize_for_validation(table_object$df)
  }
  if (is.null(lookup[["hospitalization"]])) {
    return(list())
  }

  hospitalization_data <- lookup[["hospitalization"]]
  results <- list()

  for (table_name in names(lookup)) {
    if (identical(table_name, "hospitalization")) {
      next
    }
    time_columns <- CROSS_TABLE_TIME_COLUMNS[[table_name]]
    if (is.null(time_columns)) {
      next
    }
    table_data <- lookup[[table_name]]
    available_time_columns <- intersect(time_columns, names(table_data))
    if (length(available_time_columns) == 0 || !"hospitalization_id" %in% names(table_data)) {
      next
    }

    results[[table_name]]$cross_table_temporal <- check_cross_table_temporal_plausibility(
      table_data, hospitalization_data, table_name, available_time_columns,
      warning_threshold = thresholds$cross_table_temporal$warning_threshold,
      error_threshold = thresholds$cross_table_temporal$error_threshold
    )
  }

  results
}

#' Run cross-table conditional completeness checks (K.5)
#'
#' For each rule in `cross_table_conditional_requirements`, computes the set of
#' join-column IDs that satisfy the source condition but lack the required
#' target column value.
#'
#' @param tables A list of objects with `table_name` and `df` fields.
#' @return A nested named list keyed by **target** table name, then rule key.
#' @export
#'
#' @examples
#' \dontrun{
#' run_cross_table_completeness_checks(list(hospitalization_table, patient_table))
#' }
run_cross_table_completeness_checks <- function(tables) {
  conditional_rules <- load_validation_rules()$cross_table_conditional_requirements %||% list()
  if (length(conditional_rules) == 0) {
    return(list())
  }

  lookup <- list()
  for (table_object in tables) {
    table_name <- sub("^clif_", "", table_object$table_name %||% "")
    lookup[[table_name]] <- normalize_for_validation(table_object$df)
  }

  results <- list()

  for (rule in conditional_rules) {
    rule_key <- paste0(rule$source_column, "_", rule$target_column)
    source_table <- rule$source_table
    target_table <- rule$target_table
    join_column <- rule$join_column

    if (is.null(lookup[[source_table]]) || is.null(lookup[[target_table]])) {
      next
    }
    source_data <- lookup[[source_table]]
    target_data <- lookup[[target_table]]

    if (!rule$source_column %in% names(source_data) || !join_column %in% names(source_data)) {
      next
    }
    if (!rule$target_column %in% names(target_data) || !join_column %in% names(target_data)) {
      next
    }

    match_values <- trimws(tolower(as.character(unlist(rule$source_value, use.names = FALSE))))
    source_mask <- trimws(tolower(as.character(source_data[[rule$source_column]]))) %in% match_values
    source_mask[is.na(source_mask)] <- FALSE
    source_ids <- unique(stats::na.omit(source_data[[join_column]][source_mask]))

    target_values <- target_data[[rule$target_column]]
    target_mask <- !is.na(target_values)
    if (is.character(target_values)) {
      target_mask <- target_mask & trimws(as.character(target_values)) != ""
    }
    target_ids <- unique(stats::na.omit(target_data[[join_column]][target_mask]))

    result <- dqa_completeness_result(
      "cross_table_conditional_completeness",
      paste0(source_table, "->", target_table)
    )

    if (length(source_ids) == 0) {
      result <- add_info(result, sprintf(
        "No %s = %s found in %s; cross-table conditional check not triggered",
        rule$source_column, py_list_repr(unlist(rule$source_value, use.names = FALSE)), source_table
      ))
      result$atomic_total <- 0L
      result$atomic_passed <- 0L
      results[[target_table]][[rule_key]] <- result
      next
    }

    missing_ids <- setdiff(source_ids, target_ids)
    total_matching <- length(source_ids)
    missing_count <- length(missing_ids)
    percent_missing <- if (total_matching > 0) py_round(missing_count / total_matching * 100, 1) else 0

    result$metrics$total_matching_source <- total_matching
    result$metrics$missing_in_target <- missing_count
    result$metrics$coverage_percent <- py_round(100 - percent_missing, 2)

    if (missing_count > 0) {
      result <- add_warning(
        result,
        sprintf(
          "%d/%d patients discharged as %s in %s are missing %s in %s (%s%% missing)",
          missing_count, total_matching,
          py_list_repr(unlist(rule$source_value, use.names = FALSE)),
          source_table, rule$target_column, target_table, py_num_str(percent_missing)
        ),
        list(
          column = rule$target_column,
          missing_count = missing_count,
          total_matching = total_matching,
          percent_missing = percent_missing,
          sample_ids = as.list(utils::head(sort(missing_ids), 10)),
          source_condition = sprintf(
            "%s in %s", rule$source_column,
            py_list_repr(unlist(rule$source_value, use.names = FALSE))
          )
        )
      )
    } else {
      result <- add_info(result, sprintf(
        "All %d patients discharged as %s in %s have %s in %s",
        total_matching, py_list_repr(unlist(rule$source_value, use.names = FALSE)),
        source_table, rule$target_column, target_table
      ))
    }

    result$atomic_total <- 1L
    result$atomic_passed <- if (length(result$errors) == 0) 1L else 0L
    results[[target_table]][[rule_key]] <- result
  }

  results
}

#' Run the complete DQA suite on a single table
#'
#' Orchestrates conformance, completeness and plausibility checks, plus — when
#' `tables` is supplied — auto-detected relational integrity and cross-table
#' plausibility checks.
#'
#' @param df The data to validate.
#' @param schema Parsed table schema. When `NULL`, it is loaded from the
#'   packaged schemas using `table_name` and `clif_version`.
#' @param table_name Name of the table. Required.
#' @param tables Optional list of objects with `table_name` and `df` fields.
#' @param error_threshold Percent missing above which an error is raised.
#' @param warning_threshold Percent missing above which a warning is raised.
#' @param hosp_years Optional set of hospitalization years for P.6 context.
#' @param plausibility_thresholds Optional named list of per-check threshold overrides.
#' @param clif_version CLIF schema version used when `schema` is `NULL`.
#' @return A named list with `table_name`, `backend`, `conformance`,
#'   `completeness`, `relational` and `plausibility` entries.
#' @export
#'
#' @examples
#' \dontrun{
#' run_full_dqa(patient_data, table_name = "patient", clif_version = "3.0")
#' }
run_full_dqa <- function(df, schema = NULL, table_name = "", tables = NULL,
                         error_threshold = 50.0, warning_threshold = 10.0,
                         hosp_years = NULL, plausibility_thresholds = NULL,
                         clif_version = DEFAULT_CLIF_VERSION) {
  if (!nzchar(table_name)) {
    cli::cli_abort("{.arg table_name} is required")
  }
  if (is.null(schema)) {
    schema <- load_schema(table_name, clif_version)
    if (is.null(schema)) {
      cli::cli_abort(
        "No built-in schema found for table {.val {table_name}} (CLIF {clif_version}). Pass a schema explicitly."
      )
    }
  }

  results <- list(
    table_name = table_name,
    backend = "duckdb",
    conformance = list(),
    completeness = list(),
    relational = list(),
    plausibility = list()
  )

  results$conformance <- lapply(run_conformance_checks(df, schema, table_name), to_list)
  results$completeness <- lapply(
    run_completeness_checks(df, schema, table_name, error_threshold, warning_threshold),
    to_list
  )

  if (!is.null(tables)) {
    relational_results <- run_relational_integrity_checks(tables)
    if (!is.null(relational_results[[table_name]])) {
      results$relational <- lapply(relational_results[[table_name]], to_list)
    }
  }

  if (is.null(hosp_years) && !is.null(tables)) {
    for (table_object in tables) {
      normalized_name <- sub("^clif_", "", table_object$table_name %||% "")
      if (identical(normalized_name, "hospitalization")) {
        hospitalization_data <- table_object$df
        if ("admission_dttm" %in% names(hospitalization_data)) {
          admission_values <- hospitalization_data$admission_dttm
          hosp_years <- unique(as.integer(format(
            admission_values[!is.na(admission_values)], "%Y"
          )))
        }
        break
      }
    }
  }

  results$plausibility <- lapply(
    run_plausibility_checks(
      df, schema, table_name,
      hosp_years = hosp_years, plausibility_thresholds = plausibility_thresholds
    ),
    to_list
  )

  if (!is.null(tables)) {
    cross_table_results <- run_cross_table_plausibility_checks(
      tables, plausibility_thresholds = plausibility_thresholds
    )
    if (!is.null(cross_table_results[[table_name]])) {
      for (check_name in names(cross_table_results[[table_name]])) {
        results$plausibility[[check_name]] <- to_list(cross_table_results[[table_name]][[check_name]])
      }
    }
  }

  results
}


# ---------------------------------------------------------------------------
# SCHEMA-ONLY CHECK ACCOUNTING
# ---------------------------------------------------------------------------

# Count atomic (col, [cat], [unit]) range leaves in outlier_config.yaml,
# mirroring the structure check_numeric_range_plausibility iterates.
count_numeric_range_leaves <- function(table_name) {
  table_config <- load_validator_outlier_config()$tables[[table_name]] %||% list()
  if (length(table_config) == 0) {
    return(0L)
  }

  leaf_count <- 0L
  for (column_ranges in table_config) {
    if (!is.list(column_ranges)) {
      next
    }
    if (!is.null(column_ranges$min) && !is.null(column_ranges$max)) {
      leaf_count <- leaf_count + 1L
      next
    }
    for (inner in column_ranges) {
      if (!is.list(inner)) {
        next
      }
      if (!is.null(inner$min)) {
        leaf_count <- leaf_count + 1L
      } else {
        for (ranges in inner) {
          if (is.list(ranges) && !is.null(ranges$min)) {
            leaf_count <- leaf_count + 1L
          }
        }
      }
    }
  }
  leaf_count
}

#' Expected atomic DQA check counts from schema and config alone
#'
#' Computes the per-category atomic check count for a table without needing any
#' data, so present and absent tables report comparable `N/N` denominators.
#'
#' @param table_name snake_case CLIF table name.
#' @param clif_version CLIF schema version.
#' @return A named list with `conformance`, `completeness` and `plausibility`
#'   counts.
#' @export
#'
#' @examples
#' get_schema_check_counts("patient", "3.0")
get_schema_check_counts <- function(table_name, clif_version = DEFAULT_CLIF_VERSION) {
  schema <- suppressWarnings(load_schema(table_name, clif_version))
  if (is.null(schema)) {
    return(list(conformance = 0L, completeness = 0L, plausibility = 0L))
  }

  column_specs <- schema$columns %||% list()
  category_columns <- unlist(schema$category_columns %||% character(0), use.names = FALSE)
  schema_column_names <- vapply(column_specs, function(spec) spec$name, character(1))
  required_columns <- unlist(schema$required_columns %||% character(0), use.names = FALSE)
  validation_rules <- load_validation_rules()

  # --- Conformance ---
  conformance_count <- 1L  # C.1 table_presence
  conformance_count <- conformance_count + length(required_columns)  # C.2
  conformance_count <- conformance_count +
    sum(vapply(column_specs, function(spec) !is.null(spec$data_type), logical(1)))  # C.3
  conformance_count <- conformance_count +
    sum(vapply(column_specs, function(spec) {
      !is.null(spec$data_type) && spec$data_type %in% c("DATETIME", "DATE")
    }, logical(1)))  # C.4
  conformance_count <- conformance_count +
    sum(vapply(column_specs, function(spec) {
      spec$name %in% category_columns && length(spec$permissible_values %||% list()) > 0
    }, logical(1)))  # C.5
  for (mapping_key in grep("_category_to_group_mapping$", names(schema), value = TRUE)) {
    conformance_count <- conformance_count + length(schema[[mapping_key]] %||% list())  # C.6
  }
  if (identical(table_name, "labs")) {
    conformance_count <- conformance_count + length(schema$lab_reference_units %||% list())  # C.7
  }

  # --- Completeness ---
  completeness_count <- length(required_columns)  # K.1
  completeness_count <- completeness_count + length(get_default_conditions(table_name))  # K.2
  for (column_spec in column_specs) {
    if (column_spec$name %in% category_columns) {
      completeness_count <- completeness_count + length(column_spec$permissible_values %||% list())  # K.3
    }
  }
  foreign_key_rules <- validation_rules$relational_integrity %||% list()
  for (fk_column in names(foreign_key_rules)) {
    if (fk_column %in% schema_column_names &&
        !identical(foreign_key_rules[[fk_column]]$references_table, table_name)) {
      completeness_count <- completeness_count + 1L  # K.4
    }
  }
  for (rule in validation_rules$cross_table_conditional_requirements %||% list()) {
    if (identical(rule$target_table, table_name)) {
      completeness_count <- completeness_count + 1L  # K.5
    }
  }

  # --- Plausibility ---
  plausibility_count <- length(validation_rules$chronological_order[[table_name]] %||% list())  # P.1
  plausibility_count <- plausibility_count + count_numeric_range_leaves(table_name)  # P.2
  plausibility_count <- plausibility_count +
    length(validation_rules$field_plausibility_rules[[table_name]] %||% list())  # P.3
  if (table_name %in% c("medication_admin_continuous", "medication_admin_intermittent")) {
    plausibility_count <- plausibility_count + 1L  # P.4
  }
  if (length(validation_rules$overlapping_periods[[table_name]] %||% list()) > 0) {
    plausibility_count <- plausibility_count + 1L  # P.5
  }
  datetime_columns <- vapply(
    Filter(function(spec) !is.null(spec$data_type) && spec$data_type %in% c("DATETIME", "DATE"), column_specs),
    function(spec) spec$name, character(1)
  )
  if (!is.null(detect_time_column(datetime_columns, table_name))) {
    plausibility_count <- plausibility_count + length(category_columns)  # P.6
  }
  composite_key_entry <- validation_rules$composite_keys[[table_name]]
  if (!is.null(composite_key_entry) && length(composite_key_entry$keys %||% list()) > 0) {
    plausibility_count <- plausibility_count + 1L  # P.7
  }
  if (!identical(table_name, "hospitalization") && "hospitalization_id" %in% schema_column_names) {
    for (time_column in CROSS_TABLE_TIME_COLUMNS[[table_name]] %||% character(0)) {
      if (time_column %in% schema_column_names) {
        plausibility_count <- plausibility_count + 1L  # P.8
      }
    }
  }

  list(
    conformance = as.integer(conformance_count),
    completeness = as.integer(completeness_count),
    plausibility = as.integer(plausibility_count)
  )
}

#' Build a `run_full_dqa()`-shaped result for a table the site did not submit
#'
#' An absent table is represented as a single error whose atomic footprint
#' covers the full expected conformance count, so per-site scoring stays
#' comparable: a site that submitted the table scores `N/N`, one that did not
#' scores `0/N`.
#'
#' @param table_name snake_case CLIF table name.
#' @param clif_version CLIF schema version.
#' @return A named list with the same top-level keys as [run_full_dqa()], plus
#'   `absent`, `total_rows` and `expected_check_counts`.
#' @export
#'
#' @examples
#' build_absent_table_dqa_result("microbiology_susceptibility", "3.0")
build_absent_table_dqa_result <- function(table_name, clif_version = DEFAULT_CLIF_VERSION) {
  expected_counts <- get_schema_check_counts(table_name, clif_version)
  expected_conformance <- as.integer(expected_counts$conformance)
  if (is.na(expected_conformance) || expected_conformance == 0) {
    expected_conformance <- 1L
  }

  table_presence <- list(
    check_type = "table_presence",
    table_name = table_name,
    passed = FALSE,
    errors = list(list(
      message = sprintf(
        "Table not present in dataset — %d conformance atoms could not be evaluated",
        expected_conformance
      ),
      details = list(atomic_count = expected_conformance, reason = "table_absent")
    )),
    warnings = list(),
    info = list(),
    metrics = list(row_count = 0L, column_count = 0L),
    atomic_total = expected_conformance,
    atomic_passed = 0L
  )

  list(
    table_name = table_name,
    backend = "absent",
    absent = TRUE,
    conformance = list(table_presence = table_presence),
    completeness = list(),
    relational = list(),
    plausibility = list(),
    total_rows = 0L,
    expected_check_counts = expected_counts
  )
}


# ---------------------------------------------------------------------------
# COMPATIBILITY LAYER
# ---------------------------------------------------------------------------

# Convert internal check names to the human-readable labels clifpy emits.
format_check_type <- function(check_name) {
  type_mapping <- list(
    required_columns = "Missing Required Columns",
    column_dtypes = "Data Type Mismatch",
    datetime_format = "Datetime Format Issue",
    lab_reference_units = "Lab Unit Mismatch",
    categorical_values = "Invalid Categorical Values",
    missingness = "High Missingness",
    conditional_requirements = "Conditional Requirement Violation",
    mcide_value_coverage = "mCIDE Coverage Gap",
    relational_integrity = "Relational Integrity",
    chronological_order = "Chronological Order Violation",
    numeric_range_plausibility = "Numeric Range Implausibility",
    field_plausibility = "Field Plausibility Violation",
    medication_dose_unit_consistency = "Medication Dose Unit Inconsistency",
    cross_table_temporal = "Cross-Table Temporal Implausibility",
    overlapping_periods = "Overlapping Time Periods",
    category_temporal_consistency = "Category Distribution Shift",
    duplicate_composite_keys = "Duplicate Composite Keys"
  )
  type_mapping[[check_name]] %||% title_case_words(gsub("_", " ", check_name))
}

title_case_words <- function(text) {
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "", collapse = " ")
}

#' Validate a data frame against a schema
#'
#' Runs the conformance, completeness and plausibility suites and flattens the
#' findings into a list of error records. This is the interface [BaseTable]'s
#' `validate()` consumes, and the one clifpy exposes for CLIF-TableOne.
#'
#' @param df Data to validate.
#' @param schema Parsed table schema.
#' @param table_name Optional table name; taken from the schema when omitted.
#' @param plausibility_thresholds Optional named list of per-check threshold overrides.
#' @return A list of error records, each a named list with `type`,
#'   `description`, `details`, `category` (`"schema"` or `"data_quality"`) and,
#'   for warnings, `severity`.
#' @export
#'
#' @examples
#' \dontrun{
#' validate_dataframe(patient_data, load_schema("patient", "3.0"), "patient")
#' }
validate_dataframe <- function(df, schema, table_name = NULL, plausibility_thresholds = NULL) {
  table_name <- table_name %||% schema$table_name %||% "unknown"
  errors <- list()

  # Schema-level checks (these affect the 'incomplete' status).
  schema_level_checks <- c("required_columns", "column_dtypes")

  collect_from <- function(errors, check_results, default_category) {
    for (check_name in names(check_results)) {
      check_result <- check_results[[check_name]]
      category <- if (is.null(default_category)) {
        if (check_name %in% schema_level_checks) "schema" else "data_quality"
      } else {
        default_category
      }
      for (error_entry in check_result$errors) {
        errors <- c(errors, list(list(
          type = format_check_type(check_name),
          description = error_entry$message,
          details = error_entry$details %||% list(),
          category = category
        )))
      }
      for (warning_entry in check_result$warnings) {
        errors <- c(errors, list(list(
          type = format_check_type(check_name),
          description = warning_entry$message,
          details = warning_entry$details %||% list(),
          category = "data_quality",
          severity = "warning"
        )))
      }
    }
    errors
  }

  errors <- collect_from(errors, run_conformance_checks(df, schema, table_name), NULL)
  errors <- collect_from(errors, run_completeness_checks(df, schema, table_name), "data_quality")
  errors <- collect_from(
    errors,
    run_plausibility_checks(df, schema, table_name, plausibility_thresholds = plausibility_thresholds),
    "data_quality"
  )

  errors
}

#' Format a validation error for display
#'
#' @param error An error record from [validate_dataframe()].
#' @param row_count Total row count of the table.
#' @param table_name Name of the table.
#' @return The error record with `table_name` and `row_count` attached.
#' @export
#'
#' @examples
#' format_clifpy_error(list(type = "High Missingness", description = "x"), 100, "patient")
format_clifpy_error <- function(error, row_count, table_name) {
  formatted <- list(
    type = error$type %||% "Unknown Error",
    description = error$description %||% paste(utils::capture.output(str(error)), collapse = " "),
    category = error$category %||% "other",
    details = error$details %||% list(),
    table_name = table_name,
    row_count = row_count
  )
  if (!is.null(error$severity)) {
    formatted$severity <- error$severity
  }
  formatted
}

#' Determine a table's validation status from its errors
#'
#' * `"incomplete"` — missing required columns, non-castable dtype errors, or
#'   100% null in a required column.
#' * `"partial"` — required columns present but error-severity data-quality
#'   issues remain.
#' * `"complete"` — no errors, or warnings only.
#'
#' @param errors A list of error records.
#' @param required_columns Optional character vector of required column names.
#' @param table_name Optional table name, for table-specific logic.
#' @return One of `"complete"`, `"partial"` or `"incomplete"`.
#' @export
#'
#' @examples
#' determine_validation_status(list())
determine_validation_status <- function(errors, required_columns = NULL, table_name = NULL) {
  if (length(errors) == 0) {
    return("complete")
  }

  for (error in errors) {
    error_type <- tolower(error$type %||% "")
    category <- error$category %||% ""
    details <- error$details %||% list()
    severity <- error$severity %||% "error"

    if (grepl("missing required columns", error_type, fixed = TRUE) ||
        !is.null(details$missing_columns)) {
      return("incomplete")
    }
    if (grepl("data type", error_type, fixed = TRUE) && identical(category, "schema")) {
      if (identical(details$castable, FALSE)) {
        return("incomplete")
      }
    }
    if (grepl("missingness", error_type, fixed = TRUE) && !identical(severity, "warning")) {
      percent_missing <- details$percent_missing %||% 0
      column_name <- details$column %||% ""
      if (percent_missing >= 100 && !is.null(required_columns) && column_name %in% required_columns) {
        return("incomplete")
      }
    }
  }

  has_errors <- any(vapply(errors, function(error) {
    identical(error$severity %||% "error", "error")
  }, logical(1)))

  if (has_errors) "partial" else "complete"
}

#' Split errors into status-affecting and informational buckets
#'
#' Used by report generators to separate critical errors from informational
#' messages.
#'
#' @param errors A named list with `schema_errors`, `data_quality_issues` and
#'   `other_errors` entries.
#' @param required_columns Character vector of required column names.
#' @param table_name Name of the table.
#' @param config_timezone Optional configured timezone, used to filter
#'   timezone-related errors.
#' @return A named list with `status_affecting` and `informational`, each
#'   containing the same three error buckets.
#' @export
#'
#' @examples
#' classify_errors_by_status_impact(
#'   list(schema_errors = list(), data_quality_issues = list(), other_errors = list()),
#'   character(0), "patient"
#' )
classify_errors_by_status_impact <- function(errors, required_columns, table_name,
                                             config_timezone = NULL) {
  empty_buckets <- list(schema_errors = list(), data_quality_issues = list(), other_errors = list())
  status_affecting <- empty_buckets
  informational <- empty_buckets

  optional_columns <- list(
    patient = c("race", "ethnicity", "language"),
    hospitalization = "discharge_category"
  )
  table_optional_columns <- optional_columns[[table_name]] %||% character(0)

  plausibility_keywords <- c(
    "chronological order", "numeric range", "field plausibility", "dose unit",
    "overlapping", "distribution shift", "duplicate composite", "cross-table temporal"
  )

  for (bucket_name in c("schema_errors", "data_quality_issues", "other_errors")) {
    for (error in errors[[bucket_name]] %||% list()) {
      error_type <- tolower(error$type %||% "")
      details <- error$details %||% list()
      description <- tolower(error$description %||% "")

      is_informational <- FALSE

      if (!is.null(config_timezone) && grepl("timezone", error_type, fixed = TRUE)) {
        if (grepl(tolower(config_timezone), description, fixed = TRUE)) {
          is_informational <- TRUE
        }
      }
      if (!is.null(details$column) && details$column %in% table_optional_columns) {
        is_informational <- TRUE
      }
      if (grepl("mcide", error_type, fixed = TRUE) || grepl("coverage", error_type, fixed = TRUE)) {
        is_informational <- TRUE
      }
      is_plausibility <- any(vapply(
        plausibility_keywords,
        function(keyword) grepl(keyword, error_type, fixed = TRUE),
        logical(1)
      ))
      if (is_plausibility && identical(error$severity, "warning")) {
        is_informational <- TRUE
      }
      if (identical(error$severity, "warning")) {
        is_informational <- TRUE
      }

      if (is_informational) {
        informational[[bucket_name]] <- c(informational[[bucket_name]], list(error))
      } else {
        status_affecting[[bucket_name]] <- c(status_affecting[[bucket_name]], list(error))
      }
    }
  }

  list(status_affecting = status_affecting, informational = informational)
}

#' Generate a text summary of validation results
#'
#' @param validation_results A named list with `status` and `errors` entries.
#' @return A human-readable multi-line summary string.
#' @export
#'
#' @examples
#' get_validation_summary(list(status = "complete", errors = list()))
get_validation_summary <- function(validation_results) {
  status <- validation_results$status %||% "unknown"
  errors <- validation_results$errors %||% list()

  schema_count <- length(errors$schema_errors %||% list())
  data_quality_count <- length(errors$data_quality_issues %||% list())
  other_count <- length(errors$other_errors %||% list())
  total_count <- schema_count + data_quality_count + other_count

  status_symbols <- list(complete = "✓", partial = "⚠", incomplete = "✗")
  summary_parts <- sprintf(
    "Status: %s %s", status_symbols[[status]] %||% "?", toupper(status)
  )

  if (total_count > 0) {
    summary_parts <- c(summary_parts, sprintf("Issues: %d total", total_count))
    if (schema_count > 0) {
      summary_parts <- c(summary_parts, sprintf("  - Schema: %d", schema_count))
    }
    if (data_quality_count > 0) {
      summary_parts <- c(summary_parts, sprintf("  - Data Quality: %d", data_quality_count))
    }
    if (other_count > 0) {
      summary_parts <- c(summary_parts, sprintf("  - Other: %d", other_count))
    }
  } else {
    summary_parts <- c(summary_parts, "No issues found")
  }

  paste(summary_parts, collapse = "\n")
}

#' Validate that datetime columns carry timezone information
#'
#' Reports one record per checked column. Columns that are not datetimes, or
#' that carry no timezone, are flagged.
#'
#' @param df Data to validate.
#' @param datetime_columns Character vector of column names to check.
#' @param expected_tz Timezone the site is configured to use.
#' @return A list of records with `type`, `column`, `status` and `message`.
#' @export
#'
#' @examples
#' validate_datetime_timezone(
#'   data.frame(recorded_dttm = as.POSIXct("2024-01-01", tz = "UTC")),
#'   "recorded_dttm"
#' )
validate_datetime_timezone <- function(df, datetime_columns, expected_tz = "UTC") {
  results <- list()

  for (column_name in datetime_columns) {
    if (!column_name %in% names(df)) {
      next
    }
    column_values <- df[[column_name]]

    if (!inherits(column_values, "POSIXct")) {
      results <- c(results, list(list(
        type = "timezone_validation",
        column = column_name,
        status = "error",
        message = sprintf("Column '%s' is not a datetime column", column_name)
      )))
      next
    }

    timezone_attribute <- attr(column_values, "tzone")
    if (is.null(timezone_attribute) || !nzchar(timezone_attribute)) {
      results <- c(results, list(list(
        type = "timezone_validation",
        column = column_name,
        status = "warning",
        message = sprintf("Column '%s' is timezone-naive, expected %s", column_name, expected_tz)
      )))
    } else {
      results <- c(results, list(list(
        type = "timezone_validation",
        column = column_name,
        status = "ok",
        message = sprintf("Column '%s' is timezone-aware (%s)", column_name, timezone_attribute)
      )))
    }
  }

  results
}
