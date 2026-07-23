#' Standardized DQA rule code registry and issue enrichment helpers
#'
#' Ported from `clifpy/utils/rule_codes.py`. Maps each (category, check_type)
#' pair to a stable rule code (e.g. `C.5`), provides human-readable "passed"
#' finding text, and enriches issue records with rule metadata for report
#' generation.
#'
#' @name clif-rule-codes
NULL

# (category, check_type) -> c(code, description). Keys are "category|check_type".
RULE_CODES <- list(
  "conformance|table_presence"                        = c("C.1", "Table presence verification"),
  "conformance|required_columns"                      = c("C.2", "Required columns presence check"),
  "conformance|column_dtypes"                         = c("C.3", "Column data type validation"),
  "conformance|datetime_format"                       = c("C.4", "Datetime format validation"),
  "conformance|categorical_values"                    = c("C.5", "Categorical values conformance"),
  "conformance|category_group_mapping"                = c("C.6", "Category-to-group mapping validation"),
  "conformance|lab_reference_units"                   = c("C.7", "Lab reference unit validation"),
  "completeness|missingness"                          = c("K.1", "Required column missingness"),
  "completeness|conditional_requirements"             = c("K.2", "Conditional field requirements"),
  "completeness|mcide_value_coverage"                 = c("K.3", "mCIDE value coverage"),
  "completeness|relational_integrity"                 = c("K.4", "Foreign key referential coverage"),
  "completeness|cross_table_conditional_completeness" = c("K.5", "Cross-table conditional completeness"),
  "plausibility|chronological_order"                  = c("P.1", "Chronological order constraints"),
  "plausibility|numeric_range_plausibility"           = c("P.2", "Numeric range plausibility"),
  "plausibility|field_plausibility"                   = c("P.3", "Field-level plausibility rules"),
  "plausibility|medication_dose_unit_consistency"     = c("P.4", "Medication dose-unit consistency"),
  "plausibility|overlapping_periods"                  = c("P.5", "Overlapping time period detection"),
  "plausibility|category_temporal_consistency"        = c("P.6", "Category temporal consistency"),
  "plausibility|duplicate_composite_keys"             = c("P.7", "Duplicate composite key detection"),
  "plausibility|cross_table_temporal"                 = c("P.8", "Cross-table temporal plausibility")
)

# Human-readable finding text for INFO-severity "pass" rows, keyed by rule code.
PASSING_FINDINGS <- list(
  C.1 = "Table present and has data",
  C.2 = "All required columns present",
  C.3 = "All dtypes match schema",
  C.4 = "All datetime columns timezone-aware",
  C.5 = "All categorical values conform to mCIDE",
  C.6 = "All category-group mappings consistent",
  C.7 = "All lab reference units valid",
  K.1 = "Required columns below null thresholds",
  K.2 = "All conditional requirements met",
  K.3 = "All mCIDE values represented",
  K.4 = "All foreign keys resolvable",
  K.5 = "All cross-table conditions met",
  P.1 = "Chronological order constraints met",
  P.2 = "All values within numeric range",
  P.3 = "All field plausibility rules met",
  P.4 = "All medication dose-unit pairs consistent",
  P.5 = "No overlapping time periods",
  P.6 = "Category distributions stable over time",
  P.7 = "No duplicate composite keys",
  P.8 = "All events within hospitalization bounds"
)

# Explicit partial-pass phrasing for rules whose PASSING_FINDINGS text does not
# start with "All " (so the generic "All X" -> "Remaining X" rewrite does not
# produce a sensible string).
PARTIAL_FINDINGS <- list(
  K.1 = "Remaining required columns below null thresholds",
  P.1 = "Remaining chronological order constraints met",
  P.6 = "Remaining category distributions stable over time"
)

# INFO messages that indicate a check was not applicable (never actually ran).
# These are filtered out of issue tables to reduce noise.
NOT_APPLICABLE_PREFIXES <- c(
  "No lab reference units defined in schema",
  "No category-to-group mappings defined in schema",
  "No conditional requirements defined for this table",
  "No chronological order rules defined for this table",
  "No field plausibility rules defined for this table",
  "No numeric range configuration for this table",
  "Medication dose unit check not applicable",
  "Missing hospitalization_id column; skipping",
  "No composite keys defined for this table",
  "No suitable datetime column found for temporal consistency check",
  "No category columns found for temporal consistency check",
  "No numeric columns with range configuration to check",
  "No cross-table conditional requirements applicable"
)

#' Human-readable "passed" finding for a DQA rule code
#'
#' @param rule_code The DQA rule code (e.g. `"K.3"`).
#' @param partial When `TRUE`, the check has also emitted error/warning rows,
#'   so the INFO row represents the remaining silent-pass atoms. Rewrites
#'   "All X" to "Remaining X" (or looks up the partial-findings table) so the
#'   row does not contradict the failing rows above it.
#' @return A finding string.
#' @export
#'
#' @examples
#' passing_finding("K.3")
#' passing_finding("K.3", partial = TRUE)
passing_finding <- function(rule_code, partial = FALSE) {
  if (isTRUE(partial) && !is.null(PARTIAL_FINDINGS[[rule_code]])) {
    return(PARTIAL_FINDINGS[[rule_code]])
  }
  finding_text <- PASSING_FINDINGS[[rule_code]] %||% "Checks passed"
  if (isTRUE(partial) && startsWith(finding_text, "All ")) {
    return(paste0("Remaining ", substring(finding_text, 5)))
  }
  finding_text
}

#' Extract the affected column name from an issue record
#'
#' Priority: `details$column` > `details$extra_columns` > parse from message >
#' `"NA"`. Mirrors `rule_codes.extract_column_field` in clifpy.
#'
#' @param issue A named list with (at least) `details` and `message` entries.
#' @return A single string naming the affected column(s), or `"NA"`.
#' @export
#'
#' @examples
#' extract_column_field(list(details = list(column = "vital_category")))
extract_column_field <- function(issue) {
  details <- issue$details
  if (!is.list(details)) {
    return("NA")
  }

  join_first <- function(values, limit) {
    paste(vapply(utils::head(values, limit), as.character, character(1)), collapse = ", ")
  }

  if (!is.null(details$column) && !identical(details$column, "")) {
    return(as.character(details$column))
  }
  if (is.vector(details$extra_columns) && length(details$extra_columns) > 0) {
    return(join_first(details$extra_columns, 3))
  }
  if (!is.null(details$required_column) && !identical(details$required_column, "")) {
    return(as.character(details$required_column))
  }
  if (is.vector(details$columns_checked) && length(details$columns_checked) > 0) {
    return(join_first(details$columns_checked, 3))
  }
  if (is.vector(details$missing_columns) && length(details$missing_columns) > 0) {
    return(join_first(details$missing_columns, 3))
  }
  if (is.vector(details$keys) && length(details$keys) > 0) {
    return(join_first(details$keys, length(details$keys)))
  }
  if (!is.null(details$category_column) && !is.null(details$group_column)) {
    return(paste0(details$category_column, ", ", details$group_column))
  }
  invalid_values <- details$invalid_values
  if (is.list(invalid_values) && length(invalid_values) > 0 && is.list(invalid_values[[1]])) {
    first_column_name <- invalid_values[[1]]$column
    if (!is.null(first_column_name)) {
      return(as.character(first_column_name))
    }
  }

  message_text <- issue$message %||% ""
  column_match <- regmatches(
    message_text,
    regexec("[Cc]olumn\\s+'([^']+)'", message_text)
  )[[1]]
  if (length(column_match) == 2) {
    return(column_match[2])
  }
  column_match <- regmatches(
    message_text,
    regexec("'([^']+)'\\s+column", message_text)
  )[[1]]
  if (length(column_match) == 2) {
    return(column_match[2])
  }

  "NA"
}

#' Build a rich finding string from a message and its details
#'
#' Inspects the details list for known structures (`top_invalid`,
#' `missing_columns`, orphan IDs, ...) and appends a concise summary to the
#' base message. Mirrors `rule_codes.build_finding` in clifpy.
#'
#' @param message The base issue message.
#' @param details Named list of issue details.
#' @return A finding string.
#' @export
#'
#' @examples
#' build_finding("2 invalid categorical values",
#'               list(top_invalid = list(list(value = "Foo", count = 3L))))
build_finding <- function(message, details) {
  if (!is.list(details) || length(details) == 0) {
    return(message)
  }

  finding_parts <- list(message)
  replaced_message <- FALSE

  # Categorical: top invalid values with counts (replaces generic message)
  top_invalid <- details$top_invalid
  if (is.list(top_invalid) && length(top_invalid) > 0) {
    items <- vapply(utils::head(top_invalid, 5), function(entry) {
      if (is.list(entry) && !is.null(entry$value)) {
        if (!is.null(entry$count)) {
          sprintf("'%s' (%s rows)", entry$value, py_int_comma(entry$count))
        } else {
          sprintf("'%s'", entry$value)
        }
      } else {
        as.character(entry)
      }
    }, character(1))
    suffix <- if (length(top_invalid) > 5) sprintf(" ... (%d total)", length(top_invalid)) else ""
    finding_parts <- list(sprintf("Invalid: %s%s", paste(items, collapse = ", "), suffix))
    replaced_message <- TRUE
  }

  # Missing columns — skip if the base message already lists them
  missing_columns <- details$missing_columns
  if (is.vector(missing_columns) && length(missing_columns) > 0 &&
      !grepl("required columns", message, fixed = TRUE)) {
    listed_columns <- paste(
      vapply(utils::head(missing_columns, 5), as.character, character(1)),
      collapse = ", "
    )
    suffix <- if (length(missing_columns) > 5) sprintf(" ... (%d total)", length(missing_columns)) else ""
    finding_parts <- c(finding_parts, sprintf("Missing: %s%s", listed_columns, suffix))
  }

  # Lab reference: top invalid units
  top_invalid_units <- details$top_invalid_units
  if (is.list(top_invalid_units) && length(top_invalid_units) > 0) {
    items <- vapply(utils::head(top_invalid_units, 5), function(entry) {
      if (is.list(entry)) {
        category_label <- entry$lab_category %||% entry$category %||% "?"
        unit_label <- entry$unit %||% entry$reference_unit %||% "?"
        sprintf("%s: '%s'", category_label, unit_label)
      } else {
        as.character(entry)
      }
    }, character(1))
    finding_parts <- c(finding_parts, sprintf("Units: %s", paste(items, collapse = ", ")))
  }

  # Category-group mapping: mismatched pairs (replaces generic message)
  mismatched_pairs <- details$mismatched_pairs
  if (is.list(mismatched_pairs) && length(mismatched_pairs) > 0) {
    items <- vapply(utils::head(mismatched_pairs, 3), function(entry) {
      if (is.list(entry)) {
        category_label <- entry$category %||% "?"
        actual_group <- entry$actual_group %||% "?"
        expected_group <- entry$expected_group %||% "?"
        expected_display <- if (length(expected_group) > 1) {
          paste(sprintf("'%s'", expected_group), collapse = " or ")
        } else {
          sprintf("'%s'", expected_group)
        }
        sprintf("%s: found '%s', expected %s", category_label, actual_group, expected_display)
      } else {
        as.character(entry)
      }
    }, character(1))
    suffix <- if (length(mismatched_pairs) > 3) sprintf(" ... (%d total)", length(mismatched_pairs)) else ""
    finding_parts <- list(sprintf("Mismatched: %s%s", paste(items, collapse = ", "), suffix))
    replaced_message <- TRUE
  }

  # Conditional requirements: missing counts
  rows_with_missing <- details$rows_with_missing
  if (!is.null(rows_with_missing)) {
    rows_meeting_condition <- details$rows_meeting_condition %||% 0L
    percent_missing <- details$percent_missing %||% 0
    required_column <- details$required_column %||% ""
    finding_parts <- c(finding_parts, sprintf(
      "%s: %s/%s rows missing (%s%%)",
      required_column,
      py_int_comma(rows_with_missing),
      py_int_comma(rows_meeting_condition),
      py_num_str(percent_missing)
    ))
  }

  if (length(finding_parts) == 1 && !replaced_message) {
    return(message)
  }
  paste(unlist(finding_parts), collapse = " | ")
}

#' Truncate a message for PDF display
#'
#' @param message The message to truncate.
#' @param max_len Maximum length (default 400).
#' @return The message, truncated with `...` if longer than `max_len`.
#' @export
#'
#' @examples
#' truncate_comment(strrep("x", 500))
truncate_comment <- function(message, max_len = 400) {
  if (nchar(message) <= max_len) {
    return(message)
  }
  paste0(substring(message, 1, max_len - 3), "...")
}

#' Enrich a DQA issue record with rule metadata
#'
#' Adds `rule_code`, `rule_description`, `column_field`, `finding` and
#' `atomic_count` fields to an issue record. Mirrors
#' `rule_codes.enrich_issue` in clifpy.
#'
#' @param issue A named list with `category` and `check_type` entries (plus
#'   `message`, `details`, `severity` as available).
#' @param check_key Optional dict key from the results (e.g. the FK column
#'   name for relational checks).
#' @return The enriched issue record, or `NULL` for INFO-level messages that
#'   indicate a check was not applicable.
#' @export
#'
#' @examples
#' enrich_issue(list(category = "completeness", check_type = "missingness",
#'                   message = "Column 'x' has 12% missing values",
#'                   details = list(column = "x")))
enrich_issue <- function(issue, check_key = NULL) {
  if (identical(issue$severity, "info")) {
    message_text <- issue$message %||% ""
    if (any(startsWith(message_text, NOT_APPLICABLE_PREFIXES))) {
      return(NULL)
    }
  }

  rule_key <- paste0(issue$category %||% "", "|", issue$check_type %||% "")
  rule_entry <- RULE_CODES[[rule_key]] %||% c("", "")

  issue$rule_code <- rule_entry[1]
  issue$rule_description <- rule_entry[2]
  issue$column_field <- extract_column_field(issue)
  issue$finding <- build_finding(issue$message %||% "", issue$details %||% list())
  issue$atomic_count <- extract_atomic_count(issue)

  # For relational checks, the check_key IS the FK column
  if (identical(issue$check_type, "relational_integrity") &&
      !is.null(check_key) && identical(issue$column_field, "NA")) {
    issue$column_field <- check_key
  }

  issue
}

# Infer how many atomic checks a single enriched issue row represents.
# Priority: explicit details$atomic_count, then the length of a known list
# field whose items correspond one-to-one with atoms, then 1.
extract_atomic_count <- function(issue) {
  details <- issue$details
  if (!is.list(details)) {
    return(1L)
  }

  explicit_count <- details$atomic_count
  # Accept explicit 0 as well — an informational row (e.g. the reverse
  # direction of a K.4 relational check) wants to display but not contribute
  # to the atomic sum.
  if (is.numeric(explicit_count) && length(explicit_count) == 1 && explicit_count >= 0) {
    return(as.integer(explicit_count))
  }

  for (list_field in c("missing_columns", "mismatched_pairs", "missing_categories", "missing_values")) {
    field_value <- details[[list_field]]
    if ((is.list(field_value) || is.vector(field_value)) && length(field_value) > 0) {
      return(length(field_value))
    }
  }

  1L
}
