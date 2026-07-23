#' DQA report generation
#'
#' Port of `clifpy/utils/report_generator.py` (clifpy 0.5.0). Turns the
#' `validation_data` a [run_full_dqa()] call produces into human-readable DQA
#' reports.
#'
#' clifpy renders a PDF (via reportlab), a plain-text report, and a consolidated
#' CSV. reportlab has no R equivalent and PDF byte-parity is neither achievable
#' nor useful, so the two PDF entry points here emit a **self-contained HTML**
#' report with the same content and section structure instead
#' ([generate_validation_html()], [generate_combined_validation_html()]); thin
#' `*_pdf()` aliases forward to them and announce the substitution. Every other
#' output — the text report, the consolidated CSV, and the
#' [collect_dqa_issues()] / [compute_table_stats()] structures — is ported
#' faithfully and matches clifpy byte-for-byte (modulo the run timestamp).
#'
#' All report inputs are the `validation_data` list returned by [run_full_dqa()]
#' (optionally augmented by the caller with `table_stats` and `total_rows`).
#' Validation itself is never re-implemented here.
#'
#' @name clif-report-generator
NULL

# The three DQA categories the report iterates, in display order. Relational
# integrity lives under its own key in run_full_dqa output and is intentionally
# not surfaced by the report, matching clifpy.
DQA_CATEGORIES <- c("conformance", "completeness", "plausibility")

# snake_case table name -> display label used in combined reports and the CSV.
TABLE_DISPLAY_NAMES <- list(
  adt = "ADT",
  code_status = "Code Status",
  crrt_therapy = "CRRT Therapy",
  ecmo_mcs = "ECMO/MCS",
  hospital_diagnosis = "Hospital Diagnosis",
  hospitalization = "Hospitalization",
  labs = "Labs",
  medication_admin_continuous = "Medication Admin Continuous",
  medication_admin_intermittent = "Medication Admin Intermittent",
  microbiology_culture = "Microbiology Culture",
  microbiology_nonculture = "Microbiology Nonculture",
  microbiology_susceptibility = "Microbiology Susceptibility",
  patient = "Patient",
  patient_assessments = "Patient Assessments",
  patient_procedures = "Patient Procedures",
  position = "Position",
  respiratory_support = "Respiratory Support",
  vitals = "Vitals"
)


# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------

# Left-justify a string to `width` characters (Python f-string `{x:<width}` /
# default string alignment). Never truncates on width alone. Counts characters,
# not bytes, so multibyte glyphs (the em-dash used for a zero check count) pad
# the way Python counts them.
pad_field_right <- function(value, width) {
  value <- as.character(value)
  deficit <- width - nchar(value, type = "chars")
  if (deficit <= 0) return(value)
  paste0(value, strrep(" ", deficit))
}

# Right-justify a string to `width` characters (Python `{x:>width}`).
pad_field_left <- function(value, width) {
  value <- as.character(value)
  deficit <- width - nchar(value, type = "chars")
  if (deficit <= 0) return(value)
  paste0(strrep(" ", deficit), value)
}

# Python str.title(): uppercase the first letter of each alphabetic run,
# lowercase the rest. Non-letters are word boundaries.
py_title <- function(text) {
  characters <- strsplit(text, "", fixed = TRUE)[[1]]
  previous_is_alpha <- FALSE
  for (index in seq_along(characters)) {
    character <- characters[index]
    is_alpha <- grepl("[A-Za-z]", character)
    if (is_alpha) {
      characters[index] <- if (previous_is_alpha) tolower(character) else toupper(character)
    }
    previous_is_alpha <- is_alpha
  }
  paste(characters, collapse = "")
}

# Reproduce Python's repr()/str() of a finite float (numpy's shortest
# round-tripping representation), so DOUBLE/FLOAT min/max strings in the data
# profile match clifpy byte-for-byte.
py_repr_float <- function(value) {
  if (is.na(value)) return("nan")
  if (is.infinite(value)) return(if (value > 0) "inf" else "-inf")
  if (value == 0) return("0.0")

  is_negative <- value < 0
  magnitude <- abs(value)

  digit_string <- NULL
  exponent <- NULL
  for (precision in 1:17) {
    candidate <- sprintf("%.*e", precision - 1L, magnitude)
    if (as.numeric(candidate) == magnitude) {
      matched <- regmatches(
        candidate,
        regexec("^([0-9])(?:\\.([0-9]+))?e([+-][0-9]+)$", candidate)
      )[[1]]
      digit_string <- paste0(matched[2], matched[3])
      exponent <- as.integer(matched[4])
      break
    }
  }
  if (is.null(digit_string)) {
    candidate <- sprintf("%.16e", magnitude)
    matched <- regmatches(
      candidate,
      regexec("^([0-9])(?:\\.([0-9]+))?e([+-][0-9]+)$", candidate)
    )[[1]]
    digit_string <- paste0(matched[2], matched[3])
    exponent <- as.integer(matched[4])
  }

  digit_string <- sub("0+$", "", digit_string)
  if (!nzchar(digit_string)) digit_string <- "0"
  decimal_point <- exponent + 1L
  digit_count <- nchar(digit_string)

  if (decimal_point <= -4 || decimal_point > 16) {
    mantissa <- if (digit_count > 1) {
      paste0(substr(digit_string, 1, 1), ".", substr(digit_string, 2, digit_count))
    } else {
      substr(digit_string, 1, 1)
    }
    formatted <- paste0(mantissa, "e", sprintf("%+03d", decimal_point - 1L))
  } else if (decimal_point <= 0) {
    formatted <- paste0("0.", strrep("0", -decimal_point), digit_string)
  } else if (decimal_point >= digit_count) {
    formatted <- paste0(digit_string, strrep("0", decimal_point - digit_count), ".0")
  } else {
    formatted <- paste0(
      substr(digit_string, 1, decimal_point), ".",
      substr(digit_string, decimal_point + 1, digit_count)
    )
  }
  if (is_negative) paste0("-", formatted) else formatted
}


# ---------------------------------------------------------------------------
# Issue collection and scoring
# ---------------------------------------------------------------------------

# Exact-key reimplementation of rule_codes.extract_column_field.
#
# The shared enrich_issue() in R/util_rule_codes.R extracts column_field with
# `details$column`, and R's `$` does PARTIAL matching on lists: for a details
# list carrying `columns_checked` (P.2/P.6 summary rows) but no exact `column`
# key, `details$column` silently resolves to `columns_checked` — a multi-element
# vector — where clifpy's `details.get('column')` returns None and falls through
# to the `columns_checked` (first-3) branch. That divergence both breaks the
# collapse logic here and yields a non-matching column_field. Until the shared
# helper switches to exact-key access, this module re-derives column_field with
# `[[ ]]` (exact) so report output matches clifpy.
report_column_field <- function(details, message = "") {
  if (!is.list(details)) {
    return("NA")
  }
  join_first_n <- function(values, limit) {
    paste(vapply(utils::head(values, limit), as.character, character(1)), collapse = ", ")
  }
  truthy_scalar <- function(value) {
    !is.null(value) && length(value) >= 1 && any(nzchar(as.character(value)))
  }

  column <- details[["column"]]
  if (truthy_scalar(column)) {
    return(as.character(column)[1])
  }
  extra_columns <- details[["extra_columns"]]
  if (!is.null(extra_columns) && length(extra_columns) > 0) {
    return(join_first_n(extra_columns, 3))
  }
  required_column <- details[["required_column"]]
  if (truthy_scalar(required_column)) {
    return(as.character(required_column)[1])
  }
  columns_checked <- details[["columns_checked"]]
  if (!is.null(columns_checked) && length(columns_checked) > 0) {
    return(join_first_n(columns_checked, 3))
  }
  missing_columns <- details[["missing_columns"]]
  if (!is.null(missing_columns) && length(missing_columns) > 0) {
    return(join_first_n(missing_columns, 3))
  }
  keys <- details[["keys"]]
  if (!is.null(keys) && length(keys) > 0) {
    return(join_first_n(keys, length(keys)))
  }
  category_column <- details[["category_column"]]
  group_column <- details[["group_column"]]
  if (truthy_scalar(category_column) && truthy_scalar(group_column)) {
    return(paste0(as.character(category_column)[1], ", ", as.character(group_column)[1]))
  }
  invalid_values <- details[["invalid_values"]]
  if (!is.null(invalid_values) && length(invalid_values) > 0 && is.list(invalid_values[[1]])) {
    first_column <- invalid_values[[1]][["column"]]
    if (truthy_scalar(first_column)) {
      return(as.character(first_column)[1])
    }
  }

  message_text <- message %||% ""
  column_match <- regmatches(message_text, regexec("[Cc]olumn\\s+'([^']+)'", message_text))[[1]]
  if (length(column_match) == 2) {
    return(column_match[2])
  }
  column_match <- regmatches(message_text, regexec("'([^']+)'\\s+column", message_text))[[1]]
  if (length(column_match) == 2) {
    return(column_match[2])
  }
  "NA"
}

# Exact-key reimplementation of rule_codes.build_finding.
#
# The shared build_finding() in R/util_rule_codes.R reads detail fields with
# `details$top_invalid`, which R partial-matches to `top_invalid_units` on a
# labs C.7 warning (which carries top_invalid_units but no top_invalid). clifpy's
# `details.get('top_invalid')` returns None there. That partial match sends the
# unit records down the categorical-values branch and both crashes and diverges.
# This exact-key (`[[ ]]`) port avoids it.
report_build_finding <- function(message, details) {
  if (!is.list(details) || length(details) == 0) {
    return(message)
  }
  finding_parts <- list(message)
  replaced_message <- FALSE

  top_invalid <- details[["top_invalid"]]
  if (is.list(top_invalid) && length(top_invalid) > 0) {
    items <- vapply(utils::head(top_invalid, 5), function(entry) {
      value <- if (is.list(entry)) entry[["value"]] else NULL
      if (is.list(entry) && !is.null(value)) {
        count <- entry[["count"]]
        if (!is.null(count)) {
          sprintf("'%s' (%s rows)", value, py_int_comma(count))
        } else {
          sprintf("'%s'", value)
        }
      } else {
        paste(as.character(entry), collapse = ", ")
      }
    }, character(1))
    suffix <- if (length(top_invalid) > 5) sprintf(" ... (%d total)", length(top_invalid)) else ""
    finding_parts <- list(sprintf("Invalid: %s%s", paste(items, collapse = ", "), suffix))
    replaced_message <- TRUE
  }

  missing_columns <- details[["missing_columns"]]
  if (is.vector(missing_columns) && length(missing_columns) > 0 &&
      !grepl("required columns", message, fixed = TRUE)) {
    listed_columns <- paste(
      vapply(utils::head(missing_columns, 5), as.character, character(1)), collapse = ", "
    )
    suffix <- if (length(missing_columns) > 5) sprintf(" ... (%d total)", length(missing_columns)) else ""
    finding_parts <- c(finding_parts, sprintf("Missing: %s%s", listed_columns, suffix))
  }

  top_invalid_units <- details[["top_invalid_units"]]
  if (is.list(top_invalid_units) && length(top_invalid_units) > 0) {
    items <- vapply(utils::head(top_invalid_units, 5), function(entry) {
      if (is.list(entry)) {
        category_label <- entry[["lab_category"]] %||% entry[["category"]] %||% "?"
        unit_label <- entry[["unit"]] %||% entry[["reference_unit"]] %||% "?"
        sprintf("%s: '%s'", category_label, unit_label)
      } else {
        paste(as.character(entry), collapse = ", ")
      }
    }, character(1))
    finding_parts <- c(finding_parts, sprintf("Units: %s", paste(items, collapse = ", ")))
  }

  mismatched_pairs <- details[["mismatched_pairs"]]
  if (is.list(mismatched_pairs) && length(mismatched_pairs) > 0) {
    items <- vapply(utils::head(mismatched_pairs, 3), function(entry) {
      if (is.list(entry)) {
        category_label <- entry[["category"]] %||% "?"
        actual_group <- entry[["actual_group"]] %||% "?"
        expected_group <- unlist(entry[["expected_group"]] %||% "?", use.names = FALSE)
        expected_display <- if (length(expected_group) > 1) {
          paste(sprintf("'%s'", expected_group), collapse = " or ")
        } else {
          sprintf("'%s'", expected_group)
        }
        sprintf("%s: found '%s', expected %s", category_label, actual_group, expected_display)
      } else {
        paste(as.character(entry), collapse = ", ")
      }
    }, character(1))
    suffix <- if (length(mismatched_pairs) > 3) sprintf(" ... (%d total)", length(mismatched_pairs)) else ""
    finding_parts <- list(sprintf("Mismatched: %s%s", paste(items, collapse = ", "), suffix))
    replaced_message <- TRUE
  }

  rows_with_missing <- details[["rows_with_missing"]]
  if (!is.null(rows_with_missing)) {
    rows_meeting_condition <- details[["rows_meeting_condition"]] %||% 0L
    percent_missing <- details[["percent_missing"]] %||% 0
    required_column <- details[["required_column"]] %||% ""
    finding_parts <- c(finding_parts, sprintf(
      "%s: %s/%s rows missing (%s%%)",
      required_column, py_int_comma(rows_with_missing),
      py_int_comma(rows_meeting_condition), py_num_str(percent_missing)
    ))
  }

  if (length(finding_parts) == 1 && !replaced_message) {
    return(message)
  }
  paste(unlist(finding_parts), collapse = " | ")
}

# Exact-key reimplementation of rule_codes.extract_atomic_count (its `$`-based
# atomic_count read is not currently ambiguous, but keeping the full enrichment
# local guarantees the report never routes through the partial-matching helpers).
report_extract_atomic_count <- function(issue) {
  details <- issue$details
  if (!is.list(details)) {
    return(1L)
  }
  explicit_count <- details[["atomic_count"]]
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

# Enrich an issue for the report using exact-key detail access throughout,
# reusing only the partial-match-safe shared pieces (RULE_CODES, passing_finding,
# NOT_APPLICABLE_PREFIXES). Mirrors rule_codes.enrich_issue.
enrich_issue_for_report <- function(issue, check_key = NULL) {
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
  issue$column_field <- report_column_field(issue$details %||% list(), issue$message %||% "")
  issue$finding <- report_build_finding(issue$message %||% "", issue$details %||% list())
  issue$atomic_count <- report_extract_atomic_count(issue)
  if (identical(issue$check_type, "relational_integrity") &&
      !is.null(check_key) && identical(issue$column_field, "NA")) {
    issue$column_field <- check_key
  }
  issue
}

# Unique identifier for a DQA issue, matching clifpy's feedback.create_error_id
# and report_generator._make_error_id.
make_error_id <- function(issue) {
  message_text <- issue$message %||% ""
  description_hash <- substr(
    digest::digest(message_text, algo = "md5", serialize = FALSE), 1, 8
  )
  category <- issue$category %||% ""
  check_type <- issue$check_type %||% "unknown"
  prefix <- if (nzchar(category)) paste0(category, "_", check_type) else check_type
  prefix <- tolower(gsub(" ", "_", prefix, fixed = TRUE))
  paste0(prefix, "_", description_hash)
}

# Return a "(err >X%, warn >Y%)" suffix when a group's rows carry
# error_threshold/warning_threshold in their details, else "".
threshold_suffix <- function(rows) {
  for (row in rows) {
    details <- row$details %||% list()
    error_threshold <- details$error_threshold
    warning_threshold <- details$warning_threshold
    if (!is.null(error_threshold) && !is.null(warning_threshold)) {
      return(sprintf("(err >%s%%, warn >%s%%)",
                     py_num_str(error_threshold), py_num_str(warning_threshold)))
    }
    if (!is.null(error_threshold)) {
      return(sprintf("(err >%s%%)", py_num_str(error_threshold)))
    }
    if (!is.null(warning_threshold)) {
      return(sprintf("(warn >%s%%)", py_num_str(warning_threshold)))
    }
  }
  ""
}

# Collapse INFO-severity rows sharing a rule_code into one row per group.
# Non-INFO rows pass through unchanged, preserving order; INFO groups are
# appended afterwards in first-appearance order. Warnings are never collapsed.
collapse_info_rows <- function(rows) {
  output <- list()
  info_group_keys <- character(0)
  info_groups <- list()

  for (row in rows) {
    if (!identical(row$severity, "info")) {
      output <- c(output, list(row))
      next
    }
    key <- row$rule_code %||% row$check_type %||% ""
    if (!key %in% info_group_keys) {
      info_group_keys <- c(info_group_keys, key)
      info_groups[[key]] <- list()
    }
    info_groups[[key]] <- c(info_groups[[key]], list(row))
  }

  for (key in info_group_keys) {
    group <- info_groups[[key]]
    if (length(group) == 1) {
      output <- c(output, list(group[[1]]))
      next
    }

    seen_columns <- character(0)
    for (row in group) {
      column_field <- row$column_field %||% ""
      if (nzchar(column_field) && !identical(column_field, "NA") &&
          !column_field %in% seen_columns) {
        seen_columns <- c(seen_columns, column_field)
      }
    }

    merged <- group[[1]]
    joined <- paste(seen_columns, collapse = ", ")
    if (nchar(joined) > 200) {
      joined <- paste0(substr(joined, 1, 197), "...")
    }
    merged$column_field <- if (nzchar(joined)) joined else (merged$column_field %||% "")

    finding <- passing_finding(merged$rule_code %||% "")
    suffix <- threshold_suffix(group)
    if (nzchar(suffix)) {
      finding <- paste0(finding, " ", suffix)
    }
    merged$finding <- finding
    merged$message <- finding
    merged$details <- list(count = length(group), columns = as.list(seen_columns))
    merged$atomic_count <- sum(vapply(
      group, function(row) as.numeric(row$atomic_count %||% 1), numeric(1)
    ))
    output <- c(output, list(merged))
  }
  output
}

# Align per-row atomic_count with the check's atomic_total. Mutates and returns
# the row list. Mirrors report_generator._reconcile_atomic_counts.
reconcile_atomic_counts <- function(rows, atomic_total, atomic_passed,
                                    category, check_type, check_key = NULL) {
  if (atomic_total == 0) {
    return(rows)
  }

  err_warn_sum <- sum(vapply(rows, function(row) {
    if (row$severity %in% c("error", "warning")) as.numeric(row$atomic_count %||% 1) else 0
  }, numeric(1)))
  remaining <- max(0, atomic_total - err_warn_sum)
  has_error_or_warning <- any(vapply(
    rows, function(row) row$severity %in% c("error", "warning"), logical(1)
  ))

  info_indices <- which(vapply(rows, function(row) identical(row$severity, "info"), logical(1)))
  if (length(info_indices) > 0) {
    first_info <- info_indices[1]
    rows[[first_info]]$atomic_count <- remaining
    if (has_error_or_warning) {
      rule_code <- rows[[first_info]]$rule_code %||% ""
      if (nzchar(rule_code)) {
        generic <- passing_finding(rule_code, partial = FALSE)
        if (identical(rows[[first_info]]$finding, generic)) {
          rows[[first_info]]$finding <- passing_finding(rule_code, partial = TRUE)
          rows[[first_info]]$message <- rows[[first_info]]$finding
        }
      }
    }
    return(rows)
  }

  if (remaining > 0) {
    columns_seen <- character(0)
    for (row in rows) {
      column_field <- row$column_field %||% ""
      if (!nzchar(column_field) || identical(column_field, "NA")) next
      for (piece in strsplit(column_field, ", ", fixed = TRUE)[[1]]) {
        trimmed <- trimws(piece)
        if (nzchar(trimmed) && !trimmed %in% columns_seen) {
          columns_seen <- c(columns_seen, trimmed)
        }
      }
    }
    partial <- has_error_or_warning
    synthetic <- list(
      category = category,
      check_type = check_type,
      severity = "info",
      message = "",
      details = list(count = remaining, columns_checked = as.list(columns_seen))
    )
    enriched <- enrich_issue_for_report(synthetic, check_key = check_key)
    if (!is.null(enriched)) {
      enriched$atomic_count <- remaining
      enriched$finding <- passing_finding(enriched$rule_code %||% "", partial = partial)
      enriched$message <- enriched$finding
      if (length(columns_seen) > 0) {
        joined <- paste(columns_seen, collapse = ", ")
        if (nchar(joined) > 200) {
          joined <- paste0(substr(joined, 1, 197), "...")
        }
        enriched$column_field <- joined
      } else {
        enriched$column_field <- enriched$column_field %||% "NA"
      }
      rows <- c(rows, list(enriched))
    }
  }
  rows
}

#' Collect enriched DQA issues and per-category scores
#'
#' Port of `report_generator.collect_dqa_issues`. Walks the conformance,
#' completeness and plausibility checks in `validation_data`, enriching each
#' error/warning/info message with rule metadata, collapsing silent-pass INFO
#' rows and reconciling per-row atomic counts against each check's
#' `atomic_total`.
#'
#' @param validation_data The list [run_full_dqa()] returns.
#' @return A named list with `category_scores` (a named list mapping each scored
#'   category to a length-2 `c(passed, total)` vector) and `all_issues` (a list
#'   of enriched issue records).
#' @export
#'
#' @examples
#' \dontrun{
#' validation_data <- run_full_dqa(patient_df, table_name = "patient", clif_version = "3.0")
#' collect_dqa_issues(validation_data)
#' }
collect_dqa_issues <- function(validation_data) {
  category_scores <- list()
  all_issues <- list()

  for (category in DQA_CATEGORIES) {
    checks <- validation_data[[category]]
    if (is.null(checks) || length(checks) == 0) {
      next
    }

    category_passed <- 0
    category_total <- 0

    for (check_name in names(checks)) {
      check <- checks[[check_name]]
      check_enriched <- list()

      enrich_records <- function(records, severity) {
        for (record in records) {
          issue <- list(
            category = category,
            check_type = check$check_type,
            severity = severity,
            message = record$message %||% "",
            details = record$details %||% list()
          )
          enriched <- enrich_issue_for_report(issue, check_key = check_name)
          if (!is.null(enriched)) {
            check_enriched[[length(check_enriched) + 1]] <<- enriched
          }
        }
      }
      enrich_records(check$errors %||% list(), "error")
      enrich_records(check$warnings %||% list(), "warning")
      enrich_records(check$info %||% list(), "info")

      atomic_total <- check$atomic_total
      atomic_passed <- check$atomic_passed
      if (is.null(atomic_total) || is.null(atomic_passed)) {
        cli::cli_abort(c(
          "Check {.val {check_name}} in category {.val {category}} is missing atomic counts.",
          "i" = "Every DQA check must populate {.field atomic_total}/{.field atomic_passed}."
        ))
      }
      atomic_total <- as.numeric(atomic_total)
      atomic_passed <- as.numeric(atomic_passed)
      category_total <- category_total + atomic_total
      category_passed <- category_passed + atomic_passed

      collapsed <- collapse_info_rows(check_enriched)
      collapsed <- reconcile_atomic_counts(
        collapsed, atomic_total, atomic_passed, category, check$check_type,
        check_key = check_name
      )
      all_issues <- c(all_issues, collapsed)
    }

    if (category_total > 0) {
      category_scores[[category]] <- c(category_passed, category_total)
    }
  }

  list(category_scores = category_scores, all_issues = all_issues)
}

# Sum atomic_count over issues of a given severity (default 1 per issue).
sum_atomic_count <- function(issues, severity) {
  total <- 0
  for (issue in issues) {
    if (identical(issue$severity, severity)) {
      total <- total + as.numeric(issue$atomic_count %||% 1)
    }
  }
  total
}


# ---------------------------------------------------------------------------
# Per-column data profile
# ---------------------------------------------------------------------------

RANGE_DTYPES <- c("DATETIME", "DATE", "INT", "INTEGER", "DOUBLE", "FLOAT", "NUMERIC")

#' Per-column descriptive statistics for a table
#'
#' Port of `report_generator.compute_table_stats`. Produces one record per
#' schema-defined column present in `df`, with null counts, null percentage,
#' distinct-value count and (for numeric/datetime columns) formatted min/max.
#'
#' @param df The table's data frame, or `NULL`.
#' @param schema The parsed table schema (with a `columns` list), or `NULL`.
#' @return A list of named lists, each with `column`, `dtype`, `null_count`,
#'   `null_pct`, `unique`, `min` and `max`. Empty when inputs are missing/empty.
#' @export
#'
#' @examples
#' \dontrun{
#' compute_table_stats(patient_df, load_schema("patient", "3.0"))
#' }
compute_table_stats <- function(df, schema) {
  if (is.null(df) || is.null(schema)) {
    return(list())
  }
  n_rows <- tryCatch(nrow(df), error = function(condition) NULL)
  if (is.null(n_rows) || n_rows == 0) {
    return(list())
  }

  column_specs <- schema$columns %||% list()
  stats <- list()
  for (column_spec in column_specs) {
    column_name <- column_spec$name
    if (!column_name %in% names(df)) {
      next
    }
    series <- df[[column_name]]
    null_count <- as.integer(sum(is.na(series)))
    null_pct <- if (n_rows) py_round(null_count / n_rows * 100, 1) else 0.0
    unique_count <- as.integer(length(unique(series[!is.na(series)])))
    declared_dtype <- column_spec$data_type %||% class(series)[1]
    column_dtype <- toupper(declared_dtype)

    column_min <- NULL
    column_max <- NULL
    if (column_dtype %in% RANGE_DTYPES) {
      non_null <- series[!is.na(series)]
      if (length(non_null) > 0) {
        raw_min <- min(non_null)
        raw_max <- max(non_null)
        if (column_dtype %in% c("DATETIME", "DATE")) {
          date_format <- if (column_dtype == "DATETIME") "%Y-%m-%d %H:%M" else "%Y-%m-%d"
          column_min <- format(raw_min, date_format)
          column_max <- format(raw_max, date_format)
        } else if (column_dtype %in% c("INT", "INTEGER")) {
          column_min <- sprintf("%.0f", as.numeric(raw_min))
          column_max <- sprintf("%.0f", as.numeric(raw_max))
        } else {
          column_min <- py_repr_float(as.numeric(raw_min))
          column_max <- py_repr_float(as.numeric(raw_max))
        }
      }
    }

    stats[[length(stats) + 1]] <- list(
      column = column_name,
      dtype = declared_dtype,
      null_count = null_count,
      null_pct = null_pct,
      unique = unique_count,
      min = column_min,
      max = column_max
    )
  }
  stats
}


# ---------------------------------------------------------------------------
# Loading persisted per-table results
# ---------------------------------------------------------------------------

#' Load per-table DQA JSON results and optional feedback files
#'
#' Port of `report_generator.collect_table_results`.
#'
#' @param json_dir Directory containing `{table_name}_dqa.json` files.
#' @param table_names Ordered vector of table names to include.
#' @param feedback_dir Optional directory of
#'   `{table_name}_validation_response.json` feedback files.
#' @return A named list with `results` and `feedback_map`, each mapping
#'   `table_name -> parsed list | NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' collect_table_results("output/validation", c("patient", "adt"))
#' }
collect_table_results <- function(json_dir, table_names, feedback_dir = NULL) {
  results <- list()
  feedback_map <- list()

  read_json_or_null <- function(path) {
    if (!file.exists(path)) return(NULL)
    tryCatch(
      jsonlite::fromJSON(path, simplifyVector = FALSE),
      error = function(condition) NULL
    )
  }

  for (table_name in table_names) {
    json_path <- file.path(json_dir, paste0(table_name, "_dqa.json"))
    # Assign via single-bracket so a NULL value keeps the name in the list.
    results[table_name] <- list(read_json_or_null(json_path))

    if (!is.null(feedback_dir)) {
      feedback_path <- file.path(
        feedback_dir, paste0(table_name, "_validation_response.json")
      )
      feedback_map[table_name] <- list(read_json_or_null(feedback_path))
    }
  }

  list(results = results, feedback_map = feedback_map)
}


# ---------------------------------------------------------------------------
# Plain-text report
# ---------------------------------------------------------------------------

#' Generate a plain-text DQA report
#'
#' Port of `report_generator.generate_text_report`. Byte-for-byte compatible
#' with clifpy apart from the `Generated:` timestamp line.
#'
#' @param validation_data The list [run_full_dqa()] returns, optionally carrying
#'   `table_stats` and `total_rows` for the data-profile section.
#' @param table_name Name of the table.
#' @param output_path Path where the text file is written.
#' @param site_name Optional site/hospital label.
#' @return `output_path`, invisibly matching clifpy's returned path.
#' @export
#'
#' @examples
#' \dontrun{
#' generate_text_report(validation_data, "patient", tempfile(fileext = ".txt"))
#' }
generate_text_report <- function(validation_data, table_name, output_path,
                                 site_name = NULL) {
  collected <- collect_dqa_issues(validation_data)
  category_scores <- collected$category_scores
  all_issues <- collected$all_issues

  total_passed <- sum(vapply(category_scores, function(score) score[1], numeric(1)), 0)
  total_checks <- sum(vapply(category_scores, function(score) score[2], numeric(1)), 0)
  error_count <- sum_atomic_count(all_issues, "error")
  warning_count <- sum_atomic_count(all_issues, "warning")

  int_str <- function(value) sprintf("%d", as.integer(round(value)))

  lines <- character(0)
  add <- function(...) lines[[length(lines) + 1]] <<- paste0(...)

  add(strrep("=", 120))
  add("CLIF 2.1 DQA VALIDATION REPORT")
  add(paste0(toupper(table_name), " TABLE"))
  add(strrep("=", 120))
  add("")
  if (!is.null(site_name)) {
    add("Site: ", site_name)
  }
  add("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  add("")

  # --- DQA Summary ---
  add(strrep("-", 120))
  add("DQA SUMMARY")
  add(strrep("-", 120))
  add("  ", pad_field_right("Category", 20), "  ", pad_field_left("Passed", 6),
      "  ", pad_field_left("Total", 5), "  ", pad_field_left("Errors", 6),
      "  ", pad_field_left("Warnings", 8))
  add("  ", strrep("-", 20), "  ", strrep("-", 6), "  ", strrep("-", 5),
      "  ", strrep("-", 6), "  ", strrep("-", 8))
  for (category in DQA_CATEGORIES) {
    if (is.null(category_scores[[category]])) next
    score <- category_scores[[category]]
    category_issues <- Filter(function(issue) identical(issue$category, category), all_issues)
    category_errors <- sum_atomic_count(category_issues, "error")
    category_warnings <- sum_atomic_count(category_issues, "warning")
    add("  ", pad_field_right(py_title(category), 20),
        "  ", pad_field_left(int_str(score[1]), 6),
        "  ", pad_field_left(int_str(score[2]), 5),
        "  ", pad_field_left(int_str(category_errors), 6),
        "  ", pad_field_left(int_str(category_warnings), 8))
  }
  add("  ", pad_field_right("Overall", 20),
      "  ", pad_field_left(int_str(total_passed), 6),
      "  ", pad_field_left(int_str(total_checks), 5),
      "  ", pad_field_left(int_str(error_count), 6),
      "  ", pad_field_left(int_str(warning_count), 8))
  add("")

  # --- Data Profile ---
  table_stats <- validation_data$table_stats %||% list()
  if (length(table_stats) > 0) {
    add(strrep("-", 120))
    add("DATA PROFILE")
    add(strrep("-", 120))
    total_rows <- validation_data$total_rows %||% 0
    add("  Total Rows: ", py_int_comma(total_rows))
    add("")
    add("  ", pad_field_right("Column", 25), pad_field_right("Dtype", 12),
        pad_field_left("Null", 8), pad_field_left("Null%", 8),
        pad_field_left("Unique", 10), "  ",
        pad_field_right("Min", 20), pad_field_right("Max", 20))
    add("  ", strrep("-", 25 + 12 + 8 + 8 + 10 + 2 + 20 + 20))
    for (stat in table_stats) {
      column_name <- stat$column
      if (nchar(column_name) > 23) {
        column_name <- paste0(substr(column_name, 1, 21), "..")
      }
      column_min <- stat$min %||% ""
      column_max <- stat$max %||% ""
      add("  ", pad_field_right(column_name, 25),
          pad_field_right(stat$dtype, 12),
          pad_field_left(py_int_comma(stat$null_count), 8),
          pad_field_left(sprintf("%.1f", as.numeric(stat$null_pct)), 8), "%",
          pad_field_left(py_int_comma(stat$unique), 10), "  ",
          pad_field_right(column_min, 20), pad_field_right(column_max, 20))
    }
    add("")
  }

  # --- Details ---
  if (length(all_issues) > 0) {
    add(strrep("=", 120))
    add("DETAILS")
    add(strrep("=", 120))

    for (category in DQA_CATEGORIES) {
      category_issues <- Filter(function(issue) identical(issue$category, category), all_issues)
      if (length(category_issues) == 0) next

      add("")
      score <- category_scores[[category]]
      header_tail <- if (!is.null(score)) {
        sprintf("(%s/%s)", int_str(score[1]), int_str(score[2]))
      } else {
        sprintf("(%d)", length(category_issues))
      }
      add("-- ", py_title(category), " ", header_tail, " --")
      add("")
      add("  ", pad_field_right("rule", 6), pad_field_right("rule_description", 30),
          pad_field_right("column_field", 18), pad_field_right("severity", 10),
          pad_field_left("checks", 7), "  finding")
      add("  ", strrep("-", 116))

      for (issue in category_issues) {
        severity_upper <- toupper(issue$severity)
        rule_code <- issue$rule_code %||% ""
        rule_description <- issue$rule_description %||% ""
        column_field <- issue$column_field %||% "NA"
        finding <- issue$finding %||% issue$message
        checks <- as.numeric(issue$atomic_count %||% 1)
        checks_str <- if (checks == 0) "\u2014" else int_str(checks)

        if (nchar(rule_description) > 28) {
          rule_description <- paste0(substr(rule_description, 1, 26), "..")
        }
        if (nchar(column_field) > 16) {
          column_field <- paste0(substr(column_field, 1, 14), "..")
        }
        add("  ", pad_field_right(rule_code, 6), pad_field_right(rule_description, 30),
            pad_field_right(column_field, 18), pad_field_right(severity_upper, 10),
            pad_field_left(checks_str, 7), "  ", finding)
      }
    }
  } else {
    add("No validation issues found!")
  }

  add("")
  add(strrep("=", 120))
  add("END OF REPORT")
  add(strrep("=", 120))

  # clifpy joins with '\n' and writes in text mode: LF endings, no trailing NL.
  connection <- file(output_path, open = "wb")
  on.exit(close(connection))
  writeChar(paste(unlist(lines), collapse = "\n"), connection, eos = NULL, useBytes = TRUE)
  invisible(output_path)
}


# ---------------------------------------------------------------------------
# Consolidated CSV
# ---------------------------------------------------------------------------

# Escape one CSV field the way Python's csv module does under QUOTE_MINIMAL.
csv_escape_field <- function(field) {
  field <- as.character(field %||% "")
  if (grepl("[\",\r\n]", field)) {
    paste0("\"", gsub("\"", "\"\"", field, fixed = TRUE), "\"")
  } else {
    field
  }
}

#' Generate a consolidated CSV across multiple table DQA results
#'
#' Port of `report_generator.generate_consolidated_csv`. One row per issue
#' across all tables, plus a single row for tables with no issues or that are
#' absent. Output matches clifpy byte-for-byte, including CRLF line endings.
#'
#' @param table_results Named list mapping `table_name -> parsed DQA result |
#'   NULL` (e.g. from [collect_table_results()]).
#' @param output_path Path for the CSV file.
#' @param table_names Ordered vector of table names.
#' @param feedback_map Optional named list mapping `table_name -> feedback list`.
#' @param display_names Optional named list mapping `table_name -> display
#'   label`; falls back to `TABLE_DISPLAY_NAMES`.
#' @return `output_path`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' loaded <- collect_table_results("output/validation", c("patient", "adt"))
#' generate_consolidated_csv(loaded$results, tempfile(fileext = ".csv"),
#'                           c("patient", "adt"))
#' }
generate_consolidated_csv <- function(table_results, output_path, table_names,
                                      feedback_map = NULL, display_names = NULL) {
  if (is.null(feedback_map)) feedback_map <- list()
  if (is.null(display_names)) display_names <- TABLE_DISPLAY_NAMES

  field_names <- c(
    "table_name", "category", "rule_code", "rule_description",
    "check_type", "column_field", "severity", "passed", "message",
    "checks", "decision", "reason"
  )
  rows <- list()
  add_row <- function(values) {
    rows[[length(rows) + 1]] <<- vapply(field_names, function(name) values[[name]], character(1))
  }

  default_label <- function(table_name) py_title(gsub("_", " ", table_name, fixed = TRUE))

  for (table_name in table_names) {
    dqa_data <- table_results[[table_name]]
    label <- display_names[[table_name]] %||% default_label(table_name)
    feedback <- feedback_map[[table_name]]
    feedback_decisions <- if (!is.null(feedback)) feedback$user_decisions %||% list() else list()

    is_absent <- is.null(dqa_data) || isTRUE(dqa_data$absent)
    if (is_absent) {
      if (is.null(dqa_data)) {
        dqa_data <- build_absent_table_dqa_result(table_name)
      }
      presence <- dqa_data$conformance$table_presence %||% list()
      message_text <- (presence$errors %||% list())[[1]]$message %||% "Table not present in dataset"
      add_row(list(
        table_name = label, category = "conformance", rule_code = "C.1",
        rule_description = "table_presence", check_type = "Table Status",
        column_field = "NA", severity = "error", passed = "False",
        message = message_text, checks = "1", decision = "", reason = ""
      ))
      next
    }

    all_issues <- collect_dqa_issues(dqa_data)$all_issues

    if (length(all_issues) == 0) {
      add_row(list(
        table_name = label, category = "", rule_code = "", rule_description = "",
        check_type = "Summary", column_field = "NA", severity = "info",
        passed = "True", message = "All DQA checks passed", checks = "0",
        decision = "", reason = ""
      ))
      next
    }

    for (issue in all_issues) {
      error_id <- make_error_id(issue)
      decision_info <- feedback_decisions[[error_id]] %||% list()
      is_error <- identical(issue$severity, "error")
      add_row(list(
        table_name = label,
        category = issue$category,
        rule_code = issue$rule_code %||% "",
        rule_description = issue$rule_description %||% "",
        check_type = issue$check_type,
        column_field = issue$column_field %||% "NA",
        severity = issue$severity,
        passed = "False",
        message = issue$finding %||% issue$message,
        checks = sprintf("%d", as.integer(round(as.numeric(issue$atomic_count %||% 1)))),
        decision = if (is_error) decision_info$decision %||% "" else "",
        reason = if (is_error) decision_info$reason %||% "" else ""
      ))
    }
  }

  header_line <- paste(vapply(field_names, csv_escape_field, character(1)), collapse = ",")
  data_lines <- vapply(rows, function(row) {
    paste(vapply(row, csv_escape_field, character(1)), collapse = ",")
  }, character(1))
  all_lines <- c(header_line, data_lines)
  csv_text <- paste0(paste(all_lines, collapse = "\r\n"), "\r\n")

  connection <- file(output_path, open = "wb")
  on.exit(close(connection))
  writeChar(csv_text, connection, eos = NULL, useBytes = TRUE)
  invisible(output_path)
}


# ---------------------------------------------------------------------------
# HTML reports (portable stand-in for clifpy's PDFs)
# ---------------------------------------------------------------------------

# HTML-escape a string, matching html.escape (quote=True).
html_escape <- function(text) {
  text <- as.character(text %||% "")
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub("\"", "&quot;", text, fixed = TRUE)
  gsub("'", "&#x27;", text, fixed = TRUE)
}

# Inline SVG rendering of clifpy's YearlySparkBar. Okabe-Ito blue for a present
# year, vermillion for an absent one (discrete palette, per project style).
yearly_spark_svg <- function(yearly_counts) {
  years <- sort(as.integer(names(yearly_counts)))
  if (length(years) == 0) return("")
  counts <- vapply(years, function(year) as.numeric(yearly_counts[[as.character(year)]]), numeric(1))
  max_count <- max(c(counts, 1))
  present_color <- "#0072B2"
  absent_color <- "#D55E00"
  width <- 180
  height <- 20
  gap <- 1
  bar_width <- max(1, (width - gap * (length(years) - 1)) / length(years))
  bars <- character(0)
  for (index in seq_along(years)) {
    count <- counts[index]
    x <- (index - 1) * (bar_width + gap)
    if (count > 0) {
      bar_height <- max(2, (count / max_count) * height)
      fill <- present_color
    } else {
      bar_height <- height
      fill <- absent_color
    }
    bars <- c(bars, sprintf(
      "<rect x='%.2f' y='%.2f' width='%.2f' height='%.2f' fill='%s'/>",
      x, height - bar_height, bar_width, bar_height, fill
    ))
  }
  sprintf(
    "<svg width='%d' height='%d' role='img' aria-label='per-year presence'>%s</svg><div class='spark-years'>%d&ndash;%d</div>",
    width, height, paste(bars, collapse = ""), years[1], years[length(years)]
  )
}

# Shared <head>/<style> for the HTML reports (theme-aware, self-contained).
html_document <- function(title, body) {
  paste0(
    "<!doctype html>\n<html lang='en'>\n<head>\n",
    "<meta charset='utf-8'>\n",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>\n",
    "<title>", html_escape(title), "</title>\n",
    "<style>\n",
    ":root{--bg:#ffffff;--fg:#2c3e50;--muted:#5d6d7e;--primary:#1f4e79;",
    "--header:#f5f6fa;--pass:#e8f5e8;--fail:#ffeaea;--warn:#fff3e0;--border:#dadada;}\n",
    "@media (prefers-color-scheme: dark){:root{--bg:#12161c;--fg:#e6e9ee;--muted:#9aa7b4;",
    "--primary:#7fb2e6;--header:#1c2430;--pass:#1e3a24;--fail:#3a1e1e;--warn:#3a2f1a;--border:#33404d;}}\n",
    "body{background:var(--bg);color:var(--fg);font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;",
    "margin:0;padding:2rem;line-height:1.4;}\n",
    ".wrap{max-width:1100px;margin:0 auto;}\n",
    "h1{color:var(--primary);text-align:center;font-size:1.6rem;margin:0 0 .3rem;}\n",
    "h2{color:var(--fg);font-size:1.15rem;margin:1.6rem 0 .6rem;}\n",
    ".ts{color:var(--muted);font-size:.8rem;text-align:right;}\n",
    ".scroll{overflow-x:auto;}\n",
    "table{border-collapse:collapse;width:100%;font-size:.85rem;}\n",
    "th,td{border:1px solid var(--border);padding:.35rem .5rem;text-align:left;vertical-align:top;}\n",
    "th{background:var(--header);}\n",
    "td.num,th.num{text-align:right;}\n",
    "tr.error>td{background:var(--fail);} tr.warning>td{background:var(--warn);}\n",
    ".cell-pass{background:var(--pass);} .cell-fail{background:var(--fail);} .cell-warn{background:var(--warn);}\n",
    ".spark-years{color:var(--muted);font-size:.65rem;}\n",
    "</style>\n</head>\n<body>\n<div class='wrap'>\n",
    body,
    "\n</div>\n</body>\n</html>\n"
  )
}

#' Generate an HTML DQA report for a single table
#'
#' clifR's portable stand-in for clifpy's `generate_validation_pdf`: a
#' self-contained HTML document (no external assets) with the same DQA summary,
#' data profile and per-category issue detail sections. Temporal-consistency
#' rows render their per-year presence as an inline SVG spark bar.
#'
#' @inheritParams generate_text_report
#' @param feedback Optional feedback list with `user_decisions`.
#' @return `output_path`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' generate_validation_html(validation_data, "patient", tempfile(fileext = ".html"))
#' }
generate_validation_html <- function(validation_data, table_name, output_path,
                                     site_name = NULL, feedback = NULL) {
  collected <- collect_dqa_issues(validation_data)
  category_scores <- collected$category_scores
  all_issues <- collected$all_issues

  total_passed <- sum(vapply(category_scores, function(score) score[1], numeric(1)), 0)
  total_checks <- sum(vapply(category_scores, function(score) score[2], numeric(1)), 0)
  error_count <- sum_atomic_count(all_issues, "error")
  warning_count <- sum_atomic_count(all_issues, "warning")
  int_str <- function(value) sprintf("%d", as.integer(round(value)))

  body <- character(0)
  add <- function(...) body[[length(body) + 1]] <<- paste0(...)

  add("<p class='ts'>Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>")
  title_prefix <- if (!is.null(site_name)) paste0(html_escape(site_name), " ") else ""
  add("<h1>", title_prefix, "CLIF DQA Report Card</h1>")
  add("<h2>", html_escape(py_title(gsub("_", " ", table_name, fixed = TRUE))), " Table</h2>")

  # DQA summary
  add("<h2>DQA Summary</h2><div class='scroll'><table>")
  add("<tr><th>Category</th><th class='num'>Non-Error</th><th class='num'>Total</th>",
      "<th class='num'>Errors</th><th class='num'>Warnings</th></tr>")
  summary_row <- function(name, passed, total, errors, warnings) {
    error_cls <- if (errors > 0) "cell-fail" else "cell-pass"
    warn_cls <- if (warnings > 0) "cell-warn" else "cell-pass"
    add("<tr><td>", html_escape(name), "</td>",
        "<td class='num'>", int_str(passed), "</td>",
        "<td class='num'>", int_str(total), "</td>",
        "<td class='num ", error_cls, "'>", int_str(errors), "</td>",
        "<td class='num ", warn_cls, "'>", int_str(warnings), "</td></tr>")
  }
  for (category in DQA_CATEGORIES) {
    if (is.null(category_scores[[category]])) next
    score <- category_scores[[category]]
    category_issues <- Filter(function(issue) identical(issue$category, category), all_issues)
    summary_row(py_title(category), score[1], score[2],
                sum_atomic_count(category_issues, "error"),
                sum_atomic_count(category_issues, "warning"))
  }
  summary_row("Overall", total_passed, total_checks, error_count, warning_count)
  add("</table></div>")

  # Data profile
  table_stats <- validation_data$table_stats %||% list()
  if (length(table_stats) > 0) {
    add("<h2>Data Profile</h2>")
    add("<p class='ts' style='text-align:left'>Total Rows: ",
        py_int_comma(validation_data$total_rows %||% 0), "</p>")
    add("<div class='scroll'><table>")
    add("<tr><th>Column</th><th>Dtype</th><th class='num'>Null</th>",
        "<th class='num'>Null%</th><th class='num'>Unique</th><th>Min</th><th>Max</th></tr>")
    for (stat in table_stats) {
      null_pct <- as.numeric(stat$null_pct)
      pct_cls <- if (null_pct > 50) "cell-fail" else if (null_pct > 10) "cell-warn" else ""
      add("<tr><td>", html_escape(stat$column), "</td><td>", html_escape(stat$dtype), "</td>",
          "<td class='num'>", py_int_comma(stat$null_count), "</td>",
          "<td class='num ", pct_cls, "'>", sprintf("%.1f", null_pct), "%</td>",
          "<td class='num'>", py_int_comma(stat$unique), "</td>",
          "<td>", html_escape(stat$min %||% ""), "</td>",
          "<td>", html_escape(stat$max %||% ""), "</td></tr>")
    }
    add("</table></div>")
  }

  # Details
  if (length(all_issues) > 0) {
    add("<h2>Details</h2>")
    for (category in DQA_CATEGORIES) {
      category_issues <- Filter(function(issue) identical(issue$category, category), all_issues)
      if (length(category_issues) == 0) next
      score <- category_scores[[category]]
      header_tail <- if (!is.null(score)) sprintf("(%s/%s)", int_str(score[1]), int_str(score[2])) else sprintf("(%d)", length(category_issues))
      add("<h2>", py_title(category), " ", header_tail, "</h2>")
      add("<div class='scroll'><table>")
      add("<tr><th>rule</th><th>rule_description</th><th>column_field</th>",
          "<th>severity</th><th>finding</th><th class='num'>checks</th></tr>")
      for (issue in category_issues) {
        severity <- issue$severity
        checks <- as.numeric(issue$atomic_count %||% 1)
        checks_display <- if (checks == 0) "&mdash;" else int_str(checks)
        finding_cell <- html_escape(truncate_comment(issue$finding %||% issue$message))
        yearly_counts <- issue$details$yearly_counts
        if (!is.null(yearly_counts) && length(yearly_counts) > 0) {
          finding_cell <- paste0(finding_cell, "<br>", yearly_spark_svg(yearly_counts))
        }
        row_cls <- if (severity %in% c("error", "warning")) severity else ""
        add("<tr class='", row_cls, "'>",
            "<td>", html_escape(issue$rule_code %||% ""), "</td>",
            "<td>", html_escape(issue$rule_description %||% ""), "</td>",
            "<td>", html_escape(issue$column_field %||% "NA"), "</td>",
            "<td>", toupper(html_escape(severity)), "</td>",
            "<td>", finding_cell, "</td>",
            "<td class='num'>", checks_display, "</td></tr>")
      }
      add("</table></div>")
    }
  } else {
    add("<p>No validation issues found!</p>")
  }

  html <- html_document(
    paste0(if (!is.null(site_name)) paste0(site_name, " ") else "", "CLIF DQA Report Card"),
    paste(unlist(body), collapse = "\n")
  )
  connection <- file(output_path, open = "wb")
  on.exit(close(connection))
  writeChar(html, connection, eos = NULL, useBytes = TRUE)
  invisible(output_path)
}

#' @rdname generate_validation_html
#'
#' @description
#' `generate_validation_pdf()` is a thin alias kept for clifpy call-site
#' compatibility. R has no reportlab equivalent, so it forwards to
#' [generate_validation_html()] and emits a message noting the substitution.
#' The returned path is the HTML file.
#'
#' @export
generate_validation_pdf <- function(validation_data, table_name, output_path,
                                    site_name = NULL, feedback = NULL) {
  cli::cli_alert_info(
    "clifR produces an HTML DQA report rather than a PDF; writing {.path {output_path}}."
  )
  generate_validation_html(validation_data, table_name, output_path, site_name, feedback)
}


# ---------------------------------------------------------------------------
# Combined (multi-table) reports
# ---------------------------------------------------------------------------

# Sum "p/t" cells across the category columns of one overview row.
overview_row_overall <- function(cells) {
  total_passed <- 0
  total_count <- 0
  for (cell in cells) {
    if (identical(cell, "N/A")) next
    parts <- strsplit(cell, "/", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      passed_value <- suppressWarnings(as.integer(parts[1]))
      count_value <- suppressWarnings(as.integer(parts[2]))
      if (is.na(passed_value) || is.na(count_value)) next
      total_passed <- total_passed + passed_value
      total_count <- total_count + count_value
    }
  }
  if (total_count > 0) sprintf("%d/%d", total_passed, total_count) else "N/A"
}

#' Generate a combined HTML DQA overview across tables
#'
#' clifR's portable stand-in for clifpy's `generate_combined_validation_pdf`.
#' Renders the "DQA Overview" table — one row per table with Conformance,
#' Completeness, Plausibility and Overall columns, plus a totals row — as a
#' self-contained HTML document.
#'
#' @param table_results Named list mapping `table_name -> parsed DQA result |
#'   NULL`.
#' @param output_path Path for the HTML file.
#' @param table_names Ordered vector of table names (controls row order).
#' @param site_name Optional site/hospital label.
#' @param feedback_map Optional named list mapping `table_name -> feedback list`.
#' @param display_names Optional `table_name -> label` mapping; falls back to
#'   `TABLE_DISPLAY_NAMES`.
#' @return `output_path`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' loaded <- collect_table_results("output/validation", c("patient", "adt"))
#' generate_combined_validation_html(loaded$results, tempfile(fileext = ".html"),
#'                                   c("patient", "adt"))
#' }
generate_combined_validation_html <- function(table_results, output_path, table_names,
                                              site_name = NULL, feedback_map = NULL,
                                              display_names = NULL) {
  if (is.null(feedback_map)) feedback_map <- list()
  if (is.null(display_names)) display_names <- TABLE_DISPLAY_NAMES

  default_label <- function(table_name) py_title(gsub("_", " ", table_name, fixed = TRUE))

  overview_rows <- list()
  for (table_name in table_names) {
    dqa_data <- table_results[[table_name]]
    label <- display_names[[table_name]] %||% default_label(table_name)

    if (is.null(dqa_data) || isTRUE(dqa_data$absent)) {
      if (is.null(dqa_data)) {
        dqa_data <- build_absent_table_dqa_result(table_name)
      }
      expected <- dqa_data$expected_check_counts %||%
        build_absent_table_dqa_result(table_name)$expected_check_counts
      cells <- vapply(DQA_CATEGORIES, function(category) {
        if (identical(category, "conformance")) {
          n <- as.integer(expected[[category]] %||% 0)
          if (n > 0) sprintf("0/%d", n) else "N/A"
        } else {
          "N/A"
        }
      }, character(1), USE.NAMES = FALSE)
    } else {
      scores <- collect_dqa_issues(dqa_data)$category_scores
      cells <- vapply(DQA_CATEGORIES, function(category) {
        score <- scores[[category]]
        if (!is.null(score)) sprintf("%d/%d", as.integer(score[1]), as.integer(score[2])) else "N/A"
      }, character(1), USE.NAMES = FALSE)
    }
    overview_rows[[length(overview_rows) + 1]] <- list(
      label = label, cells = cells, overall = overview_row_overall(cells)
    )
  }

  totals_cells <- vapply(seq_along(DQA_CATEGORIES), function(column_index) {
    total_passed <- 0
    total_count <- 0
    for (row in overview_rows) {
      cell <- row$cells[column_index]
      if (identical(cell, "N/A")) next
      parts <- strsplit(cell, "/", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        passed_value <- suppressWarnings(as.integer(parts[1]))
        count_value <- suppressWarnings(as.integer(parts[2]))
        if (is.na(passed_value) || is.na(count_value)) next
        total_passed <- total_passed + passed_value
        total_count <- total_count + count_value
      }
    }
    if (total_count > 0) sprintf("%d/%d", total_passed, total_count) else "N/A"
  }, character(1))

  cell_html <- function(cell) {
    parts <- strsplit(cell, "/", fixed = TRUE)[[1]]
    class_name <- if (identical(cell, "N/A")) {
      ""
    } else if (length(parts) == 2 && identical(parts[1], parts[2])) {
      "cell-pass"
    } else {
      "cell-fail"
    }
    paste0("<td class='num ", class_name, "'>", html_escape(cell), "</td>")
  }

  body <- character(0)
  add <- function(...) body[[length(body) + 1]] <<- paste0(...)
  add("<p class='ts'>Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>")
  title_prefix <- if (!is.null(site_name)) paste0(html_escape(site_name), " ") else ""
  add("<h1>", title_prefix, "CLIF DQA Report Card</h1>")
  add("<h2>Combined Validation Report</h2>")
  add("<h2>DQA Overview</h2><div class='scroll'><table>")
  add("<tr><th>Table</th>",
      paste(vapply(DQA_CATEGORIES, function(category) paste0("<th class='num'>", py_title(category), "</th>"), character(1)), collapse = ""),
      "<th class='num'>Overall</th></tr>")
  for (row in overview_rows) {
    add("<tr><td>", html_escape(row$label), "</td>",
        paste(vapply(row$cells, cell_html, character(1)), collapse = ""),
        cell_html(row$overall), "</tr>")
  }
  add("<tr><td><strong>Total</strong></td>",
      paste(vapply(totals_cells, cell_html, character(1)), collapse = ""),
      cell_html(overview_row_overall(totals_cells)), "</tr>")
  add("</table></div>")

  html <- html_document(
    paste0(if (!is.null(site_name)) paste0(site_name, " ") else "", "CLIF DQA Report Card"),
    paste(unlist(body), collapse = "\n")
  )
  connection <- file(output_path, open = "wb")
  on.exit(close(connection))
  writeChar(html, connection, eos = NULL, useBytes = TRUE)
  invisible(output_path)
}

#' @rdname generate_combined_validation_html
#'
#' @description
#' `generate_combined_validation_pdf()` is a thin alias for clifpy call-site
#' compatibility that forwards to [generate_combined_validation_html()] and
#' announces the HTML substitution.
#'
#' @export
generate_combined_validation_pdf <- function(table_results, output_path, table_names,
                                             site_name = NULL, feedback_map = NULL,
                                             display_names = NULL) {
  cli::cli_alert_info(
    "clifR produces an HTML combined DQA report rather than a PDF; writing {.path {output_path}}."
  )
  generate_combined_validation_html(
    table_results, output_path, table_names, site_name, feedback_map, display_names
  )
}

#' Generate a combined DQA report (HTML + CSV) from persisted JSON results
#'
#' Port of `report_generator.generate_combined_report`. Loads per-table
#' `{table_name}_dqa.json` results and writes both a combined HTML overview
#' (`combined_validation_report.html`) and a consolidated CSV
#' (`consolidated_validation.csv`). Returns the HTML path (clifpy returns the
#' PDF path), or `NULL` when no table results were found.
#'
#' @param json_dir Directory of `{table_name}_dqa.json` files.
#' @param output_dir Directory where the HTML and CSV are written.
#' @param table_names Ordered vector of table names to include.
#' @param site_name Optional site/hospital label.
#' @param feedback_dir Optional directory of feedback JSON files.
#' @return Path to the generated HTML report, or `NULL` on failure/empty input.
#' @export
#'
#' @examples
#' \dontrun{
#' generate_combined_report("output/validation", "output/report",
#'                          c("patient", "adt", "labs"))
#' }
generate_combined_report <- function(json_dir, output_dir, table_names,
                                     site_name = NULL, feedback_dir = NULL) {
  tryCatch({
    loaded <- collect_table_results(json_dir, table_names, feedback_dir)
    table_results <- loaded$results
    feedback_map <- loaded$feedback_map

    analyzed_count <- sum(vapply(
      table_names, function(name) !is.null(table_results[[name]]), logical(1)
    ))
    if (analyzed_count == 0) {
      return(NULL)
    }

    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    html_path <- file.path(output_dir, "combined_validation_report.html")
    generate_combined_validation_html(
      table_results, html_path, table_names, site_name,
      feedback_map = feedback_map
    )

    csv_path <- file.path(output_dir, "consolidated_validation.csv")
    generate_consolidated_csv(
      table_results, csv_path, table_names, feedback_map = feedback_map
    )

    html_path
  }, error = function(condition) {
    cli::cli_alert_warning("Combined report generation failed: {conditionMessage(condition)}")
    NULL
  })
}
