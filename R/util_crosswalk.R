#' CLIF 2.1 to 3.0 value crosswalk
#'
#' Port of `clifpy/utils/crosswalk.py`. The dominant change from CLIF 2.1 to 3.0
#' is that the standardized `*_category` / `*_group` / `*_type` column **values**
#' were lowercased and snake_cased, with a minority of non-derivable renames
#' (`High Flow NC` -> `hfnc`, `DNR/DNI` -> `dnr_or_dni`, `Psychiatric Hospital`
#' -> `mental_health_hosp`).
#'
#' Most values are handled by [normalize_category_value()], a deterministic
#' lowercase + snake_case transform; the rest come from the curated resource at
#' `inst/schemas/crosswalks/clif_2.1_to_3.0.yaml`.
#'
#' The converter is non-mutating: it returns a converted copy of the input plus a
#' structured report. Values it cannot confidently map — 1-to-many splits such as
#' `albumin` -> `albumin_5`/`albumin_25`, or anything that does not resolve to a
#' valid 3.0 permissible value — are left unchanged and surfaced in the report.
#'
#' @name clif-crosswalk
NULL

#' The 16 CLIF beta tables
#'
#' The default scope for a 2.1 to 3.0 migration.
#' @export
BETA_TABLES <- c(
  "patient",
  "hospitalization",
  "adt",
  "vitals",
  "labs",
  "patient_assessments",
  "medication_admin_continuous",
  "medication_admin_intermittent",
  "respiratory_support",
  "position",
  "patient_procedures",
  "code_status",
  "crrt_therapy",
  "hospital_diagnosis",
  "microbiology_culture",
  "microbiology_susceptibility"
)

# Suffixes that mark a "standardized" column whose values are crosswalked.
CROSSWALK_STANDARDIZED_SUFFIXES <- c("_category", "_group", "_type")

CROSSWALK_FILENAME <- "clif_2.1_to_3.0.yaml"

# Cache for the bundled crosswalk so repeated calls do not re-read the YAML.
crosswalk_cache_environment <- new.env(parent = emptyenv())

#' Lowercase and snake_case a single category value
#'
#' Reproduces the CLIF 3.0 token convention for values that changed by case or
#' punctuation only. Non-character input is returned unchanged, so numeric flag
#' columns such as `tracheostomy` (0/1) pass through untouched. `NA` is preserved.
#'
#' Rules, applied in order:
#'
#' 1. Non-character or missing input is returned unchanged.
#' 2. Trim surrounding whitespace, then lowercase.
#' 3. `&` becomes `_and_`, `/` becomes `_or_`.
#' 4. Space, `-`, `,`, `(` and `)` each become `_`.
#' 5. Runs of `_` collapse to one; leading and trailing `_` are stripped.
#'
#' @param value A value or vector of values to normalize.
#'
#' @return The normalized value, same length as `value`.
#' @export
#'
#' @examples
#' normalize_category_value("Non-Hispanic")
#' normalize_category_value("l&d")
#' normalize_category_value("pulmonary vasodilators (IV)")
#' normalize_category_value(c("DNR/DNI", NA))
normalize_category_value <- function(value) {
  if (is.null(value) || !is.character(value)) {
    return(value)
  }

  normalized <- trimws(value)
  normalized <- tolower(normalized)
  normalized <- gsub("&", "_and_", normalized, fixed = TRUE)
  normalized <- gsub("/", "_or_", normalized, fixed = TRUE)
  for (replaced_character in c(" ", "-", ",", "(", ")")) {
    normalized <- gsub(replaced_character, "_", normalized, fixed = TRUE)
  }
  normalized <- gsub("_+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)

  normalized[is.na(value)] <- NA_character_
  normalized
}

#' Path to the bundled 2.1 to 3.0 crosswalk resource
#'
#' @return Absolute path to `clif_2.1_to_3.0.yaml`.
#' @keywords internal
bundled_crosswalk_path <- function() {
  file.path(system.file("schemas", package = "clifR"), "crosswalks", CROSSWALK_FILENAME)
}

#' Load the curated 2.1 to 3.0 crosswalk resource
#'
#' The bundled resource is cached after the first read; an explicit
#' `crosswalk_path` bypasses the cache.
#'
#' @param crosswalk_path Optional path to an alternative crosswalk YAML file.
#'
#' @return A named list with `from_version`, `to_version`, `renames` and
#'   `unresolved` elements. Missing `renames`/`unresolved` default to an empty
#'   list.
#' @export
#'
#' @examples
#' crosswalk <- load_crosswalk()
#' crosswalk$to_version
load_crosswalk <- function(crosswalk_path = NULL) {
  use_cache <- is.null(crosswalk_path)

  if (use_cache) {
    if (!is.null(crosswalk_cache_environment$crosswalk)) {
      return(crosswalk_cache_environment$crosswalk)
    }
    crosswalk_path <- bundled_crosswalk_path()
  }

  if (!file.exists(crosswalk_path)) {
    cli::cli_abort("Crosswalk resource not found: {.file {crosswalk_path}}")
  }

  crosswalk <- yaml::read_yaml(crosswalk_path)
  if (is.null(crosswalk)) {
    crosswalk <- list()
  }
  if (is.null(crosswalk$renames)) {
    crosswalk$renames <- list()
  }
  if (is.null(crosswalk$unresolved)) {
    crosswalk$unresolved <- list()
  }

  if (use_cache) {
    crosswalk_cache_environment$crosswalk <- crosswalk
  }
  crosswalk
}

#' Map column name to permissible values for a schema
#'
#' @param schema Parsed table schema, or `NULL`.
#' @return Named list of character vectors, one per column that defines
#'   `permissible_values`.
#' @keywords internal
schema_permissible_value_map <- function(schema) {
  if (is.null(schema) || is.null(schema$columns)) {
    return(list())
  }
  permissible_values_by_column <- list()
  for (column_definition in schema$columns) {
    permissible_values <- column_definition$permissible_values
    if (!is.null(permissible_values) && length(permissible_values) > 0) {
      permissible_values_by_column[[column_definition$name]] <-
        unlist(permissible_values, use.names = FALSE)
    }
  }
  permissible_values_by_column
}

#' Standardized column names for a table
#'
#' Union of `*_category`/`*_group`/`*_type` columns named in the 2.1 or 3.0
#' schema. Taking the union makes column discovery robust to the 2.1-to-3.0 flag
#' flip (for example `assessment_group` and `med_group` move from category to
#' group), since the column *name* is unchanged across versions.
#'
#' @param table_name snake_case CLIF table name.
#' @return Character vector of column names.
#' @keywords internal
standardized_columns_for_table <- function(table_name) {
  standardized_column_names <- character(0)
  for (clif_version in c("2.1", "3.0")) {
    schema <- suppressWarnings(tryCatch(
      load_schema(table_name, clif_version),
      error = function(condition) NULL
    ))
    if (is.null(schema) || is.null(schema$columns)) {
      next
    }
    for (column_definition in schema$columns) {
      column_name <- column_definition$name %||% ""
      if (any(endsWith(column_name, CROSSWALK_STANDARDIZED_SUFFIXES))) {
        standardized_column_names <- c(standardized_column_names, column_name)
      }
    }
  }
  unique(standardized_column_names)
}

#' Standardized columns present in a set of available columns
#'
#' @param table_name snake_case CLIF table name.
#' @param available_columns Character vector of column names present in the data.
#' @return Character vector, in `available_columns` order.
#' @keywords internal
crosswalk_target_columns <- function(table_name, available_columns) {
  standardized_column_names <- standardized_columns_for_table(table_name)
  available_columns[available_columns %in% standardized_column_names]
}

#' Empty change report skeleton
#'
#' @param table_name snake_case CLIF table name.
#' @return A named list with `table`, `from_version`, `to_version`, `columns`
#'   and `is_complete`.
#' @keywords internal
new_crosswalk_report <- function(table_name) {
  list(
    table = table_name,
    from_version = "2.1",
    to_version = "3.0",
    columns = list(),
    is_complete = TRUE
  )
}

#' Per-value counts for a column, in pandas `value_counts()` order
#'
#' Missing values are dropped; counts are ordered descending, ties broken by
#' first appearance.
#'
#' @param column_values A vector.
#' @return A named integer vector of counts keyed by value.
#' @keywords internal
value_counts_descending <- function(column_values) {
  present_values <- as.character(column_values[!is.na(column_values)])
  if (length(present_values) == 0) {
    return(stats::setNames(integer(0), character(0)))
  }
  first_appearance_order <- unique(present_values)
  counts <- table(factor(present_values, levels = first_appearance_order))
  counts <- as.integer(counts) |> stats::setNames(first_appearance_order)
  counts[order(-counts, seq_along(counts))]
}

#' Plan the conversion of one standardized column
#'
#' Given per-value counts for a column, build the old-to-new value map and the
#' column's section of the change report. This is the single source of truth for
#' crosswalk semantics; every backend feeds it a value-to-count mapping.
#'
#' @param table_name snake_case CLIF table name.
#' @param column_name Name of the standardized column.
#' @param value_counts Named integer vector of counts keyed by raw value.
#' @param crosswalk Parsed crosswalk resource.
#' @param permissible_values_30 Named list of CLIF 3.0 permissible values.
#'
#' @return A named list with `value_map` (named character vector) and
#'   `column_report`.
#' @keywords internal
plan_crosswalk_column <- function(table_name,
                                  column_name,
                                  value_counts,
                                  crosswalk,
                                  permissible_values_30) {
  column_renames <- crosswalk$renames[[table_name]][[column_name]] %||% list()
  column_unresolved <- crosswalk$unresolved[[table_name]][[column_name]] %||% list()
  allowed_values_30 <- permissible_values_30[[column_name]] %||% character(0)

  value_map <- character(0)
  ambiguous_entries <- list()
  unresolved_entries <- list()
  n_values_converted <- 0L

  for (raw_value in names(value_counts)) {
    value_count <- as.integer(value_counts[[raw_value]])

    if (raw_value %in% names(column_unresolved)) {
      unresolved_entry <- column_unresolved[[raw_value]] %||% list()
      value_map[[raw_value]] <- raw_value
      ambiguous_entries[[length(ambiguous_entries) + 1L]] <- list(
        original = raw_value,
        candidates = unlist(unresolved_entry$candidates %||% list(), use.names = FALSE),
        reason = unresolved_entry$reason %||% "",
        count = value_count
      )
      next
    }

    produced_value <- if (raw_value %in% names(column_renames)) {
      as.character(column_renames[[raw_value]])
    } else {
      normalize_category_value(raw_value)
    }

    value_map[[raw_value]] <- produced_value
    if (!identical(produced_value, raw_value)) {
      n_values_converted <- n_values_converted + value_count
    }
    if (length(allowed_values_30) > 0 && !produced_value %in% allowed_values_30) {
      unresolved_entries[[length(unresolved_entries) + 1L]] <- list(
        original = raw_value,
        produced = produced_value,
        count = value_count
      )
    }
  }

  list(
    value_map = value_map,
    column_report = list(
      n_values_converted = n_values_converted,
      ambiguous = ambiguous_entries,
      unresolved = unresolved_entries
    )
  )
}

#' Convert a table's standardized column values from CLIF 2.1 to 3.0
#'
#' Transforms values in `*_category`/`*_group`/`*_type` columns to their CLIF 3.0
#' form. Each value is mapped via the curated rename map, else by
#' [normalize_category_value()]. Values flagged as one-to-many `unresolved`, such
#' as `albumin`, are left unchanged, as are values that do not resolve to a valid
#' 3.0 permissible value. All such cases appear in the report.
#'
#' Column header names are not changed, and the input is never mutated.
#'
#' @param df A data frame of a site's table data in CLIF 2.1 format.
#' @param table_name snake_case CLIF table name, e.g. `"respiratory_support"`.
#' @param crosswalk_path Optional override path to the crosswalk resource.
#'
#' @return A named list with two elements:
#'   * `data` — a tibble copy of `df` with converted values.
#'   * `report` — a named list with `table`, `from_version`, `to_version`,
#'     `columns` (per-column `n_values_converted` / `ambiguous` / `unresolved`)
#'     and `is_complete`.
#' @export
#'
#' @examples
#' respiratory_support_21 <- data.frame(
#'   device_category = c("High Flow NC", "IMV", "Nasal Cannula")
#' )
#' converted <- crosswalk_table_2_1_to_3_0(respiratory_support_21, "respiratory_support")
#' converted$data$device_category
crosswalk_table_2_1_to_3_0 <- function(df, table_name, crosswalk_path = NULL) {
  crosswalk <- load_crosswalk(crosswalk_path)
  permissible_values_30 <- schema_permissible_value_map(
    suppressWarnings(load_schema(table_name, "3.0"))
  )

  target_columns <- crosswalk_target_columns(table_name, names(df))
  converted_data <- dplyr::as_tibble(df)
  report <- new_crosswalk_report(table_name)

  for (column_name in target_columns) {
    column_values <- converted_data[[column_name]]
    plan <- plan_crosswalk_column(
      table_name,
      column_name,
      value_counts_descending(column_values),
      crosswalk,
      permissible_values_30
    )

    if (is.character(column_values) && length(plan$value_map) > 0) {
      matched_positions <- match(column_values, names(plan$value_map))
      mapped_values <- unname(plan$value_map[matched_positions])
      converted_data[[column_name]] <- ifelse(is.na(matched_positions), column_values, mapped_values)
    }

    report$columns[[column_name]] <- plan$column_report
    if (length(plan$column_report$ambiguous) > 0 || length(plan$column_report$unresolved) > 0) {
      report$is_complete <- FALSE
    }
  }

  list(data = converted_data, report = report)
}

#' Crosswalk a CLIF 2.1 table file to 3.0, writing the result
#'
#' For tables too large to hold in memory comfortably. Reads `input_path`,
#' converts the standardized columns and writes the converted table to
#' `output_path`. Both paths may be `.parquet`/`.pq` or `.csv`; the format is
#' inferred from the extension. Returns the same report shape as
#' [crosswalk_table_2_1_to_3_0()], aggregated over the whole file.
#'
#' @param input_path Source file.
#' @param output_path Destination file.
#' @param table_name snake_case CLIF table name.
#' @param backend `"duckdb"` streams the whole transform in SQL, handling
#'   larger-than-RAM inputs; `"memory"` reads the file into R and delegates to
#'   [crosswalk_table_2_1_to_3_0()].
#' @param chunk_size Accepted for signature parity with clifpy's chunked pandas
#'   backend. Neither R backend needs it: the DuckDB backend streams and the
#'   in-memory backend reads the file whole.
#' @param crosswalk_path Optional override path to the crosswalk resource.
#'
#' @return The aggregated change report, as a named list.
#' @export
#'
#' @examples
#' \dontrun{
#' report <- crosswalk_file_2_1_to_3_0(
#'   "clif_labs_21.parquet", "clif_labs_30.parquet", "labs"
#' )
#' report$is_complete
#' }
crosswalk_file_2_1_to_3_0 <- function(input_path,
                                      output_path,
                                      table_name,
                                      backend = c("duckdb", "memory"),
                                      chunk_size = 1e6,
                                      crosswalk_path = NULL) {
  backend <- match.arg(backend)

  if (backend == "memory") {
    input_is_csv <- endsWith(tolower(input_path), ".csv")
    source_data <- if (input_is_csv) {
      readr::read_csv(input_path, show_col_types = FALSE)
    } else {
      arrow::read_parquet(input_path)
    }
    converted <- crosswalk_table_2_1_to_3_0(source_data, table_name, crosswalk_path)
    if (endsWith(tolower(output_path), ".csv")) {
      readr::write_csv(converted$data, output_path)
    } else {
      arrow::write_parquet(converted$data, output_path)
    }
    return(converted$report)
  }

  crosswalk <- load_crosswalk(crosswalk_path)
  permissible_values_30 <- schema_permissible_value_map(
    suppressWarnings(load_schema(table_name, "3.0"))
  )

  connection <- duckdb_connect()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  reader_expression <- if (endsWith(tolower(input_path), ".csv")) {
    sprintf("read_csv_auto(%s)", sql_quote_value(input_path))
  } else {
    sprintf("read_parquet(%s)", sql_quote_value(input_path))
  }

  available_columns <- DBI::dbGetQuery(
    connection,
    sprintf("DESCRIBE SELECT * FROM %s", reader_expression)
  )$column_name
  target_columns <- crosswalk_target_columns(table_name, available_columns)

  report <- new_crosswalk_report(table_name)
  case_expressions <- character(0)

  for (column_name in target_columns) {
    counts_frame <- DBI::dbGetQuery(connection, sprintf(
      'SELECT "%s" AS value, COUNT(*) AS n FROM %s WHERE "%s" IS NOT NULL GROUP BY "%s"',
      column_name, reader_expression, column_name, column_name
    ))
    value_counts <- stats::setNames(as.integer(counts_frame$n), as.character(counts_frame$value))
    value_counts <- value_counts[order(-value_counts, seq_along(value_counts))]

    plan <- plan_crosswalk_column(
      table_name, column_name, value_counts, crosswalk, permissible_values_30
    )
    report$columns[[column_name]] <- plan$column_report
    if (length(plan$column_report$ambiguous) > 0 || length(plan$column_report$unresolved) > 0) {
      report$is_complete <- FALSE
    }

    changed_values <- plan$value_map[names(plan$value_map) != unname(plan$value_map)]
    if (length(changed_values) > 0) {
      when_branches <- paste(sprintf(
        'WHEN "%s" = %s THEN %s',
        column_name,
        vapply(names(changed_values), sql_quote_value, character(1)),
        vapply(unname(changed_values), sql_quote_value, character(1))
      ), collapse = " ")
      case_expressions[[column_name]] <- sprintf(
        'CASE %s ELSE "%s" END AS "%s"', when_branches, column_name, column_name
      )
    }
  }

  select_clause <- if (length(case_expressions) > 0) {
    excluded_columns <- paste(sprintf('"%s"', names(case_expressions)), collapse = ", ")
    paste0("* EXCLUDE (", excluded_columns, "), ", paste(case_expressions, collapse = ", "))
  } else {
    "*"
  }

  output_format_clause <- if (endsWith(tolower(output_path), ".csv")) {
    "FORMAT CSV, HEADER"
  } else {
    "FORMAT PARQUET"
  }

  DBI::dbExecute(connection, sprintf(
    "COPY (SELECT %s FROM %s) TO %s (%s)",
    select_clause, reader_expression, sql_quote_value(output_path), output_format_clause
  ))

  report
}
