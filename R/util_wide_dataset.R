#' Wide dataset creation utilities
#'
#' Port of `clifpy/utils/wide_dataset.py`. Wide datasets are built by a DuckDB SQL
#' pipeline: narrow tables are pivoted with DuckDB's `PIVOT` statement, keyed on a
#' `hospitalization_id`/minute "combo id", and joined onto the union of every event
#' timestamp. The same SQL is issued here through R's `duckdb` package so the
#' resulting columns, column order and values match the Python implementation.
#'
#' Two behaviours from clifpy are reproduced deliberately even though they look like
#' oversights, because parity is the requirement:
#'
#' * `save_to_data_location` and `return_dataframe` only take effect in batch mode
#'   (`batch_size > 0` and more hospitalizations than `batch_size`); single-pass
#'   processing always returns the dataset and never writes it.
#' * `category_filters` means different things per table type — category *values* for
#'   pivot tables, column *names* for already-wide tables.
#'
#' Timezone handling: clifpy holds timezone-aware pandas columns and lets DuckDB's
#' session timezone decide the wall clock used by `strftime`. R's `duckdb` registers
#' `POSIXct` as a naive `TIMESTAMP` holding the true UTC instant, so the instant
#' survives registration unshifted (distinct instants stay distinct even across a DST
#' fall-back) and the site-local wall clock needed for combo ids is reconstructed in
#' SQL with a double `AT TIME ZONE`. Returned datetimes are re-labelled to the site
#' timezone without shifting the instant. See the timezone helpers below.
#'
#' @name clif-wide-dataset
NULL

# Cached parse of wide_tables_config.yaml, mirroring clifpy's module-level cache.
wide_tables_config_cache <- new.env(parent = emptyenv())

#' Load the wide dataset table configuration
#'
#' @return Parsed `wide_tables_config.yaml` as a named list.
#' @keywords internal
load_wide_tables_config <- function() {
  if (is.null(wide_tables_config_cache$config)) {
    wide_tables_config_cache$config <- load_shared_config("wide_tables_config.yaml")
  }
  wide_tables_config_cache$config
}

#' Names of tables supported by the wide dataset builder
#'
#' @param table_type Optional filter: `"pivot"`, `"wide"` or `"base"`.
#' @return Character vector of table names, in config order.
#' @keywords internal
get_supported_tables <- function(table_type = NULL) {
  tables <- load_wide_tables_config()$tables
  supported_table_names <- character(0)
  for (table_name in names(tables)) {
    table_config <- tables[[table_name]]
    if (isTRUE(table_config$supported)) {
      if (is.null(table_type) || identical(table_config$type, table_type)) {
        supported_table_names <- c(supported_table_names, table_name)
      }
    }
  }
  supported_table_names
}

#' Configuration block for one table
#'
#' @param table_name snake_case table name.
#' @return Named list, or `NULL` when the table is not in the config.
#' @keywords internal
get_table_config <- function(table_name) {
  load_wide_tables_config()$tables[[table_name]]
}

#' Timestamp column configured for a table
#'
#' @param table_name snake_case table name.
#' @return Column name, or `NULL`.
#' @keywords internal
get_timestamp_column <- function(table_name) {
  table_config <- get_table_config(table_name)
  if (is.null(table_config)) {
    return(NULL)
  }
  table_config$timestamp_column
}

#' First configured fallback timestamp column present in the data
#'
#' @param table_name snake_case table name.
#' @param columns Column names available in the loaded table.
#' @return Column name, or `NULL` when no alternative is present.
#' @keywords internal
find_alternative_timestamp <- function(table_name, columns) {
  table_config <- get_table_config(table_name)
  if (is.null(table_config)) {
    return(NULL)
  }
  alternative_columns <- table_config$alternative_timestamps
  if (is.null(alternative_columns)) {
    return(NULL)
  }
  for (alternative_column in unlist(alternative_columns, use.names = FALSE)) {
    if (alternative_column %in% columns) {
      return(alternative_column)
    }
  }
  NULL
}

# ---------------------------------------------------------------------------
# Timezone helpers
# ---------------------------------------------------------------------------
#
# clifpy carries timezone-aware pandas columns and lets DuckDB's TIMESTAMPTZ type
# and session timezone decide the wall clock that `strftime` produces for combo ids.
# R's `duckdb` package instead reads a POSIXct's underlying epoch-seconds and stores
# it as a *naive* TIMESTAMP holding the UTC wall clock, ignoring the vector's `tzone`
# label. So the true instant survives registration unchanged (distinct instants stay
# distinct even across a DST fall-back), and the site-local wall clock needed for
# combo ids is reconstructed inside SQL with a double `AT TIME ZONE` instead of by
# shifting the data in R. Shifting in R with `force_tz` would collapse the two
# instants that share a wall clock during a fall-back hour, which clifpy keeps apart.

# Re-label every POSIXct column to UTC, keeping the wall clock. Used by
# convert_wide_to_hourly to mirror pandas' tz_localize(None): the window maths runs
# on the wall clock of whatever timezone the wide dataset was labelled in.
strip_datetime_timezones <- function(data) {
  for (column_name in names(data)) {
    column_values <- data[[column_name]]
    if (inherits(column_values, "POSIXct")) {
      data[[column_name]] <- lubridate::force_tz(column_values, "UTC")
    }
  }
  data
}

# Re-label naive UTC instants coming back from DuckDB into the site timezone,
# without shifting the instant, so the wide dataset's datetime columns display in the
# site zone exactly like clifpy's timezone-aware output while carrying the same
# underlying instant compared by the parity tests.
relabel_datetime_timezones <- function(data, timezone) {
  if (is.null(timezone)) {
    return(data)
  }
  for (column_name in names(data)) {
    column_values <- data[[column_name]]
    if (inherits(column_values, "POSIXct")) {
      attr(column_values, "tzone") <- timezone
      data[[column_name]] <- column_values
    }
  }
  data
}

# SQL expression yielding the site-local wall clock of a naive UTC timestamp column,
# for use inside strftime when building combo ids. Reproduces the wall clock clifpy's
# session-timezone strftime produces from a TIMESTAMPTZ.
site_local_expression <- function(timestamp_identifier, site_timezone) {
  sprintf(
    "(%s AT TIME ZONE 'UTC') AT TIME ZONE '%s'",
    timestamp_identifier, site_timezone
  )
}

# minute-resolution combo id: hospitalization_id + site-local wall clock.
combo_id_expression <- function(timestamp_identifier, site_timezone, alias = TRUE) {
  expression <- sprintf(
    "hospitalization_id || '_' || strftime(%s, '%%Y%%m%%d%%H%%M')",
    site_local_expression(timestamp_identifier, site_timezone)
  )
  if (alias) paste(expression, "AS combo_id") else expression
}

# Register a data frame as a materialized DuckDB table, as clifpy does with
# register() + CREATE TABLE AS + unregister(). The frame is registered as-is so the
# true instant (POSIXct epoch seconds) reaches DuckDB unshifted.
register_as_duckdb_table <- function(connection, table_name, data) {
  duckdb::duckdb_register(connection, "clifr_temp_registration", data)
  DBI::dbExecute(
    connection,
    sprintf('CREATE OR REPLACE TABLE %s AS SELECT * FROM clifr_temp_registration', quote_identifier(table_name))
  )
  duckdb::duckdb_unregister(connection, "clifr_temp_registration")
  invisible(table_name)
}

quote_identifier <- function(identifier) {
  paste0('"', gsub('"', '""', identifier, fixed = TRUE), '"')
}

duckdb_table_names <- function(connection) {
  DBI::dbGetQuery(connection, "SHOW TABLES")$name
}

duckdb_column_names <- function(connection, table_name) {
  names(DBI::dbGetQuery(connection, sprintf("SELECT * FROM %s LIMIT 0", quote_identifier(table_name))))
}

# ---------------------------------------------------------------------------
# create_wide_dataset
# ---------------------------------------------------------------------------

#' Create a wide time-series dataset from CLIF tables
#'
#' Joins the base cohort (hospitalization, patient, ADT) to the union of every event
#' timestamp across the requested tables, pivoting narrow tables into one column per
#' category. Port of `clifpy.utils.wide_dataset.create_wide_dataset`.
#'
#' @param clif_instance A [ClifOrchestrator] object (or any list-like object exposing
#'   `timezone`, `data_directory` and one field per CLIF table, each with a `df`).
#' @param optional_tables Deprecated. Used only when `category_filters` is empty, in
#'   which case each named table is loaded with no category filter.
#' @param category_filters Named list mapping table name to a character vector. For
#'   **pivot** tables the values are category values to keep and pivot into columns;
#'   for **wide** tables (e.g. `respiratory_support`) they are column names to keep.
#'   The names of this list determine which tables are loaded.
#' @param sample When `TRUE`, randomly select 20 hospitalizations.
#' @param hospitalization_ids Optional character vector of hospitalization IDs.
#' @param cohort_df Optional data frame with `hospitalization_id`, `start_time` and
#'   `end_time`; events outside each window are dropped.
#' @param output_format `"dataframe"`, `"csv"` or `"parquet"`.
#' @param save_to_data_location Whether to write the result into `data_directory`.
#'   Only honoured in batch mode, matching clifpy.
#' @param output_filename File name stem for the saved file. Defaults to
#'   `wide_dataset_YYYYMMDD_HHMMSS`.
#' @param return_dataframe Whether to return the dataset when saving. Only honoured
#'   in batch mode, matching clifpy.
#' @param base_table_columns Deprecated and unused; base columns are selected
#'   automatically.
#' @param batch_size Number of hospitalizations per batch. `0` disables batching.
#' @param memory_limit Optional DuckDB memory limit, e.g. `"8GB"`.
#' @param threads Optional DuckDB thread count.
#' @param show_progress Whether to report per-batch progress.
#'
#' @return A tibble with one row per hospitalization per event time, or `NULL` when
#'   batching is used with `return_dataframe = FALSE`.
#' @export
#'
#' @examples
#' \dontrun{
#' orchestrator <- ClifOrchestrator$new(
#'   data_directory = "data/clif", filetype = "parquet", timezone = "UTC"
#' )
#' wide_dataset <- create_wide_dataset(
#'   orchestrator,
#'   category_filters = list(
#'     vitals = c("heart_rate", "sbp", "spo2"),
#'     labs = c("hemoglobin", "creatinine"),
#'     respiratory_support = c("device_category", "fio2_set")
#'   )
#' )
#' }
create_wide_dataset <- function(clif_instance,
                                optional_tables = NULL,
                                category_filters = NULL,
                                sample = FALSE,
                                hospitalization_ids = NULL,
                                cohort_df = NULL,
                                output_format = "dataframe",
                                save_to_data_location = FALSE,
                                output_filename = NULL,
                                return_dataframe = TRUE,
                                base_table_columns = NULL,
                                batch_size = 1000,
                                memory_limit = NULL,
                                threads = NULL,
                                show_progress = TRUE) {
  if (!is.null(cohort_df)) {
    required_cohort_columns <- c("hospitalization_id", "start_time", "end_time")
    missing_cohort_columns <- setdiff(required_cohort_columns, names(cohort_df))
    if (length(missing_cohort_columns) > 0) {
      cli::cli_abort(
        "{.arg cohort_df} must contain columns {.val {required_cohort_columns}}. Missing: {.val {missing_cohort_columns}}"
      )
    }
    cohort_df$hospitalization_id <- as.character(cohort_df$hospitalization_id)
    for (time_column in c("start_time", "end_time")) {
      if (!inherits(cohort_df[[time_column]], "POSIXct")) {
        cohort_df[[time_column]] <- as.POSIXct(cohort_df[[time_column]], tz = clif_instance$timezone %||% "UTC")
      }
    }
  }

  pivot_table_names <- get_supported_tables(table_type = "pivot")
  wide_table_names <- get_supported_tables(table_type = "wide")

  if (is.null(category_filters)) {
    category_filters <- list()
  }
  if (length(optional_tables) > 0 && length(category_filters) == 0) {
    cli::cli_alert_warning(
      "{.arg optional_tables} is deprecated. Converting to {.arg category_filters} format."
    )
    category_filters <- stats::setNames(
      replicate(length(optional_tables), character(0), simplify = FALSE),
      optional_tables
    )
  }
  tables_to_load <- names(category_filters)

  ensure_table_loaded(clif_instance, "patient")
  ensure_table_loaded(clif_instance, "hospitalization")
  ensure_table_loaded(clif_instance, "adt")
  for (table_name in tables_to_load) {
    ensure_table_loaded(clif_instance, table_name)
  }

  site_timezone <- clif_instance$timezone %||% "UTC"

  connection <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(connection, sprintf("SET timezone = '%s'", site_timezone))
  DBI::dbExecute(connection, "SET preserve_insertion_order = false")
  if (!is.null(memory_limit)) {
    DBI::dbExecute(connection, sprintf("SET memory_limit = '%s'", memory_limit))
  }
  if (!is.null(threads)) {
    DBI::dbExecute(connection, sprintf("SET threads = %d", as.integer(threads)))
  }

  hospitalization_data <- clif_instance$hospitalization$df

  if (!is.null(hospitalization_ids)) {
    required_hospitalization_ids <- hospitalization_ids
  } else if (!is.null(cohort_df)) {
    required_hospitalization_ids <- unique(cohort_df$hospitalization_id)
  } else if (isTRUE(sample)) {
    all_hospitalization_ids <- unique(hospitalization_data$hospitalization_id)
    required_hospitalization_ids <- sample(
      all_hospitalization_ids,
      size = min(20L, length(all_hospitalization_ids)),
      replace = FALSE
    )
  } else {
    required_hospitalization_ids <- unique(hospitalization_data$hospitalization_id)
  }

  hospitalization_required_columns <- c("hospitalization_id", "patient_id", "age_at_admission")
  hospitalization_available_columns <- intersect(hospitalization_required_columns, names(hospitalization_data))
  hospitalization_data <- hospitalization_data[
    hospitalization_data$hospitalization_id %in% required_hospitalization_ids,
    hospitalization_available_columns,
    drop = FALSE
  ]
  patient_data <- clif_instance$patient$df[, "patient_id", drop = FALSE]

  adt_data <- clif_instance$adt$df
  adt_data <- adt_data[adt_data$hospitalization_id %in% required_hospitalization_ids, , drop = FALSE]

  if (!is.null(cohort_df) && "in_dttm" %in% names(adt_data)) {
    adt_data <- filter_to_cohort_window(adt_data, cohort_df, "in_dttm")
  }

  adt_columns <- names(adt_data)
  adt_columns <- adt_columns[!grepl("_name$", adt_columns) & adt_columns != "patient_id"]
  adt_data <- adt_data[, adt_columns, drop = FALSE]

  if (batch_size > 0 && length(required_hospitalization_ids) > batch_size) {
    return(process_in_batches(
      connection, clif_instance, required_hospitalization_ids, patient_data,
      hospitalization_data, adt_data, tables_to_load, category_filters,
      pivot_table_names, wide_table_names, batch_size, show_progress,
      save_to_data_location, output_filename, output_format, return_dataframe,
      cohort_df, site_timezone
    ))
  }

  process_hospitalizations(
    connection, clif_instance, required_hospitalization_ids, patient_data,
    hospitalization_data, adt_data, tables_to_load, category_filters,
    pivot_table_names, wide_table_names, show_progress, cohort_df, site_timezone
  )
}

# Auto-load a table the way clifpy's orchestrator does before delegating here.
ensure_table_loaded <- function(clif_instance, table_name) {
  existing_table <- tryCatch(clif_instance[[table_name]], error = function(condition) NULL)
  if (!is.null(existing_table)) {
    return(invisible(existing_table))
  }
  load_table_method <- tryCatch(clif_instance$load_table, error = function(condition) NULL)
  if (!is.function(load_table_method)) {
    return(invisible(NULL))
  }
  tryCatch(
    load_table_method(table_name),
    error = function(condition) {
      cli::cli_alert_warning("Could not load {.val {table_name}}: {conditionMessage(condition)}")
      NULL
    }
  )
}

# Inner-join onto the cohort windows and keep rows whose timestamp falls inside.
filter_to_cohort_window <- function(data, cohort_df, timestamp_column) {
  windows <- cohort_df[, c("hospitalization_id", "start_time", "end_time"), drop = FALSE]
  joined <- dplyr::inner_join(data, windows, by = "hospitalization_id")
  keep_rows <- !is.na(joined[[timestamp_column]]) &
    joined[[timestamp_column]] >= joined$start_time &
    joined[[timestamp_column]] <= joined$end_time
  joined <- joined[keep_rows, , drop = FALSE]
  joined[, setdiff(names(joined), c("start_time", "end_time")), drop = FALSE]
}

# ---------------------------------------------------------------------------
# Single-pass processing
# ---------------------------------------------------------------------------

process_hospitalizations <- function(connection,
                                     clif_instance,
                                     required_hospitalization_ids,
                                     patient_data,
                                     hospitalization_data,
                                     adt_data,
                                     tables_to_load,
                                     category_filters,
                                     pivot_table_names,
                                     wide_table_names,
                                     show_progress,
                                     cohort_df = NULL,
                                     site_timezone = "UTC") {
  base_cohort <- dplyr::inner_join(hospitalization_data, patient_data, by = "patient_id")

  register_as_duckdb_table(connection, "base_cohort", base_cohort)
  register_as_duckdb_table(connection, "adt", adt_data)

  event_time_queries <- character(0)
  pivoted_table_lookup <- list()
  raw_table_lookup <- list()

  if ("in_dttm" %in% names(adt_data)) {
    event_time_queries <- c(event_time_queries, paste(
      "SELECT DISTINCT hospitalization_id, in_dttm AS event_time",
      "FROM adt",
      "WHERE in_dttm IS NOT NULL",
      sep = "\n"
    ))
  }

  for (table_name in tables_to_load) {
    table_object <- tryCatch(clif_instance[[table_name]], error = function(condition) NULL)
    if (is.null(table_object)) {
      cli::cli_alert_warning("{.val {table_name}} not loaded in CLIF instance, skipping")
      next
    }

    table_config <- get_table_config(table_name)
    converted_data <- tryCatch(table_object$df_converted, error = function(condition) NULL)
    if (!is.null(table_config) && isTRUE(table_config$supports_unit_conversion) && !is.null(converted_data)) {
      table_data <- converted_data[converted_data$hospitalization_id %in% required_hospitalization_ids, , drop = FALSE]
    } else {
      table_data <- table_object$df
      table_data <- table_data[table_data$hospitalization_id %in% required_hospitalization_ids, , drop = FALSE]
    }

    if (nrow(table_data) == 0) {
      cli::cli_alert_warning("No data found in {.val {table_name}} for selected hospitalizations")
      next
    }

    if (table_name %in% wide_table_names && table_name %in% names(category_filters)) {
      required_columns <- "hospitalization_id"
      configured_timestamp_column <- get_timestamp_column(table_name)
      if (!is.null(configured_timestamp_column)) {
        required_columns <- c(required_columns, configured_timestamp_column)
      }
      required_columns <- c(required_columns, category_filters[[table_name]])
      available_columns <- intersect(required_columns, names(table_data))
      missing_columns <- setdiff(required_columns, names(table_data))
      if (length(missing_columns) > 0) {
        cli::cli_alert_warning("Columns not found in {.val {table_name}}: {.val {missing_columns}}")
      }
      if (length(available_columns) > 0) {
        table_data <- table_data[, available_columns, drop = FALSE]
      }
    }

    timestamp_column <- get_timestamp_column(table_name)
    if (!is.null(timestamp_column) && !timestamp_column %in% names(table_data)) {
      timestamp_column <- find_alternative_timestamp(table_name, names(table_data))
    }
    if (is.null(timestamp_column) || !timestamp_column %in% names(table_data)) {
      cli::cli_alert_warning("No timestamp column found for {.val {table_name}}, skipping")
      next
    }

    if (!is.null(cohort_df)) {
      table_data <- filter_to_cohort_window(table_data, cohort_df, timestamp_column)
    }

    raw_table_name <- paste0(table_name, "_raw")
    register_as_duckdb_table(connection, raw_table_name, table_data)
    raw_table_lookup[[table_name]] <- raw_table_name

    if (table_name %in% pivot_table_names) {
      pivoted_table_name <- pivot_table_duckdb(
        connection, table_name, table_data, timestamp_column, category_filters, site_timezone
      )
      if (!is.null(pivoted_table_name)) {
        pivoted_table_lookup[[table_name]] <- pivoted_table_name
        event_time_queries <- c(event_time_queries, sprintf(
          "SELECT DISTINCT hospitalization_id, %s AS event_time\nFROM %s\nWHERE %s IS NOT NULL",
          quote_identifier(timestamp_column), quote_identifier(raw_table_name),
          quote_identifier(timestamp_column)
        ))
      }
    } else {
      event_time_queries <- c(event_time_queries, sprintf(
        "SELECT DISTINCT hospitalization_id, %s AS event_time\nFROM %s\nWHERE %s IS NOT NULL",
        quote_identifier(timestamp_column), quote_identifier(raw_table_name),
        quote_identifier(timestamp_column)
      ))
    }
  }

  if (length(event_time_queries) == 0) {
    cli::cli_alert_warning("No event times found, returning base cohort only")
    return(dplyr::as_tibble(base_cohort))
  }

  build_wide_dataset(
    connection, base_cohort, event_time_queries, pivoted_table_lookup,
    raw_table_lookup, tables_to_load, pivot_table_names, category_filters,
    cohort_df, site_timezone
  )
}

# ---------------------------------------------------------------------------
# Pivoting
# ---------------------------------------------------------------------------

pivot_table_duckdb <- function(connection, table_name, table_data, timestamp_column, category_filters, site_timezone = "UTC") {
  table_config <- get_table_config(table_name)
  if (is.null(table_config)) {
    cli::cli_alert_warning("No configuration found for {.val {table_name}}")
    return(NULL)
  }

  category_column <- table_config$category_column
  value_column <- table_config$value_column

  has_converted_medications <- FALSE
  unit_column <- NULL
  if (isTRUE(table_config$supports_unit_conversion)) {
    converted_value_column <- table_config$converted_value_column
    converted_unit_column <- table_config$converted_unit_column
    if (!is.null(converted_value_column) && !is.null(converted_unit_column) &&
      converted_value_column %in% names(table_data) && converted_unit_column %in% names(table_data)) {
      has_converted_medications <- TRUE
      value_column <- converted_value_column
      unit_column <- converted_unit_column
    }
  }

  if (is.null(category_column) || is.null(value_column)) {
    cli::cli_alert_warning("No pivot configuration for {.val {table_name}}")
    return(NULL)
  }
  if (!category_column %in% names(table_data) || !value_column %in% names(table_data)) {
    cli::cli_alert_warning(
      "Required columns {.val {category_column}} or {.val {value_column}} not found in {.val {table_name}}"
    )
    return(NULL)
  }

  filter_clause <- ""
  if (table_name %in% names(category_filters) && length(category_filters[[table_name]]) > 0) {
    quoted_categories <- paste(gsub("'", "''", category_filters[[table_name]], fixed = TRUE), collapse = "','")
    filter_clause <- sprintf("AND %s IN ('%s')", quote_identifier(category_column), quoted_categories)
  }

  pivoted_table_name <- paste0(table_name, "_pivoted")
  raw_table_name <- paste0(table_name, "_raw")
  combo_id_select <- combo_id_expression(quote_identifier(timestamp_column), site_timezone)

  if (has_converted_medications) {
    pivot_query <- sprintf(
      "CREATE OR REPLACE TABLE %s AS
       WITH pivot_data AS (
         SELECT DISTINCT
           %s as value,
           %s || '_' ||
           REPLACE(REPLACE(REPLACE(REPLACE(%s, '/', '_'), '-', '_'), ' ', '_'), '.', '_')
           AS category_for_pivot,
           %s
         FROM %s
         WHERE %s IS NOT NULL %s
       )
       PIVOT pivot_data
       ON category_for_pivot
       USING first(value)
       GROUP BY combo_id",
      quote_identifier(pivoted_table_name),
      quote_identifier(value_column),
      quote_identifier(category_column),
      quote_identifier(unit_column),
      combo_id_select,
      quote_identifier(raw_table_name),
      quote_identifier(timestamp_column),
      filter_clause
    )
  } else {
    pivot_query <- sprintf(
      "CREATE OR REPLACE TABLE %s AS
       WITH pivot_data AS (
         SELECT DISTINCT
           %s,
           %s,
           %s
         FROM %s
         WHERE %s IS NOT NULL %s
       )
       PIVOT pivot_data
       ON %s
       USING first(%s)
       GROUP BY combo_id",
      quote_identifier(pivoted_table_name),
      quote_identifier(value_column),
      quote_identifier(category_column),
      combo_id_select,
      quote_identifier(raw_table_name),
      quote_identifier(timestamp_column),
      filter_clause,
      quote_identifier(category_column),
      quote_identifier(value_column)
    )
  }

  tryCatch(
    {
      DBI::dbExecute(connection, pivot_query)
      pivoted_table_name
    },
    error = function(condition) {
      cli::cli_alert_danger("Error pivoting {.val {table_name}}: {conditionMessage(condition)}")
      NULL
    }
  )
}

# ---------------------------------------------------------------------------
# Final join
# ---------------------------------------------------------------------------

build_wide_dataset <- function(connection,
                               base_cohort,
                               event_time_queries,
                               pivoted_table_lookup,
                               raw_table_lookup,
                               tables_to_load,
                               pivot_table_names,
                               category_filters,
                               cohort_df = NULL,
                               site_timezone = "UTC") {
  union_query <- paste(event_time_queries, collapse = "\n UNION ALL \n")

  # clifpy grows the select list by repeatedly replacing the literal "SELECT ec.*",
  # so each newly handled table's columns land in front of the previously added
  # ones. Prepending to a vector reproduces that ordering exactly.
  extra_select_expressions <- character(0)

  existing_table_names <- duckdb_table_names(connection)

  if ("adt" %in% existing_table_names) {
    adt_column_names <- setdiff(duckdb_column_names(connection, "adt"), "hospitalization_id")
    if (length(adt_column_names) > 0) {
      extra_select_expressions <- c(
        extra_select_expressions,
        sprintf("adt_combo.%s", quote_identifier(adt_column_names))
      )
    }
  }

  for (table_name in names(pivoted_table_lookup)) {
    pivoted_table_name <- pivoted_table_lookup[[table_name]]
    pivot_column_names <- setdiff(duckdb_column_names(connection, pivoted_table_name), "combo_id")
    if (length(pivot_column_names) > 0) {
      extra_select_expressions <- c(
        sprintf("%s.%s", quote_identifier(pivoted_table_name), quote_identifier(pivot_column_names)),
        extra_select_expressions
      )
    }
  }

  for (table_name in tables_to_load) {
    if (!table_name %in% pivot_table_names && table_name %in% names(raw_table_lookup)) {
      timestamp_column <- get_timestamp_column(table_name)
      if (is.null(timestamp_column)) {
        next
      }
      raw_column_names <- duckdb_column_names(connection, raw_table_lookup[[table_name]])
      table_column_names <- setdiff(raw_column_names, c("hospitalization_id", timestamp_column))
      if (length(table_column_names) > 0) {
        extra_select_expressions <- c(
          sprintf("%s_combo.%s", table_name, quote_identifier(table_column_names)),
          extra_select_expressions
        )
      }
    }
  }

  select_clause <- "SELECT ec.*"
  if (length(extra_select_expressions) > 0) {
    select_clause <- paste0(select_clause, ", ", paste(extra_select_expressions, collapse = ", "))
  }

  query <- sprintf(
    "WITH all_events AS (
       SELECT DISTINCT hospitalization_id, event_time
       FROM (%s) uni_time
     ),
     expanded_cohort AS (
       SELECT
         a.*,
         b.event_time,
         a.hospitalization_id || '_' || strftime(%s, '%%Y%%m%%d%%H%%M') AS combo_id
       FROM base_cohort a
       INNER JOIN all_events b ON a.hospitalization_id = b.hospitalization_id
     )
     %s FROM expanded_cohort ec",
    union_query, site_local_expression("b.event_time", site_timezone), select_clause
  )

  if ("adt" %in% existing_table_names) {
    query <- paste0(query, sprintf("
      LEFT JOIN (
        SELECT
          %s,
          *
        FROM adt
        WHERE in_dttm IS NOT NULL
      ) adt_combo USING (combo_id)", combo_id_expression("in_dttm", site_timezone)))
  }

  for (table_name in names(pivoted_table_lookup)) {
    query <- paste0(
      query, " LEFT JOIN ", quote_identifier(pivoted_table_lookup[[table_name]]), " USING (combo_id)"
    )
  }

  for (table_name in tables_to_load) {
    if (!table_name %in% pivot_table_names && table_name %in% names(raw_table_lookup)) {
      timestamp_column <- get_timestamp_column(table_name)
      if (is.null(timestamp_column)) {
        next
      }
      raw_column_names <- duckdb_column_names(connection, raw_table_lookup[[table_name]])
      if (!timestamp_column %in% raw_column_names) {
        next
      }
      table_column_names <- setdiff(raw_column_names, c("hospitalization_id", timestamp_column))
      if (length(table_column_names) == 0) {
        next
      }
      query <- paste0(query, sprintf(
        "
        LEFT JOIN (
          SELECT
            %s,
            %s
          FROM %s
          WHERE %s IS NOT NULL
        ) %s_combo USING (combo_id)",
        combo_id_expression(quote_identifier(timestamp_column), site_timezone),
        paste(quote_identifier(table_column_names), collapse = ", "),
        quote_identifier(raw_table_lookup[[table_name]]),
        quote_identifier(timestamp_column),
        table_name
      ))
    }
  }

  result_data <- DBI::dbGetQuery(connection, query)

  if (!is.null(cohort_df)) {
    # event_time comes back as a UTC-labelled POSIXct carrying the true instant, so
    # it compares correctly against the site-timezone cohort bounds instant-for-instant.
    result_data <- filter_to_cohort_window(result_data, cohort_df, "event_time")
  }

  result_data <- result_data[, !duplicated(names(result_data)), drop = FALSE]

  # clifpy's day_number ranks the site-local calendar date; event_time is the true
  # instant, so the date is taken in the site timezone rather than in UTC.
  result_data$date <- as.Date(result_data$event_time, tz = site_timezone)
  row_order <- order(result_data$hospitalization_id, result_data$event_time, method = "radix")
  result_data <- result_data[row_order, , drop = FALSE]
  rownames(result_data) <- NULL

  result_data$day_number <- as.integer(
    result_data |>
      dplyr::group_by(.data$hospitalization_id) |>
      dplyr::mutate(.day_number = dplyr::dense_rank(.data$date)) |>
      dplyr::ungroup() |>
      dplyr::pull(".day_number")
  )
  result_data$hosp_id_day_key <- paste0(
    as.character(result_data$hospitalization_id), "_day_", result_data$day_number
  )

  result_data <- add_missing_columns(result_data, category_filters, tables_to_load)

  result_data <- result_data[, setdiff(names(result_data), c("combo_id", "date")), drop = FALSE]
  result_data <- relabel_datetime_timezones(result_data, site_timezone)

  dplyr::as_tibble(result_data)
}

add_missing_columns <- function(data, category_filters, tables_loaded) {
  if (length(category_filters) == 0) {
    return(data)
  }

  configuration <- load_wide_tables_config()
  medication_table_names <- character(0)
  for (table_name in names(configuration$tables)) {
    if (isTRUE(configuration$tables[[table_name]]$supports_unit_conversion)) {
      medication_table_names <- c(medication_table_names, table_name)
    }
  }

  for (table_name in names(category_filters)) {
    categories <- category_filters[[table_name]]
    if (!table_name %in% tables_loaded || length(categories) == 0) {
      next
    }
    for (category in categories) {
      if (table_name %in% medication_table_names) {
        pattern_matches <- grep(paste0("^", category, "_"), names(data), value = TRUE)
        if (length(pattern_matches) == 0 && !category %in% names(data)) {
          data[[category]] <- NA_real_
        }
      } else if (!category %in% names(data)) {
        data[[category]] <- NA_real_
      }
    }
  }

  data
}

# ---------------------------------------------------------------------------
# Batched processing
# ---------------------------------------------------------------------------

process_in_batches <- function(connection,
                               clif_instance,
                               all_hospitalization_ids,
                               patient_data,
                               hospitalization_data,
                               adt_data,
                               tables_to_load,
                               category_filters,
                               pivot_table_names,
                               wide_table_names,
                               batch_size,
                               show_progress,
                               save_to_data_location,
                               output_filename,
                               output_format,
                               return_dataframe,
                               cohort_df = NULL,
                               site_timezone = "UTC") {
  batch_starts <- seq(1L, length(all_hospitalization_ids), by = batch_size)
  batch_results <- list()

  for (batch_index in seq_along(batch_starts)) {
    batch_start <- batch_starts[batch_index]
    batch_end <- min(batch_start + batch_size - 1L, length(all_hospitalization_ids))
    batch_hospitalization_ids <- all_hospitalization_ids[batch_start:batch_end]

    if (isTRUE(show_progress)) {
      cli::cli_alert_info("Processing batch {batch_index}/{length(batch_starts)}")
    }

    batch_result <- tryCatch(
      {
        batch_hospitalization_data <- hospitalization_data[
          hospitalization_data$hospitalization_id %in% batch_hospitalization_ids, ,
          drop = FALSE
        ]
        batch_adt_data <- adt_data[
          adt_data$hospitalization_id %in% batch_hospitalization_ids, ,
          drop = FALSE
        ]
        batch_cohort_df <- NULL
        if (!is.null(cohort_df)) {
          batch_cohort_df <- cohort_df[
            cohort_df$hospitalization_id %in% batch_hospitalization_ids, ,
            drop = FALSE
          ]
        }

        for (existing_table_name in duckdb_table_names(connection)) {
          if (!existing_table_name %in% c("base_cohort", "adt")) {
            try(
              DBI::dbExecute(
                connection, sprintf("DROP TABLE IF EXISTS %s", quote_identifier(existing_table_name))
              ),
              silent = TRUE
            )
          }
        }

        process_hospitalizations(
          connection, clif_instance, batch_hospitalization_ids, patient_data,
          batch_hospitalization_data, batch_adt_data, tables_to_load, category_filters,
          pivot_table_names, wide_table_names, show_progress = FALSE,
          cohort_df = batch_cohort_df, site_timezone = site_timezone
        )
      },
      error = function(condition) {
        cli::cli_alert_danger("Failed to process batch {batch_index}: {conditionMessage(condition)}")
        NULL
      }
    )

    if (!is.null(batch_result) && nrow(batch_result) > 0) {
      batch_results[[length(batch_results) + 1L]] <- batch_result
    }
  }

  if (length(batch_results) == 0) {
    cli::cli_alert_danger("No data processed successfully")
    return(NULL)
  }

  final_data <- dplyr::bind_rows(batch_results)

  if (isTRUE(save_to_data_location)) {
    save_wide_dataset(final_data, clif_instance$data_directory, output_filename, output_format)
  }

  if (isTRUE(return_dataframe)) final_data else NULL
}

#' Write a wide dataset to the data directory
#'
#' @param data Wide dataset to write.
#' @param data_directory Destination directory.
#' @param output_filename File name stem; defaults to `wide_dataset_YYYYMMDD_HHMMSS`.
#' @param output_format `"csv"` or `"parquet"`.
#' @return Path to the written file, invisibly.
#' @keywords internal
save_wide_dataset <- function(data, data_directory, output_filename, output_format) {
  if (is.null(output_filename)) {
    output_filename <- format(Sys.time(), "wide_dataset_%Y%m%d_%H%M%S")
  }
  output_path <- file.path(data_directory, paste0(output_filename, ".", output_format))
  if (identical(output_format, "csv")) {
    readr::write_csv(data, output_path)
  } else if (identical(output_format, "parquet")) {
    arrow::write_parquet(data, output_path)
  }
  invisible(output_path)
}

# ---------------------------------------------------------------------------
# convert_wide_to_hourly
# ---------------------------------------------------------------------------

#' Aggregate a wide dataset into fixed-width time windows
#'
#' Windows are event-based: window 0 starts at each group's first event and each
#' window spans `hourly_window` hours. Port of
#' `clifpy.utils.wide_dataset.convert_wide_to_hourly`.
#'
#' @param wide_df A wide dataset from [create_wide_dataset()].
#' @param aggregation_config Named list mapping an aggregation method to the columns
#'   it applies to. Supported methods: `max`, `mean`, `min`, `median`, `first`,
#'   `last`, `boolean`, `one_hot_encode`. Columns not listed are carried through with
#'   `first` and a `_c` suffix.
#' @param id_name Grouping column, e.g. `"hospitalization_id"` or `"encounter_block"`.
#' @param hourly_window Window width in hours, an integer between 1 and 72.
#' @param fill_gaps When `TRUE`, emit a row for every window between each group's
#'   first and last window, filling gaps with `NA`.
#' @param memory_limit DuckDB memory limit.
#' @param temp_directory Directory for DuckDB spill files.
#' @param batch_size Number of groups per batch. `NULL` auto-selects; `0` disables.
#' @param timezone DuckDB session timezone.
#'
#' @return A tibble with `window_number`, `window_start_dttm`, `window_end_dttm`,
#'   `patient_id`, `day_number` and one column per configured aggregate.
#' @export
#'
#' @examples
#' \dontrun{
#' hourly <- convert_wide_to_hourly(
#'   wide_dataset,
#'   aggregation_config = list(
#'     max = c("sbp"), mean = c("heart_rate"), boolean = c("norepinephrine")
#'   )
#' )
#' }
convert_wide_to_hourly <- function(wide_df,
                                   aggregation_config,
                                   id_name = "hospitalization_id",
                                   hourly_window = 1,
                                   fill_gaps = FALSE,
                                   memory_limit = "4GB",
                                   temp_directory = NULL,
                                   batch_size = NULL,
                                   timezone = "UTC") {
  if (!is.numeric(hourly_window) || length(hourly_window) != 1 ||
    is.na(hourly_window) || hourly_window != as.integer(hourly_window)) {
    cli::cli_abort("{.arg hourly_window} must be an integer, got: {.val {hourly_window}}")
  }
  hourly_window <- as.integer(hourly_window)
  if (hourly_window < 1L || hourly_window > 72L) {
    cli::cli_abort("{.arg hourly_window} must be between 1 and 72 hours, got: {.val {hourly_window}}")
  }
  if (!is.logical(fill_gaps) || length(fill_gaps) != 1 || is.na(fill_gaps)) {
    cli::cli_abort("{.arg fill_gaps} must be a boolean, got: {.val {fill_gaps}}")
  }

  wide_df <- strip_datetime_timezones(as.data.frame(wide_df, stringsAsFactors = FALSE))

  required_columns <- c("event_time", id_name, "day_number")
  for (column_name in required_columns) {
    if (!column_name %in% names(wide_df)) {
      cli::cli_abort("{.arg wide_df} must contain {.val {column_name}} column")
    }
  }

  if (is.null(batch_size)) {
    row_count <- nrow(wide_df)
    group_count <- length(unique(wide_df[[id_name]]))
    if (row_count > 1e6 || group_count > 10000) {
      batch_size <- min(5000L, group_count %/% 4L)
    } else {
      batch_size <- 0L
    }
  }

  connection <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(connection, sprintf("SET memory_limit = '%s'", memory_limit))
  DBI::dbExecute(connection, sprintf("SET temp_directory = '%s'", temp_directory %||% "/tmp/duckdb_temp"))
  DBI::dbExecute(connection, "SET preserve_insertion_order = false")
  DBI::dbExecute(connection, "SET threads = 4")
  DBI::dbExecute(connection, sprintf("SET timezone = '%s'", timezone))

  if (batch_size > 0) {
    process_hourly_in_batches(connection, wide_df, aggregation_config, id_name, batch_size, hourly_window, fill_gaps)
  } else {
    process_hourly_single_batch(connection, wide_df, aggregation_config, id_name, hourly_window, fill_gaps)
  }
}

process_hourly_single_batch <- function(connection,
                                        wide_df,
                                        aggregation_config,
                                        id_name = "hospitalization_id",
                                        hourly_window = 1L,
                                        fill_gaps = FALSE) {
  duckdb::duckdb_register(connection, "wide_data", wide_df)
  on.exit(try(duckdb::duckdb_unregister(connection, "wide_data"), silent = TRUE), add = TRUE)

  DBI::dbExecute(connection, sprintf(
    "CREATE OR REPLACE TABLE windowed_data AS
     WITH first_events AS (
       SELECT
         %1$s,
         MIN(event_time) AS first_event_time
       FROM wide_data
       GROUP BY %1$s
     )
     SELECT
       wd.*,
       fe.first_event_time,
       CAST(FLOOR((EPOCH(wd.event_time) - EPOCH(fe.first_event_time)) / (%2$d * 3600)) AS INTEGER) AS window_number
     FROM wide_data wd
     JOIN first_events fe ON wd.%1$s = fe.%1$s",
    quote_identifier(id_name), hourly_window
  ))

  aggregation_queries <- build_aggregation_query_duckdb(
    connection, aggregation_config, names(wide_df), id_name, hourly_window
  )

  result_data <- DBI::dbGetQuery(connection, aggregation_queries$base)

  aggregation_order <- c("max", "min", "mean", "median", "first", "last", "boolean", "one_hot_encode")
  for (aggregation_method in aggregation_order) {
    if (!aggregation_method %in% names(aggregation_queries)) {
      next
    }
    tryCatch(
      {
        aggregation_result <- DBI::dbGetQuery(connection, aggregation_queries[[aggregation_method]])
        result_data <- dplyr::left_join(
          result_data, aggregation_result,
          by = c(id_name, "window_number")
        )
      },
      error = function(condition) {
        cli::cli_alert_danger("{aggregation_method} failed: {conditionMessage(condition)}")
      }
    )
  }

  if (isTRUE(fill_gaps)) {
    result_data <- fill_window_gaps(connection, result_data, id_name, hourly_window)
  }

  row_order <- order(result_data[[id_name]], result_data$window_number, method = "radix")
  result_data <- result_data[row_order, , drop = FALSE]
  rownames(result_data) <- NULL

  DBI::dbExecute(connection, "DROP TABLE IF EXISTS windowed_data")

  dplyr::as_tibble(result_data)
}

fill_window_gaps <- function(connection, aggregated_data, id_name, hourly_window) {
  duckdb::duckdb_register(connection, "aggregated_data", strip_datetime_timezones(aggregated_data))
  on.exit(try(duckdb::duckdb_unregister(connection, "aggregated_data"), silent = TRUE), add = TRUE)

  quoted_id <- quote_identifier(id_name)
  complete_data <- DBI::dbGetQuery(connection, sprintf(
    "WITH window_ranges AS (
       SELECT
         %1$s,
         MIN(window_number) AS min_window,
         MAX(window_number) AS max_window
       FROM aggregated_data
       GROUP BY %1$s
     ),
     first_event_times AS (
       SELECT
         %1$s,
         window_start_dttm
       FROM aggregated_data
       WHERE window_number = 0
     ),
     all_windows AS (
       SELECT
         wr.%1$s,
         unnest(generate_series(wr.min_window, wr.max_window, 1)) AS window_number
       FROM window_ranges wr
     ),
     window_timestamps AS (
       SELECT
         aw.%1$s,
         aw.window_number,
         fe.window_start_dttm + (aw.window_number * %2$d) * INTERVAL '1' HOUR AS window_start_dttm,
         fe.window_start_dttm + ((aw.window_number + 1) * %2$d) * INTERVAL '1' HOUR AS window_end_dttm
       FROM all_windows aw
       LEFT JOIN first_event_times fe ON aw.%1$s = fe.%1$s
     )
     SELECT
       wt.%1$s,
       wt.window_number,
       wt.window_start_dttm,
       wt.window_end_dttm,
       ad.* EXCLUDE (%1$s, window_number, window_start_dttm, window_end_dttm)
     FROM window_timestamps wt
     LEFT JOIN aggregated_data ad
       ON wt.%1$s = ad.%1$s
       AND wt.window_number = ad.window_number
     ORDER BY wt.%1$s, wt.window_number",
    quoted_id, hourly_window
  ))

  complete_data
}

process_hourly_in_batches <- function(connection,
                                      wide_df,
                                      aggregation_config,
                                      id_name,
                                      batch_size,
                                      hourly_window = 1L,
                                      fill_gaps = FALSE) {
  unique_group_ids <- unique(wide_df[[id_name]])
  batch_starts <- seq(1L, length(unique_group_ids), by = batch_size)
  batch_results <- list()

  for (batch_index in seq_along(batch_starts)) {
    batch_start <- batch_starts[batch_index]
    batch_end <- min(batch_start + batch_size - 1L, length(unique_group_ids))
    batch_group_ids <- unique_group_ids[batch_start:batch_end]

    batch_result <- tryCatch(
      {
        batch_data <- wide_df[wide_df[[id_name]] %in% batch_group_ids, , drop = FALSE]
        process_hourly_single_batch(
          connection, batch_data, aggregation_config, id_name, hourly_window, fill_gaps
        )
      },
      error = function(condition) {
        cli::cli_alert_danger("Error processing batch {batch_index}: {conditionMessage(condition)}")
        NULL
      }
    )

    if (!is.null(batch_result) && nrow(batch_result) > 0) {
      batch_results[[length(batch_results) + 1L]] <- batch_result
    }
  }

  if (length(batch_results) == 0) {
    cli::cli_abort("No batches processed successfully")
  }

  final_data <- dplyr::bind_rows(batch_results)
  row_order <- order(final_data[[id_name]], final_data$window_number, method = "radix")
  dplyr::as_tibble(final_data[row_order, , drop = FALSE])
}

build_aggregation_query_duckdb <- function(connection,
                                           aggregation_config,
                                           all_columns,
                                           id_name = "hospitalization_id",
                                           hourly_window = 1L) {
  group_columns <- c(id_name, "window_number", "window_start_dttm", "window_end_dttm")

  all_aggregated_columns <- unlist(aggregation_config, use.names = FALSE)

  non_aggregated_columns <- all_columns[
    !all_columns %in% all_aggregated_columns &
      !all_columns %in% group_columns &
      !all_columns %in% c("patient_id", "day_number", "first_event_time", "event_time", "window_number")
  ]

  if (length(non_aggregated_columns) > 0) {
    cli::cli_alert_info(
      "Columns not in aggregation_config, defaulting to 'first' with '_c' postfix: {paste(utils::head(non_aggregated_columns, 5), collapse = ', ')}"
    )
    if (is.null(aggregation_config[["first"]])) {
      aggregation_config[["first"]] <- character(0)
    }
    aggregation_config[["first"]] <- c(aggregation_config[["first"]], non_aggregated_columns)
  }

  queries <- list()
  quoted_id <- quote_identifier(id_name)

  queries$base <- sprintf(
    "WITH window_aggregates AS (
       SELECT
         %1$s,
         window_number,
         MIN(first_event_time) AS first_event_time
       FROM windowed_data
       GROUP BY %1$s, window_number
     )
     SELECT
       wa.%1$s,
       wa.window_number,
       wa.first_event_time + (wa.window_number * %2$d) * INTERVAL '1' HOUR AS window_start_dttm,
       wa.first_event_time + ((wa.window_number + 1) * %2$d) * INTERVAL '1' HOUR AS window_end_dttm,
       FIRST(wd.patient_id ORDER BY wd.event_time) AS patient_id,
       FIRST(wd.day_number ORDER BY wd.event_time) AS day_number
     FROM windowed_data wd
     JOIN window_aggregates wa
       ON wd.%1$s = wa.%1$s
       AND wd.window_number = wa.window_number
     GROUP BY wa.%1$s, wa.window_number, wa.first_event_time",
    quoted_id, hourly_window
  )

  for (aggregation_method in names(aggregation_config)) {
    if (identical(aggregation_method, "one_hot_encode")) {
      next
    }
    valid_columns <- intersect(aggregation_config[[aggregation_method]], all_columns)
    if (length(valid_columns) == 0) {
      next
    }

    select_parts <- c(quoted_id, "window_number")
    for (column_name in valid_columns) {
      quoted_column <- quote_identifier(column_name)
      select_parts <- c(select_parts, switch(aggregation_method,
        max = sprintf('MAX(%s) AS "%s_max"', quoted_column, column_name),
        min = sprintf('MIN(%s) AS "%s_min"', quoted_column, column_name),
        mean = sprintf('AVG(%s) AS "%s_mean"', quoted_column, column_name),
        median = sprintf('MEDIAN(%s) AS "%s_median"', quoted_column, column_name),
        first = if (column_name %in% non_aggregated_columns) {
          sprintf('FIRST(%s ORDER BY event_time) AS "%s_c"', quoted_column, column_name)
        } else {
          sprintf('FIRST(%s ORDER BY event_time) AS "%s_first"', quoted_column, column_name)
        },
        last = sprintf('LAST(%s ORDER BY event_time) AS "%s_last"', quoted_column, column_name),
        boolean = sprintf('CASE WHEN COUNT(%s) > 0 THEN 1 ELSE 0 END AS "%s_boolean"', quoted_column, column_name),
        NULL
      ))
    }

    if (length(select_parts) <= 2) {
      next
    }

    queries[[aggregation_method]] <- sprintf(
      "SELECT
         %s
       FROM windowed_data
       GROUP BY %s, window_number",
      paste(select_parts, collapse = ", "), quoted_id
    )
  }

  if ("one_hot_encode" %in% names(aggregation_config)) {
    one_hot_query <- build_one_hot_encoding_query_duckdb(
      connection, aggregation_config[["one_hot_encode"]], all_columns, id_name
    )
    if (!is.null(one_hot_query)) {
      queries$one_hot_encode <- one_hot_query
    }
  }

  queries
}

build_one_hot_encoding_query_duckdb <- function(connection,
                                                one_hot_columns,
                                                all_columns,
                                                id_name = "hospitalization_id") {
  valid_columns <- intersect(one_hot_columns, all_columns)
  if (length(valid_columns) == 0) {
    return(NULL)
  }

  quoted_id <- quote_identifier(id_name)
  select_parts <- c(quoted_id, "window_number")

  for (column_name in valid_columns) {
    quoted_column <- quote_identifier(column_name)
    unique_values_query <- sprintf(
      "SELECT DISTINCT %1$s
       FROM windowed_data
       WHERE %1$s IS NOT NULL
       ORDER BY %1$s
       LIMIT 100",
      quoted_column
    )

    unique_values <- tryCatch(
      DBI::dbGetQuery(connection, unique_values_query)[[1]],
      error = function(condition) {
        cli::cli_alert_warning(
          "Could not create one-hot encoding for {.val {column_name}}: {conditionMessage(condition)}"
        )
        NULL
      }
    )
    if (is.null(unique_values)) {
      next
    }
    if (length(unique_values) > 50) {
      cli::cli_alert_warning(
        "{.val {column_name}} has {length(unique_values)} unique values. One-hot encoding may create many columns"
      )
    }

    for (unique_value in unique_values) {
      cleaned_value <- gsub("[^a-zA-Z0-9_]", "_", as.character(unique_value))
      one_hot_column_name <- paste0(column_name, "_", cleaned_value)
      literal <- if (is.character(unique_value)) {
        paste0("'", gsub("'", "''", unique_value, fixed = TRUE), "'")
      } else {
        format(unique_value, scientific = FALSE)
      }
      select_parts <- c(select_parts, sprintf(
        'MAX(CASE WHEN %s = %s THEN 1 ELSE 0 END) AS "%s"',
        quoted_column, literal, one_hot_column_name
      ))
    }
  }

  if (length(select_parts) <= 2) {
    return(NULL)
  }

  sprintf(
    "SELECT
       %s
     FROM windowed_data
     GROUP BY %s, window_number",
    paste(select_parts, collapse = ", "), quoted_id
  )
}
