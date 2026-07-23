#' Extremal-value lookups in long CLIF tables
#'
#' Port of `clifpy/utils/query.py`. Answers questions of the form "for each
#' hospitalization window, what was the maximum SpO2 and the latest weight?"
#' against a long (narrow) CLIF table.
#'
#' The ranking and windowing are done in DuckDB with the same SQL clifpy uses, so
#' tie-breaking and null handling match across the two implementations.
#'
#' @name clif-query
NULL

#' Long-table category column by table name
#' @keywords internal
QUERY_TABLE_CATEGORY_COLUMNS <- c(
  vitals = "vital_category",
  labs = "lab_category",
  medication_admin_continuous = "med_category",
  patient_assessments = "assessment_category"
)

#' Long-table value column by table name
#' @keywords internal
QUERY_TABLE_VALUE_COLUMNS <- c(
  vitals = "vital_value",
  labs = "lab_value_numeric",
  medication_admin_continuous = "med_dose",
  patient_assessments = "numerical_value"
)

#' Long-table datetime column by table name
#' @keywords internal
QUERY_TABLE_DTTM_COLUMNS <- c(
  vitals = "recorded_dttm",
  labs = "lab_collect_dttm",
  medication_admin_continuous = "admin_dttm",
  patient_assessments = "recorded_dttm"
)

#' Table names supported by [lookup_extremal_values_in_long_table()]
#' @export
QUERY_SUPPORTED_TABLE_NAMES <- names(QUERY_TABLE_CATEGORY_COLUMNS)

QUERY_VALID_EXTREMAL_VALUES <- c("min", "max", "latest")

#' Build the SQL predicate selecting one category's requested extremal rows
#'
#' For example category `spo2` with `c("max")` becomes
#' `(vital_category = 'spo2' AND rn_max = 1)`.
#'
#' @param category_key Category value, e.g. `"spo2"`.
#' @param extremal_values Character vector drawn from `"min"`, `"max"`, `"latest"`.
#' @param category_column Name of the table's category column.
#' @return A SQL boolean expression.
#' @keywords internal
extremal_value_sql_clause <- function(category_key, extremal_values, category_column) {
  invalid_extremal_values <- setdiff(extremal_values, QUERY_VALID_EXTREMAL_VALUES)
  if (length(invalid_extremal_values) > 0) {
    cli::cli_abort(
      "Invalid extremal_values: {.val {invalid_extremal_values}}. Must be one of {.val {QUERY_VALID_EXTREMAL_VALUES}}."
    )
  }
  paste(sprintf(
    "(%s = %s AND rn_%s = 1)",
    category_column, sql_quote_value(category_key), extremal_values
  ), collapse = " OR ")
}

#' Build the SQL predicate for a whole query specification
#'
#' @param query_dict Named list mapping category values to extremal-value vectors.
#' @param category_column Name of the table's category column.
#' @return A SQL boolean expression.
#' @keywords internal
query_dict_sql_clause <- function(query_dict, category_column) {
  paste(vapply(
    names(query_dict),
    function(category_key) {
      extremal_value_sql_clause(category_key, query_dict[[category_key]], category_column)
    },
    character(1)
  ), collapse = " OR ")
}

#' Look up extremal values in a long CLIF table
#'
#' For each `(hospitalization_id, start_dttm, end_dttm)` window in `ids_w_dttm`,
#' finds the minimum, maximum and/or most recent value of each requested
#' category, and returns them pivoted into one column per
#' `category_extremal` combination.
#'
#' Only measurements recorded inside the window (inclusive at both ends) with a
#' non-missing value are considered. Windows with no matching measurement do not
#' appear in the result, matching clifpy.
#'
#' @param ids_w_dttm A data frame with columns `hospitalization_id`,
#'   `start_dttm` and `end_dttm`.
#' @param query_dict Named list mapping category values to the extremal values
#'   wanted, e.g. `list(weight_kg = c("latest", "max"), spo2 = c("max", "min"))`.
#'   Valid extremal values are `"min"`, `"max"` and `"latest"`.
#' @param table_name One of `"vitals"`, `"labs"`,
#'   `"medication_admin_continuous"` or `"patient_assessments"`.
#' @param data Optional pre-loaded long table. When `NULL` the table is loaded
#'   from the resolved configuration, filtered to the hospitalizations in
#'   `ids_w_dttm`.
#' @param config_path Optional path to a configuration file, used only when
#'   `data` is `NULL`.
#' @param data_directory,filetype,timezone Optional direct configuration
#'   parameters, used only when `data` is `NULL`. See [get_config_or_params()].
#'
#' @return A tibble keyed by `hospitalization_id`, `start_dttm` and `end_dttm`
#'   with one additional column per requested `category_extremal` combination,
#'   e.g. `spo2_max`, `weight_kg_latest`.
#' @export
#'
#' @examples
#' \dontrun{
#' windows <- data.frame(
#'   hospitalization_id = "H001",
#'   start_dttm = as.POSIXct("2023-01-01", tz = "UTC"),
#'   end_dttm = as.POSIXct("2023-01-02", tz = "UTC")
#' )
#' lookup_extremal_values_in_long_table(
#'   windows,
#'   list(spo2 = c("min", "max"), weight_kg = "latest"),
#'   "vitals",
#'   data = vitals_table$df
#' )
#' }
lookup_extremal_values_in_long_table <- function(ids_w_dttm,
                                                 query_dict,
                                                 table_name,
                                                 data = NULL,
                                                 config_path = NULL,
                                                 data_directory = NULL,
                                                 filetype = NULL,
                                                 timezone = NULL) {
  required_window_columns <- c("hospitalization_id", "start_dttm", "end_dttm")
  missing_columns <- setdiff(required_window_columns, names(ids_w_dttm))
  if (length(missing_columns) > 0) {
    cli::cli_abort("{.arg ids_w_dttm} must have columns: {.field {missing_columns}}")
  }
  if (!table_name %in% QUERY_SUPPORTED_TABLE_NAMES) {
    cli::cli_abort(
      "{.arg table_name} must be one of the long tables: {.val {QUERY_SUPPORTED_TABLE_NAMES}}"
    )
  }
  if (length(query_dict) == 0 || is.null(names(query_dict))) {
    cli::cli_abort("{.arg query_dict} must be a named list of category -> extremal values.")
  }

  category_column <- QUERY_TABLE_CATEGORY_COLUMNS[[table_name]]
  value_column <- QUERY_TABLE_VALUE_COLUMNS[[table_name]]
  dttm_column <- QUERY_TABLE_DTTM_COLUMNS[[table_name]]
  requested_categories <- names(query_dict)

  window_frame <- dplyr::as_tibble(ids_w_dttm)

  long_table_data <- data
  if (is.null(long_table_data)) {
    resolved_config <- get_config_or_params(
      config_path = config_path,
      data_directory = data_directory,
      filetype = filetype,
      timezone = timezone
    )
    long_table_data <- load_data(
      table_name = table_name,
      table_path = resolved_config$data_directory,
      table_format_type = resolved_config$filetype,
      filters = list(hospitalization_id = unique(window_frame$hospitalization_id)),
      site_tz = resolved_config$timezone
    )
  }
  long_table_data <- dplyr::as_tibble(long_table_data)

  connection <- duckdb_connect()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(connection, "ids_w_dttm", window_frame)
  duckdb::duckdb_register(connection, "long_table", long_table_data)

  categories_sql <- paste(vapply(requested_categories, sql_quote_value, character(1)), collapse = ", ")

  joined_query <- sprintf(
    "SELECT l.*
        , r.%1$s
        , r.%2$s
        , r.%3$s
        , ROW_NUMBER() OVER (
            PARTITION BY l.hospitalization_id, l.start_dttm, l.end_dttm, r.%3$s
            ORDER BY r.%2$s DESC
            ) as rn_latest
        , ROW_NUMBER() OVER (
            PARTITION BY l.hospitalization_id, l.start_dttm, l.end_dttm, r.%3$s
            ORDER BY r.%1$s DESC
            ) as rn_max
        , ROW_NUMBER() OVER (
            PARTITION BY l.hospitalization_id, l.start_dttm, l.end_dttm, r.%3$s
            ORDER BY r.%1$s ASC
            ) as rn_min
    FROM ids_w_dttm l
    LEFT JOIN long_table r
    ON l.hospitalization_id = r.hospitalization_id
        AND l.start_dttm <= r.%2$s
        AND r.%2$s <= l.end_dttm
        AND r.%3$s IN (%4$s)
        AND r.%1$s IS NOT NULL
    ORDER BY l.hospitalization_id, l.start_dttm, r.%3$s, rn_latest, rn_max, rn_min",
    value_column, dttm_column, category_column, categories_sql
  )

  qualified_query <- sprintf(
    "SELECT * FROM (%s) WHERE %s",
    joined_query,
    query_dict_sql_clause(query_dict, category_column)
  )
  qualified_rows <- dplyr::as_tibble(DBI::dbGetQuery(connection, qualified_query))

  if (nrow(qualified_rows) == 0) {
    return(window_frame[0, required_window_columns])
  }

  # Long form: one row per (window, category, extremal) that actually ranked first.
  melted <- qualified_rows |>
    dplyr::select(dplyr::all_of(c(
      required_window_columns, category_column, value_column,
      "rn_max", "rn_latest", "rn_min"
    ))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(c("rn_max", "rn_latest", "rn_min")),
      names_to = "extreme",
      values_to = "rn"
    ) |>
    dplyr::filter(.data$rn == 1) |>
    dplyr::mutate(
      column_name = paste0(.data[[category_column]], "_", sub("^rn_", "", .data$extreme))
    )

  # pandas' pivot_table averages collisions; after the rn == 1 filter there is at
  # most one value per cell, so mean() is a no-op that preserves that behaviour.
  melted |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(required_window_columns)),
      .data$column_name
    ) |>
    dplyr::summarise(
      pivot_value = mean(.data[[value_column]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = "column_name",
      values_from = "pivot_value",
      names_sort = TRUE
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(required_window_columns)))
}
