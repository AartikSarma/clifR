#' Data loading utilities
#'
#' DuckDB-backed loaders mirroring `clifpy/utils/io.py`. Using the same engine and
#' the same query shape as the Python implementation is what makes cross-language
#' results comparable: column selection, filtering and row sampling all happen
#' inside DuckDB rather than in R-specific readers, so the rows that reach R are
#' the same rows clifpy sees.
#'
#' @name clif-io
NULL

#' Build the path to a CLIF table's data file
#'
#' @description
#' The single source of truth for where a CLIF table's file lives. CLIF data
#' directories use the `clif_` prefix convention, so the file for table
#' `patient` in a parquet directory is `clif_patient.parquet` — there is no
#' fallback to a bare `patient.parquet` and no case-insensitive search. One
#' rule, applied everywhere, so a table cannot be found by one caller and
#' missed by another.
#'
#' This only builds the path; it does not check that the file exists. Callers
#' test with [file.exists()] and raise their own error, which lets a missing
#' optional table be skipped while a missing requested table aborts.
#'
#' @param data_directory Character. Path to directory containing data files.
#' @param table_name Character. Name of the CLIF table (e.g. "patient").
#' @param filetype Character. File type: "csv" or "parquet".
#'
#' @return Character path to the table's expected file.
#'
#' @examples
#' clif_table_file_path("data/clif", "patient", "parquet")
#'
#' @export
clif_table_file_path <- function(data_directory, table_name, filetype) {
  file.path(data_directory, paste0("clif_", table_name, ".", filetype))
}

#' Open a configured DuckDB connection
#'
#' Applies the same session settings clifpy uses so reads behave identically. Setting
#' the session timezone needs DuckDB's ICU extension; most builds bundle it, but when
#' they do not this fails loudly rather than continuing in whatever timezone the
#' machine happens to use, which would silently diverge from clifpy.
#'
#' @return A DBI connection to an in-memory DuckDB database.
#' @keywords internal
duckdb_connect <- function() {
  connection <- DBI::dbConnect(duckdb::duckdb())

  timezone_error <- tryCatch(
    {
      DBI::dbExecute(connection, "SET timezone = 'UTC';")
      NULL
    },
    error = function(condition) condition
  )

  if (!is.null(timezone_error)) {
    # ICU ships with most DuckDB builds; when it is merely unloaded, loading it and
    # retrying is enough.
    timezone_error <- tryCatch(
      {
        DBI::dbExecute(connection, "INSTALL icu; LOAD icu;")
        DBI::dbExecute(connection, "SET timezone = 'UTC';")
        NULL
      },
      error = function(condition) condition
    )
  }

  if (!is.null(timezone_error)) {
    DBI::dbDisconnect(connection, shutdown = TRUE)
    cli::cli_abort(c(
      "Could not set the DuckDB session timezone to UTC.",
      "x" = conditionMessage(timezone_error),
      "i" = "This needs DuckDB's {.pkg icu} extension. Try reinstalling the {.pkg duckdb} R package,
             or set {.envvar DUCKDB_EXTENSION_DIRECTORY} to a directory holding the extension.",
      "i" = "Continuing without it would read timestamps in the machine's local timezone
             and silently disagree with clifpy."
    ))
  }

  connection
}

#' Quote a value for inline SQL
#'
#' @param value A scalar to embed in a SQL string literal.
#' @return The value wrapped in single quotes with internal quotes escaped.
#' @keywords internal
sql_quote_value <- function(value) {
  paste0("'", gsub("'", "''", as.character(value), fixed = TRUE), "'")
}

#' Cast identifier columns to character
#'
#' Every column whose name ends in `_id` is cast to character, matching clifpy's
#' `_cast_id_cols_to_string`. Float-typed IDs such as `123456.0` are formatted
#' without the fractional part so they render as `"123456"`, which is what the
#' Python side produces by casting through a nullable integer.
#'
#' @param data A data frame.
#' @return The data frame with ID columns cast to character.
#' @keywords internal
cast_id_cols_to_string <- function(data) {
  id_columns <- grep("_id$", names(data), value = TRUE)
  for (column_name in id_columns) {
    column_values <- data[[column_name]]
    if (is.numeric(column_values) && !is.integer(column_values)) {
      is_whole_number <- is.na(column_values) | column_values == trunc(column_values)
      if (all(is_whole_number)) {
        data[[column_name]] <- ifelse(
          is.na(column_values),
          NA_character_,
          sprintf("%.0f", column_values)
        )
        next
      }
    }
    data[[column_name]] <- as.character(column_values)
  }
  data
}

#' Convert datetime columns to the site timezone
#'
#' Every column whose name contains `dttm` is converted. Mirrors clifpy's
#' `convert_datetime_columns_to_site_tz`. Because R stores the timezone as an
#' attribute of the whole vector rather than per element, converting is a
#' relabelling of the display zone; the underlying instants are unchanged.
#'
#' @param data A data frame.
#' @param site_timezone Olson timezone name, e.g. `"America/New_York"`.
#' @param verbose Whether to emit a conversion summary.
#' @return The data frame with datetime columns in `site_timezone`.
#' @export
convert_datetime_columns_to_site_tz <- function(data, site_timezone, verbose = FALSE) {
  datetime_columns <- grep("dttm", names(data), value = TRUE)
  if (length(datetime_columns) == 0) {
    return(data)
  }

  converted_columns <- character(0)
  already_correct_columns <- character(0)
  problem_columns <- character(0)

  for (column_name in datetime_columns) {
    column_values <- data[[column_name]]
    if (inherits(column_values, "POSIXct")) {
      current_timezone <- attr(column_values, "tzone")
      if (!is.null(current_timezone) && identical(current_timezone, site_timezone)) {
        already_correct_columns <- c(already_correct_columns, column_name)
      } else {
        attr(column_values, "tzone") <- site_timezone
        data[[column_name]] <- column_values
        converted_columns <- c(converted_columns, column_name)
      }
    } else {
      problem_columns <- c(problem_columns, column_name)
    }
  }

  if (verbose && length(c(converted_columns, problem_columns)) > 0) {
    summary_parts <- character(0)
    if (length(converted_columns) > 0) {
      summary_parts <- c(summary_parts, sprintf("%d converted to %s", length(converted_columns), site_timezone))
    }
    if (length(already_correct_columns) > 0) {
      summary_parts <- c(summary_parts, sprintf("%d already correct", length(already_correct_columns)))
    }
    if (length(problem_columns) > 0) {
      summary_parts <- c(summary_parts, sprintf("%d problematic", length(problem_columns)))
    }
    cli::cli_alert_info("Timezone processing complete: {paste(summary_parts, collapse = ', ')}")
  }

  data
}

#' Load a CLIF table from a data directory
#'
#' Reads `clif_<table_name>.<filetype>` from `table_path` using DuckDB, applying
#' optional column selection, equality filters and row sampling before the data
#' reaches R. Port of `clifpy.utils.io.load_data`.
#'
#' @param table_name snake_case table name, e.g. `"labs"`.
#' @param table_path Directory containing the data file.
#' @param table_format_type Either `"csv"` or `"parquet"`.
#' @param sample_size Optional maximum number of rows to read.
#' @param columns Optional character vector of columns to select.
#' @param filters Optional named list of equality filters. A length-1 value becomes
#'   `column = value`; a longer vector becomes `column IN (...)`.
#' @param site_tz Optional Olson timezone to convert `dttm` columns into.
#' @param verbose Whether to emit loading messages.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' labs <- load_data("labs", "data/clif", "parquet", site_tz = "US/Central")
#' }
load_data <- function(table_name,
                      table_path,
                      table_format_type,
                      sample_size = NULL,
                      columns = NULL,
                      filters = NULL,
                      site_tz = NULL,
                      verbose = FALSE) {
  if (!table_format_type %in% c("csv", "parquet")) {
    cli::cli_abort("Unsupported filetype. Only {.val csv} and {.val parquet} are supported.")
  }

  if (!dir.exists(table_path)) {
    cli::cli_abort("The data directory {.file {table_path}} does not exist.")
  }

  file_path <- clif_table_file_path(table_path, table_name, table_format_type)
  if (!file.exists(file_path)) {
    cli::cli_abort(c(
      "No file found for table {.val {table_name}} in {.file {table_path}}.",
      "i" = "Expected {.file {basename(file_path)}}; CLIF data files carry the {.code clif_} prefix."
    ))
  }

  connection <- duckdb_connect()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  reader <- if (table_format_type == "csv") "read_csv_auto" else "read_parquet"
  select_clause <- if (is.null(columns)) "*" else paste(sprintf('"%s"', columns), collapse = ", ")

  where_clauses <- character(0)
  if (!is.null(filters)) {
    for (column_name in names(filters)) {
      filter_values <- filters[[column_name]]
      if (length(filter_values) > 1) {
        quoted_values <- paste(vapply(filter_values, sql_quote_value, character(1)), collapse = ", ")
        where_clauses <- c(where_clauses, sprintf('"%s" IN (%s)', column_name, quoted_values))
      } else {
        where_clauses <- c(where_clauses, sprintf('"%s" = %s', column_name, sql_quote_value(filter_values)))
      }
    }
  }
  where_sql <- if (length(where_clauses) > 0) paste("WHERE", paste(where_clauses, collapse = " AND ")) else ""
  limit_sql <- if (!is.null(sample_size)) sprintf("LIMIT %d", as.integer(sample_size)) else ""

  query <- sprintf(
    "SELECT %s FROM %s('%s') %s %s",
    select_clause, reader, file_path, where_sql, limit_sql
  )

  if (verbose) {
    cli::cli_alert_info("Loading {.file {basename(file_path)}}")
  }

  loaded_data <- DBI::dbGetQuery(connection, query)
  loaded_data <- cast_id_cols_to_string(loaded_data)

  if (!is.null(site_tz)) {
    loaded_data <- convert_datetime_columns_to_site_tz(loaded_data, site_tz, verbose)
  }

  if (verbose) {
    cli::cli_alert_success(
      "Loaded {.val {nrow(loaded_data)}} rows, {.val {ncol(loaded_data)}} columns from {.file {basename(file_path)}}"
    )
  }

  dplyr::as_tibble(loaded_data)
}

#' Load a CLIF data file by path
#'
#' Convenience wrapper for reading a single file when the caller already knows the
#' full path rather than a data directory and table name.
#'
#' @param file_path Character. Path to data file.
#' @param filetype Character. File type: "csv" or "parquet".
#' @param timezone Character. Timezone for datetime columns (default: "UTC").
#' @param schema Unused; retained so existing callers keep working.
#'
#' @return tibble containing loaded data.
#' @export
load_clif_data <- function(file_path, filetype = c("csv", "parquet"),
                           timezone = "UTC", schema = NULL) {
  filetype <- match.arg(filetype)

  if (!file.exists(file_path)) {
    cli::cli_abort("Data file not found: {.file {file_path}}")
  }

  connection <- duckdb_connect()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  reader <- if (filetype == "csv") "read_csv_auto" else "read_parquet"
  loaded_data <- DBI::dbGetQuery(
    connection,
    sprintf("SELECT * FROM %s('%s')", reader, file_path)
  )
  loaded_data <- cast_id_cols_to_string(loaded_data)
  loaded_data <- convert_datetime_columns_to_site_tz(loaded_data, timezone, verbose = FALSE)

  dplyr::as_tibble(loaded_data)
}

#' Load a CLIF table (Polars-compatible entry point)
#'
#' R port of clifpy's `load_data_polars`. clifpy ships a Polars-backed loader
#' alongside its pandas one; clifR has a single DuckDB-backed engine, so this
#' delegates to [load_data()] and returns a tibble. The `lazy` argument is accepted
#' for signature compatibility and is a no-op — clifR always returns a materialized
#' tibble.
#'
#' @inheritParams load_data
#' @param lazy Accepted for clifpy compatibility; ignored in R.
#' @return A tibble.
#' @export
load_data_polars <- function(table_name,
                             table_path,
                             table_format_type,
                             sample_size = NULL,
                             columns = NULL,
                             filters = NULL,
                             site_tz = NULL,
                             lazy = TRUE,
                             verbose = FALSE) {
  load_data(
    table_name = table_name,
    table_path = table_path,
    table_format_type = table_format_type,
    sample_size = sample_size,
    columns = columns,
    filters = filters,
    site_tz = site_tz,
    verbose = verbose
  )
}

#' Load a CLIF table with common filters (Polars-compatible entry point)
#'
#' R port of clifpy's `load_clif_table_polars`, a convenience loader that filters by
#' `hospitalization_ids` at read time. Delegates to [load_data()].
#'
#' @param data_directory Directory containing the CLIF data files.
#' @param table_name snake_case CLIF table name.
#' @param filetype Either `"parquet"` or `"csv"`.
#' @param hospitalization_ids Optional character vector to filter on.
#' @param columns Optional character vector of columns to read.
#' @param site_tz Optional Olson timezone to convert `dttm` columns into.
#' @param lazy Accepted for clifpy compatibility; ignored in R.
#' @return A tibble.
#' @export
load_clif_table_polars <- function(data_directory,
                                   table_name,
                                   filetype = "parquet",
                                   hospitalization_ids = NULL,
                                   columns = NULL,
                                   site_tz = NULL,
                                   lazy = TRUE) {
  filters <- if (!is.null(hospitalization_ids)) {
    list(hospitalization_id = hospitalization_ids)
  } else {
    NULL
  }
  load_data(
    table_name = table_name,
    table_path = data_directory,
    table_format_type = filetype,
    columns = columns,
    filters = filters,
    site_tz = site_tz
  )
}

#' Save CLIF data file
#'
#' @param data tibble or data.frame. Data to save.
#' @param file_path Character. Output file path.
#' @param filetype Character. File type: "csv" or "parquet".
#' @param overwrite Logical. Whether to overwrite existing file (default: FALSE).
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' save_clif_data(patient_data, "output/patient.csv", "csv")
#' }
save_clif_data <- function(data, file_path, filetype = c("csv", "parquet"),
                           overwrite = FALSE) {
  filetype <- match.arg(filetype)

  if (file.exists(file_path) && !overwrite) {
    cli::cli_abort(c(
      "File already exists: {.file {file_path}}",
      "i" = "Set {.code overwrite = TRUE} to overwrite"
    ))
  }

  destination_directory <- dirname(file_path)
  if (!dir.exists(destination_directory)) {
    dir.create(destination_directory, recursive = TRUE)
  }

  switch(filetype,
    csv = readr::write_csv(data, file_path),
    parquet = arrow::write_parquet(data, file_path)
  )

  invisible(NULL)
}

#' Load all CLIF tables from a directory
#'
#' @param data_directory Character. Path to directory containing CLIF data files.
#' @param table_names Character vector. Names of tables to load. If NULL, attempts
#'   to load all standard CLIF tables.
#' @param filetype Character. File type: "csv" or "parquet".
#' @param timezone Character. Timezone for datetime columns (default: "UTC").
#'
#' @return Named list of tibbles, one for each table found.
#' @export
load_all_tables <- function(data_directory, table_names = NULL,
                            filetype = c("csv", "parquet"),
                            timezone = "UTC") {
  filetype <- match.arg(filetype)

  if (!dir.exists(data_directory)) {
    cli::cli_abort("Data directory not found: {.file {data_directory}}")
  }

  if (is.null(table_names)) {
    table_names <- CLIF_TABLE_NAMES
  }

  loaded_tables <- list()
  for (table_name in table_names) {
    file_path <- clif_table_file_path(data_directory, table_name, filetype)
    if (file.exists(file_path)) {
      loaded_tables[[table_name]] <- load_data(
        table_name = table_name,
        table_path = data_directory,
        table_format_type = filetype,
        site_tz = timezone
      )
    }
  }

  if (length(loaded_tables) == 0) {
    cli::cli_abort("No CLIF tables found in {.file {data_directory}}")
  }

  loaded_tables
}

#' Export data to JSON
#'
#' @param data tibble or data.frame. Data to export.
#' @param file_path Character. Output JSON file path.
#' @param pretty Logical. Pretty print JSON (default: TRUE).
#'
#' @return Invisible NULL.
#' @export
export_to_json <- function(data, file_path, pretty = TRUE) {
  jsonlite::write_json(
    data,
    file_path,
    pretty = pretty,
    auto_unbox = TRUE,
    POSIXt = "ISO8601",
    na = "null"
  )
  invisible(NULL)
}

#' Get file size in human-readable format
#'
#' @param file_path Character. Path to file.
#' @return Character string with size and units.
#' @keywords internal
get_file_size <- function(file_path) {
  if (!file.exists(file_path)) {
    return("File not found")
  }

  size_in_bytes <- file.info(file_path)$size
  size_units <- c("B", "KB", "MB", "GB", "TB")
  unit_index <- 1

  while (size_in_bytes >= 1024 && unit_index < length(size_units)) {
    size_in_bytes <- size_in_bytes / 1024
    unit_index <- unit_index + 1
  }

  sprintf("%.2f %s", size_in_bytes, size_units[unit_index])
}
