#' Locate a CLIF table's data file
#'
#' @description
#' Searches a data directory for the file belonging to a given CLIF table.
#' CLIF data directories are not guaranteed to use a single naming
#' convention: some sites name files exactly after the table
#' (e.g. `patient.parquet`), while many CLIF consortium datasets use a
#' `clif_` prefix (e.g. `clif_patient.parquet`). This helper checks both
#' conventions, falling back to a case-insensitive match, so table lookup
#' does not depend on a single hardcoded filename pattern.
#'
#' @param data_directory Character. Path to directory containing data files.
#' @param table_name Character. Name of the CLIF table (e.g. "patient").
#' @param filetype Character. File type: "csv" or "parquet".
#'
#' @return Character path to the located file, or `NA_character_` if no
#'   matching file is found.
#'
#' @export
find_clif_table_file <- function(data_directory, table_name, filetype) {
  candidate_names <- c(
    paste0(table_name, ".", filetype),
    paste0("clif_", table_name, ".", filetype)
  )

  candidate_paths <- file.path(data_directory, candidate_names)
  existing_paths <- candidate_paths[file.exists(candidate_paths)]

  if (length(existing_paths) > 0) {
    return(existing_paths[[1]])
  }

  # Fall back to a case-insensitive scan of the directory contents, in case
  # the file exists but with different capitalization (e.g. "Patient.csv").
  if (dir.exists(data_directory)) {
    pattern <- paste0("^(clif_)?", table_name, "\\.", filetype, "$")
    matches <- list.files(
      data_directory,
      pattern = pattern,
      ignore.case = TRUE,
      full.names = TRUE
    )

    if (length(matches) > 0) {
      return(matches[[1]])
    }
  }

  return(NA_character_)
}

#' Load CLIF data file
#'
#' @description
#' Load CLIF data from CSV or Parquet file with proper type handling.
#'
#' @param file_path Character. Path to data file.
#' @param filetype Character. File type: "csv" or "parquet".
#' @param timezone Character. Timezone for datetime columns (default: "UTC").
#' @param schema List. Optional schema for type specification.
#'
#' @return tibble containing loaded data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' patient_data <- load_clif_data("patient.csv", filetype = "csv")
#' vitals_data <- load_clif_data("vitals.parquet", filetype = "parquet")
#' }
load_clif_data <- function(file_path, filetype = c("csv", "parquet"),
                            timezone = "UTC", schema = NULL) {
  filetype <- match.arg(filetype)

  if (!file.exists(file_path)) {
    cli::cli_abort("Data file not found: {.file {file_path}}")
  }

  cli::cli_alert_info("Loading data from {.file {basename(file_path)}}")

  tryCatch({
    data <- switch(filetype,
      csv = load_csv(file_path, timezone, schema),
      parquet = load_parquet(file_path, timezone)
    )

    cli::cli_alert_success(
      "Loaded {.val {nrow(data)}} rows, {.val {ncol(data)}} columns"
    )

    return(data)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to load data file",
      "x" = "Error: {e$message}"
    ))
  })
}

#' Load CSV file
#'
#' @keywords internal
load_csv <- function(file_path, timezone = "UTC", schema = NULL) {
  # Read CSV with readr for better type inference
  data <- readr::read_csv(
    file_path,
    show_col_types = FALSE,
    guess_max = 10000
  )

  # Convert datetime columns based on schema
  if (!is.null(schema) && "columns" %in% names(schema)) {
    datetime_cols <- purrr::map_lgl(schema$columns, ~.x$data_type == "DATETIME")
    datetime_col_names <- purrr::map_chr(schema$columns[datetime_cols], ~.x$name)

    for (col_name in datetime_col_names) {
      if (col_name %in% names(data)) {
        data[[col_name]] <- lubridate::with_tz(
          lubridate::as_datetime(data[[col_name]]),
          tzone = timezone
        )
      }
    }
  }

  return(dplyr::as_tibble(data))
}

#' Load Parquet file
#'
#' @keywords internal
load_parquet <- function(file_path, timezone = "UTC") {
  data <- arrow::read_parquet(file_path)

  # Convert datetime columns to proper timezone
  datetime_cols <- purrr::map_lgl(data, lubridate::is.POSIXct)
  for (col_name in names(data)[datetime_cols]) {
    data[[col_name]] <- lubridate::with_tz(data[[col_name]], tzone = timezone)
  }

  return(dplyr::as_tibble(data))
}

#' Load a single CLIF table with optional filters
#'
#' @description
#' Load data for a specific CLIF table from a directory, optionally
#' restricting rows to matching values in one or more columns. This mirrors
#' clifpy's `load_data()` helper, letting users load a single table (e.g.
#' `"vitals"`) directly without constructing a full `ClifOrchestrator`.
#'
#' @param table_name Character. Name of the CLIF table (e.g. "vitals").
#' @param table_path Character. Path to directory containing CLIF data files.
#' @param table_format_type Character. File type: "csv" or "parquet"
#'   (default: "csv").
#' @param site_tz Character. Timezone for datetime columns (default: "UTC").
#' @param filters Named list. Optional column filters to apply after
#'   loading. Each element's name is a column in the table, and its value is
#'   a scalar or vector of values; rows are kept when the column matches any
#'   of the specified values.
#' @param schema_dir Character. Custom schema directory (default: NULL uses
#'   package default).
#'
#' @return tibble containing the (optionally filtered) table data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' vitals_data <- load_data(
#'   table_name = "vitals",
#'   table_path = "path/to/clif/data",
#'   table_format_type = "csv",
#'   site_tz = "US/Eastern",
#'   filters = list(vital_category = "weight_kg")
#' )
#' }
load_data <- function(table_name, table_path,
                       table_format_type = c("csv", "parquet"),
                       site_tz = "UTC", filters = NULL, schema_dir = NULL) {
  table_format_type <- match.arg(table_format_type)

  file_path <- find_clif_table_file(table_path, table_name, table_format_type)

  if (is.na(file_path)) {
    cli::cli_abort(c(
      "Data file not found for table {.field {table_name}}",
      "i" = paste0(
        "Looked in {.file {table_path}} for ",
        "{.file {table_name}.{table_format_type}} or ",
        "{.file clif_{table_name}.{table_format_type}}"
      )
    ))
  }

  schema <- load_schema(table_name, schema_dir = schema_dir)

  data <- load_clif_data(
    file_path,
    filetype = table_format_type,
    timezone = site_tz,
    schema = schema
  )

  if (!is.null(filters) && length(filters) > 0) {
    for (column_name in names(filters)) {
      if (!(column_name %in% names(data))) {
        cli::cli_abort(
          "Filter column {.field {column_name}} not found in {.field {table_name}} table"
        )
      }

      data <- data[data[[column_name]] %in% filters[[column_name]], ]
    }
  }

  return(dplyr::as_tibble(data))
}

#' Save CLIF data file
#'
#' @description
#' Save CLIF data to CSV or Parquet file.
#'
#' @param data tibble or data.frame. Data to save.
#' @param file_path Character. Output file path.
#' @param filetype Character. File type: "csv" or "parquet".
#' @param overwrite Logical. Whether to overwrite existing file (default: FALSE).
#'
#' @return Invisible NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' save_clif_data(patient_data, "output/patient.csv", "csv")
#' save_clif_data(vitals_data, "output/vitals.parquet", "parquet")
#' }
save_clif_data <- function(data, file_path, filetype = c("csv", "parquet"),
                            overwrite = FALSE) {
  filetype <- match.arg(filetype)

  # Check if file exists
  if (file.exists(file_path) && !overwrite) {
    cli::cli_abort(c(
      "File already exists: {.file {file_path}}",
      "i" = "Set {.code overwrite = TRUE} to overwrite"
    ))
  }

  # Create directory if needed
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  cli::cli_alert_info("Saving data to {.file {basename(file_path)}}")

  tryCatch({
    switch(filetype,
      csv = readr::write_csv(data, file_path),
      parquet = arrow::write_parquet(data, file_path)
    )

    cli::cli_alert_success(
      "Saved {.val {nrow(data)}} rows, {.val {ncol(data)}} columns"
    )

    invisible(NULL)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to save data file",
      "x" = "Error: {e$message}"
    ))
  })
}

#' Load all CLIF tables from directory
#'
#' @description
#' Load all CLIF tables from a data directory.
#'
#' @param data_directory Character. Path to directory containing CLIF data files.
#' @param table_names Character vector. Names of tables to load. If NULL,
#'   attempts to load all standard CLIF tables.
#' @param filetype Character. File type: "csv" or "parquet".
#' @param timezone Character. Timezone for datetime columns (default: "UTC").
#'
#' @return Named list of tibbles, one for each table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' all_tables <- load_all_tables("data/clif_data", filetype = "csv")
#' patient_data <- all_tables$patient
#' }
load_all_tables <- function(data_directory, table_names = NULL,
                             filetype = c("csv", "parquet"),
                             timezone = "UTC") {
  filetype <- match.arg(filetype)

  if (!dir.exists(data_directory)) {
    cli::cli_abort("Data directory not found: {.file {data_directory}}")
  }

  # Standard CLIF table names
  if (is.null(table_names)) {
    table_names <- c(
      "patient", "hospitalization", "adt", "vitals", "labs",
      "respiratory_support", "medication_admin_continuous",
      "medication_admin_intermittent", "hospital_diagnosis",
      "code_status", "patient_assessments", "patient_procedures",
      "position", "crrt_therapy", "ecmo_mcs",
      "microbiology_culture", "microbiology_nonculture",
      "microbiology_susceptibility"
    )
  }

  tables <- list()

  cli::cli_alert_info("Loading tables from {.file {data_directory}}")

  for (table_name in table_names) {
    file_path <- find_clif_table_file(data_directory, table_name, filetype)

    if (!is.na(file_path)) {
      tables[[table_name]] <- load_clif_data(
        file_path,
        filetype = filetype,
        timezone = timezone
      )
    } else {
      cli::cli_alert_warning("Table {.val {table_name}} not found, skipping")
    }
  }

  if (length(tables) == 0) {
    cli::cli_abort("No CLIF tables found in directory")
  }

  cli::cli_alert_success("Loaded {.val {length(tables)}} tables")

  return(tables)
}

#' Export data to JSON
#'
#' @description
#' Export data to JSON format for interoperability.
#'
#' @param data tibble or data.frame. Data to export.
#' @param file_path Character. Output JSON file path.
#' @param pretty Logical. Pretty print JSON (default: TRUE).
#'
#' @return Invisible NULL.
#'
#' @export
export_to_json <- function(data, file_path, pretty = TRUE) {
  tryCatch({
    jsonlite::write_json(
      data,
      file_path,
      pretty = pretty,
      auto_unbox = TRUE,
      POSIXt = "ISO8601"
    )

    cli::cli_alert_success("Exported to {.file {file_path}}")
    invisible(NULL)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to export to JSON",
      "x" = "Error: {e$message}"
    ))
  })
}

#' Get file size in human-readable format
#'
#' @description
#' Get file size with appropriate units.
#'
#' @param file_path Character. Path to file.
#'
#' @return Character string with size and units.
#'
#' @keywords internal
get_file_size <- function(file_path) {
  if (!file.exists(file_path)) {
    return("File not found")
  }

  size_bytes <- file.info(file_path)$size
  units <- c("B", "KB", "MB", "GB", "TB")
  unit_idx <- 1

  while (size_bytes >= 1024 && unit_idx < length(units)) {
    size_bytes <- size_bytes / 1024
    unit_idx <- unit_idx + 1
  }

  sprintf("%.2f %s", size_bytes, units[unit_idx])
}
