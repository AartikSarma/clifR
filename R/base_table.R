#' Base Table Class for CLIF Data
#'
#' @description
#' R6 class providing common functionality for all CLIF table types.
#' All specific table classes (Patient, Vitals, Labs, etc.) inherit from this base class.
#'
#' @export
#' @importFrom R6 R6Class
BaseTable <- R6::R6Class(
  classname = "BaseTable",

  public = list(
    #' @field df tibble containing the table data
    df = NULL,

    #' @field schema List containing the YAML schema definition
    schema = NULL,

    #' @field table_name Character name of the table
    table_name = NULL,

    #' @field validation_results List containing validation results
    validation_results = NULL,

    #' @field timezone Character timezone for datetime columns
    timezone = NULL,

    #' @field data_directory Character path to data directory
    data_directory = NULL,

    #' @field filetype Character file type ("csv" or "parquet")
    filetype = NULL,

    #' @field output_directory Character path for output files
    output_directory = NULL,

    #' @description
    #' Initialize a BaseTable instance
    #'
    #' @param table_name Character. Name of the CLIF table.
    #' @param data_directory Character. Path to directory containing data files.
    #' @param filetype Character. File type: "csv" or "parquet" (default: "csv").
    #' @param timezone Character. Timezone for datetime columns (default: "UTC").
    #' @param output_directory Character. Path for saving outputs (default: NULL).
    #' @param data Optional tibble. Pre-loaded data (if NULL, loads from file).
    #' @param schema_dir Character. Custom schema directory (default: NULL uses package default).
    #'
    #' @return A new BaseTable instance.
    initialize = function(table_name,
                          data_directory = NULL,
                          filetype = "csv",
                          timezone = "UTC",
                          output_directory = NULL,
                          data = NULL,
                          schema_dir = NULL) {

      self$table_name <- table_name
      self$data_directory <- data_directory
      self$filetype <- filetype
      self$timezone <- timezone
      self$output_directory <- output_directory

      # Load schema
      self$schema <- load_schema(table_name, schema_dir = schema_dir)

      # Load or set data
      if (!is.null(data)) {
        self$df <- dplyr::as_tibble(data)
        cli::cli_alert_info(
          "Loaded {.val {nrow(self$df)}} rows for {.field {table_name}} table from provided data"
        )
      } else {
        self$load_data()
      }

      invisible(self)
    },

    #' @description
    #' Load data from file
    load_data = function() {
      if (is.null(self$data_directory)) {
        cli::cli_abort("No data directory specified and no data provided")
      }

      file_path <- file.path(
        self$data_directory,
        paste0(self$table_name, ".", self$filetype)
      )

      self$df <- load_clif_data(
        file_path,
        filetype = self$filetype,
        timezone = self$timezone,
        schema = self$schema
      )

      invisible(self)
    },

    #' @description
    #' Validate table data against schema
    #'
    #' @param verbose Logical. Print validation details (default: TRUE).
    #'
    #' @return List of validation results.
    validate = function(verbose = TRUE) {
      if (verbose) {
        cli::cli_h2("Validating {self$table_name} table")
      }

      self$validation_results <- validate_table(
        data = self$df,
        schema = self$schema,
        table_name = self$table_name
      )

      return(self$validation_results)
    },

    #' @description
    #' Check if table is valid
    #'
    #' @return Logical. TRUE if valid, FALSE otherwise.
    is_valid = function() {
      if (is.null(self$validation_results)) {
        self$validate(verbose = FALSE)
      }

      return(self$validation_results$is_valid)
    },

    #' @description
    #' Get validation errors
    #'
    #' @return List of errors.
    get_errors = function() {
      if (is.null(self$validation_results)) {
        self$validate(verbose = FALSE)
      }

      return(self$validation_results$errors)
    },

    #' @description
    #' Get validation warnings
    #'
    #' @return List of warnings.
    get_warnings = function() {
      if (is.null(self$validation_results)) {
        self$validate(verbose = FALSE)
      }

      return(self$validation_results$warnings)
    },

    #' @description
    #' Generate summary statistics
    #'
    #' @return List of summary statistics.
    summarize = function() {
      cli::cli_h2("Summary: {self$table_name}")

      summary_stats <- list(
        table_name = self$table_name,
        n_rows = nrow(self$df),
        n_cols = ncol(self$df),
        columns = names(self$df),
        memory_size = object.size(self$df),
        datetime_range = self$get_datetime_range()
      )

      cli::cli_text("Rows: {.val {summary_stats$n_rows}}")
      cli::cli_text("Columns: {.val {summary_stats$n_cols}}")
      cli::cli_text("Memory: {.val {format(summary_stats$memory_size, units = 'MB')}}")

      if (!is.null(summary_stats$datetime_range)) {
        cli::cli_text(
          "Date range: {.val {summary_stats$datetime_range$min}} to {.val {summary_stats$datetime_range$max}}"
        )
      }

      return(invisible(summary_stats))
    },

    #' @description
    #' Get datetime column range
    #'
    #' @return List with min and max datetime, or NULL if no datetime columns.
    get_datetime_range = function() {
      datetime_cols <- purrr::map_lgl(self$df, lubridate::is.POSIXct)

      if (!any(datetime_cols)) {
        return(NULL)
      }

      # Get first datetime column for range
      first_datetime_col <- names(self$df)[datetime_cols][1]

      list(
        min = min(self$df[[first_datetime_col]], na.rm = TRUE),
        max = max(self$df[[first_datetime_col]], na.rm = TRUE),
        column = first_datetime_col
      )
    },

    #' @description
    #' Save table data to file
    #'
    #' @param file_path Character. Output file path. If NULL, uses output_directory and table_name.
    #' @param filetype Character. File type: "csv" or "parquet". If NULL, uses self$filetype.
    #' @param overwrite Logical. Whether to overwrite existing file (default: FALSE).
    #'
    #' @return Invisible self.
    save = function(file_path = NULL, filetype = NULL, overwrite = FALSE) {
      if (is.null(file_path)) {
        if (is.null(self$output_directory)) {
          cli::cli_abort("No output directory specified")
        }

        ft <- filetype %||% self$filetype
        file_path <- file.path(
          self$output_directory,
          paste0(self$table_name, ".", ft)
        )
      }

      ft <- filetype %||% self$filetype

      save_clif_data(
        data = self$df,
        file_path = file_path,
        filetype = ft,
        overwrite = overwrite
      )

      invisible(self)
    },

    #' @description
    #' Export validation results to markdown report
    #'
    #' @param output_file Character. Path to output file.
    #'
    #' @return Invisible self.
    export_validation_report = function(output_file = NULL) {
      if (is.null(self$validation_results)) {
        self$validate()
      }

      if (is.null(output_file)) {
        if (is.null(self$output_directory)) {
          cli::cli_abort("No output directory specified")
        }

        output_file <- file.path(
          self$output_directory,
          paste0(self$table_name, "_validation_report.md")
        )
      }

      generate_validation_report(self$validation_results, output_file)

      invisible(self)
    },

    #' @description
    #' Get unique values for a categorical column
    #'
    #' @param column_name Character. Name of column.
    #' @param include_na Logical. Include NA values (default: FALSE).
    #'
    #' @return tibble with unique values and counts.
    get_unique_values = function(column_name, include_na = FALSE) {
      if (!(column_name %in% names(self$df))) {
        cli::cli_abort("Column {.field {column_name}} not found in table")
      }

      result <- self$df %>%
        dplyr::count(.data[[column_name]], name = "n") %>%
        dplyr::arrange(dplyr::desc(n))

      if (!include_na) {
        result <- result %>%
          dplyr::filter(!is.na(.data[[column_name]]))
      }

      return(result)
    },

    #' @description
    #' Filter table by criteria
    #'
    #' @param ... Filter expressions passed to dplyr::filter().
    #'
    #' @return New BaseTable instance with filtered data.
    filter_data = function(...) {
      filtered_df <- self$df %>%
        dplyr::filter(...)

      # Create new instance with filtered data
      new_instance <- self$clone()
      new_instance$df <- filtered_df
      new_instance$validation_results <- NULL  # Reset validation

      return(new_instance)
    },

    #' @description
    #' Get column names by type
    #'
    #' @param type Character. Column type: "category", "datetime", "numeric", etc.
    #'
    #' @return Character vector of column names.
    get_columns_by_type = function(type = c("category", "datetime", "numeric", "character")) {
      type <- match.arg(type)

      switch(type,
        category = {
          category_flags <- purrr::map_lgl(
            self$schema$columns,
            ~.x$is_category_column
          )
          purrr::map_chr(self$schema$columns[category_flags], ~.x$name)
        },
        datetime = {
          datetime_flags <- purrr::map_lgl(
            self$schema$columns,
            ~.x$data_type == "DATETIME"
          )
          purrr::map_chr(self$schema$columns[datetime_flags], ~.x$name)
        },
        numeric = {
          names(self$df)[purrr::map_lgl(self$df, is.numeric)]
        },
        character = {
          names(self$df)[purrr::map_lgl(self$df, is.character)]
        }
      )
    },

    #' @description
    #' Print method
    print = function() {
      cli::cli_h2("{self$table_name} Table")
      cli::cli_text("Rows: {.val {nrow(self$df)}}")
      cli::cli_text("Columns: {.val {ncol(self$df)}}")

      if (!is.null(self$validation_results)) {
        status <- if (self$validation_results$is_valid) {
          cli::col_green("\u2713 Valid")
        } else {
          cli::col_red("\u2717 Invalid")
        }
        cli::cli_text("Validation: {status}")
      } else {
        cli::cli_text("Validation: {.emph Not yet validated}")
      }

      cli::cli_rule()
      print(self$df)

      invisible(self)
    }
  )
)

# Helper operator for NULL defaults
`%||%` <- function(x, y) if (is.null(x)) y else x
