#' BaseTable: foundation class for all CLIF tables
#'
#' Provides data loading, schema resolution, validation and summary reporting shared
#' by every CLIF table class. Ported from `clifpy/tables/base_table.py`; field and
#' method names follow the Python original so results and call sites line up across
#' the two implementations.
#'
#' @details
#' The `table_name` is derived from the class name, so `RespiratorySupport` resolves
#' to `respiratory_support` and loads `respiratory_support_schema.yaml` for the
#' configured CLIF version.
#'
#' @export
#' @importFrom R6 R6Class
BaseTable <- R6::R6Class(
  classname = "BaseTable",
  public = list(
    #' @field data_directory Directory containing the CLIF data files.
    data_directory = NULL,
    #' @field filetype Either `"csv"` or `"parquet"`.
    filetype = NULL,
    #' @field timezone Olson timezone used for datetime columns.
    timezone = NULL,
    #' @field clif_version CLIF schema version this table validates against.
    clif_version = NULL,
    #' @field output_directory Directory for logs, summaries and validation output.
    output_directory = NULL,
    #' @field table_name snake_case CLIF table name derived from the class name.
    table_name = NULL,
    #' @field df The loaded data as a tibble.
    df = NULL,
    #' @field schema Parsed YAML schema for this table.
    schema = NULL,
    #' @field errors List of validation errors from the last validation run.
    errors = NULL,
    #' @field outlier_config Parsed outlier configuration.
    outlier_config = NULL,

    #' @description Create a table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @param table_name Optional explicit table name; normally derived from the class.
    #'
    #' @return A new table instance.
    initialize = function(data_directory = NULL,
                          filetype = NULL,
                          timezone = "UTC",
                          output_directory = NULL,
                          data = NULL,
                          clif_version = DEFAULT_CLIF_VERSION,
                          table_name = NULL) {
      # clifpy allows constructing purely from an in-memory frame; the directory and
      # filetype then carry placeholder values rather than being required.
      if (is.null(data_directory) && is.null(filetype) && !is.null(data)) {
        data_directory <- "."
        filetype <- "parquet"
      }

      self$table_name <- table_name %||% pascal_to_snake_case(class(self)[1])
      self$data_directory <- data_directory
      self$filetype <- filetype
      self$timezone <- timezone
      self$clif_version <- clif_version
      self$output_directory <- output_directory %||% file.path(data_directory %||% ".", "output")
      self$errors <- list()
      private$has_been_validated <- FALSE

      if (!dir.exists(self$output_directory)) {
        dir.create(self$output_directory, recursive = TRUE, showWarnings = FALSE)
      }

      self$schema <- load_schema(self$table_name, self$clif_version)
      self$outlier_config <- tryCatch(
        load_shared_config("outlier_config.yaml"),
        error = function(condition) NULL
      )

      if (!is.null(data)) {
        self$df <- dplyr::as_tibble(data)
      } else if (!is.null(data_directory) && !is.null(filetype)) {
        self$df <- load_data(
          table_name = self$table_name,
          table_path = data_directory,
          table_format_type = filetype,
          site_tz = timezone
        )
      }

      invisible(self)
    },

    #' @description Run validation against the table schema.
    #'
    #' Populates `$errors` with schema and data-quality findings, then runs any
    #' table-specific checks a subclass defines.
    #' @param verbose Whether to print a summary of the results.
    #' @return The table object, invisibly.
    validate = function(verbose = TRUE) {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No dataframe to validate")
        return(invisible(self))
      }

      self$errors <- list()
      private$has_been_validated <- TRUE

      if (!is.null(self$schema)) {
        schema_errors <- validate_dataframe(self$df, self$schema, self$table_name)
        self$errors <- c(self$errors, schema_errors)
      }

      table_specific_errors <- private$run_table_specific_validations()
      if (length(table_specific_errors) > 0) {
        self$errors <- c(self$errors, table_specific_errors)
      }

      if (verbose) {
        if (length(self$errors) == 0) {
          cli::cli_alert_success("Validation completed successfully for {.val {self$table_name}}")
        } else {
          cli::cli_alert_warning(
            "Validation completed with {.val {length(self$errors)}} error(s) for {.val {self$table_name}}"
          )
        }
      }

      if (length(self$errors) > 0) {
        private$save_validation_errors()
      }

      invisible(self)
    },

    #' @description Whether the last validation run found no errors.
    #' @return `TRUE` when validation has run and produced no errors.
    isvalid = function() {
      if (!private$has_been_validated) {
        cli::cli_alert_warning("Validation has not been run yet. Please call validate() first.")
        return(FALSE)
      }
      length(self$errors) == 0
    },

    #' @description Summary statistics for the table.
    #' @return A named list describing shape, memory use, validation state,
    #'   numeric summaries and missing-data counts.
    get_summary = function() {
      if (is.null(self$df)) {
        return(list(status = "No data loaded"))
      }

      summary_result <- list(
        table_name = self$table_name,
        num_rows = nrow(self$df),
        num_columns = ncol(self$df),
        columns = names(self$df),
        memory_usage_mb = as.numeric(utils::object.size(self$df)) / 1024 / 1024,
        validation_run = private$has_been_validated,
        validation_errors = if (private$has_been_validated) length(self$errors) else NULL,
        is_valid = private$has_been_validated && length(self$errors) == 0
      )

      numeric_column_names <- names(self$df)[vapply(self$df, is.numeric, logical(1))]
      if (length(numeric_column_names) > 0) {
        summary_result$numeric_columns <- numeric_column_names
        summary_result$numeric_stats <- lapply(
          stats::setNames(numeric_column_names, numeric_column_names),
          function(column_name) describe_numeric_column(self$df[[column_name]])
        )
      }

      missing_counts <- vapply(self$df, function(column) sum(is.na(column)), numeric(1))
      missing_counts <- missing_counts[missing_counts > 0]
      if (length(missing_counts) > 0) {
        summary_result$missing_data <- as.list(missing_counts)
      }

      summary_result
    },

    #' @description Write the table summary to `summary_<table>.json`.
    #' @return Path to the written file, invisibly.
    save_summary = function() {
      summary_file <- file.path(self$output_directory, paste0("summary_", self$table_name, ".json"))
      jsonlite::write_json(
        self$get_summary(),
        summary_file,
        auto_unbox = TRUE,
        pretty = TRUE,
        digits = NA,
        na = "null"
      )
      invisible(summary_file)
    },

    #' @description Distribution of each categorical column by unique encounter.
    #'
    #' Counts distinct `hospitalization_id` (or `patient_id` when the former is
    #' absent) per category value, matching clifpy's denominator choice.
    #' @param save Whether to write one CSV per categorical column.
    #' @return A named list of tibbles, one per categorical column.
    analyze_categorical_distributions = function(save = TRUE) {
      if (is.null(self$df) || is.null(self$schema)) {
        return(list())
      }

      categorical_columns <- intersect(schema_category_columns(self$schema), names(self$df))
      if (length(categorical_columns) == 0) {
        return(list())
      }

      id_column <- if ("hospitalization_id" %in% names(self$df)) {
        "hospitalization_id"
      } else if ("patient_id" %in% names(self$df)) {
        "patient_id"
      } else {
        NULL
      }

      distributions <- list()
      for (column_name in categorical_columns) {
        if (is.null(id_column)) {
          distribution <- dplyr::count(self$df, .data[[column_name]], name = "count")
        } else {
          distribution <- self$df |>
            dplyr::group_by(.data[[column_name]]) |>
            dplyr::summarise(count = dplyr::n_distinct(.data[[id_column]]), .groups = "drop")
        }
        distribution <- distribution |>
          dplyr::mutate(percentage = 100 * .data$count / sum(.data$count)) |>
          dplyr::arrange(dplyr::desc(.data$count))

        distributions[[column_name]] <- distribution

        if (save) {
          readr::write_csv(
            distribution,
            file.path(self$output_directory, sprintf("distribution_%s_%s.csv", self$table_name, column_name))
          )
        }
      }

      distributions
    },

    #' @description Bar plots of each categorical column's distribution by encounter.
    #'
    #' Counts distinct `hospitalization_id` (or `patient_id`) per category value and
    #' draws one bar plot per categorical column. Ports clifpy's
    #' `plot_categorical_distributions`; uses ggplot2 with the viridis palette.
    #' @param columns Optional character vector restricting which categorical columns
    #'   to plot.
    #' @param save Whether to write one PNG per column to the output directory.
    #' @param width,height Plot dimensions in inches.
    #' @param dpi Resolution for saved plots.
    #' @return A named list of ggplot objects, one per categorical column.
    plot_categorical_distributions = function(columns = NULL, save = TRUE,
                                              width = 10, height = 6, dpi = 300) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        cli::cli_abort("Package {.pkg ggplot2} is required for plotting. Install it first.")
      }
      distributions <- self$analyze_categorical_distributions(save = FALSE)
      if (!is.null(columns)) {
        distributions <- distributions[intersect(names(distributions), columns)]
      }
      if (length(distributions) == 0) {
        return(list())
      }

      plots <- list()
      for (column_name in names(distributions)) {
        distribution <- distributions[[column_name]]
        distribution[[column_name]] <- factor(
          as.character(distribution[[column_name]]),
          levels = as.character(distribution[[column_name]])
        )

        plot_object <- ggplot2::ggplot(
          distribution,
          ggplot2::aes(x = .data[[column_name]], y = .data$count, fill = .data$count)
        ) +
          ggplot2::geom_col(width = 0.8) +
          ggplot2::scale_fill_viridis_c(option = "cividis", guide = "none") +
          ggplot2::labs(
            title = sprintf("Distribution of %s", column_name),
            x = "Category",
            y = "Unique encounter count"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

        plots[[column_name]] <- plot_object

        if (save) {
          plot_path <- file.path(
            self$output_directory,
            sprintf("categorical_dist_%s_%s.png", self$table_name, column_name)
          )
          ggplot2::ggsave(plot_path, plot_object, width = width, height = height, dpi = dpi)
        }
      }

      plots
    },

    #' @description Empirical CDF of a numeric column stratified by category.
    #'
    #' For each category value, computes the ECDF of `value_column`: sorted values
    #' paired with `rank / n` cumulative probabilities. Ports clifpy's
    #' `calculate_stratified_ecdf`.
    #' @param value_column Numeric column to compute the ECDF for.
    #' @param category_column Categorical column to stratify by.
    #' @param category_values Optional character vector of categories to include.
    #'   Defaults to the schema's permissible values, or all observed values.
    #' @param save Whether to write the combined ECDF to a CSV.
    #' @return A named list of tibbles, one per category, or `NULL` when no data.
    calculate_stratified_ecdf = function(value_column, category_column,
                                         category_values = NULL, save = TRUE) {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No dataframe available for ECDF calculation")
        return(NULL)
      }
      if (!value_column %in% names(self$df) || !category_column %in% names(self$df)) {
        cli::cli_alert_warning("Value or category column not found in data")
        return(NULL)
      }

      if (is.null(category_values)) {
        category_values <- schema_permissible_values(self$schema, category_column)
        if (length(category_values) == 0) {
          category_values <- unique(self$df[[category_column]][!is.na(self$df[[category_column]])])
        }
      }

      ecdf_by_category <- list()
      for (category in category_values) {
        category_values_vector <- self$df[[value_column]][
          !is.na(self$df[[category_column]]) & self$df[[category_column]] == category
        ]
        category_values_vector <- sort(category_values_vector[!is.na(category_values_vector)])
        observation_count <- length(category_values_vector)
        if (observation_count == 0) {
          next
        }
        ecdf_by_category[[as.character(category)]] <- dplyr::tibble(
          !!value_column := category_values_vector,
          cumulative_probability = seq_len(observation_count) / observation_count,
          !!category_column := category
        )
      }

      if (length(ecdf_by_category) == 0) {
        cli::cli_alert_warning("No valid ECDF data for any category")
        return(NULL)
      }

      if (save) {
        combined_ecdf <- dplyr::bind_rows(ecdf_by_category)
        csv_path <- file.path(
          self$output_directory,
          sprintf("ecdf_%s_%s_by_%s.csv", self$table_name, value_column, category_column)
        )
        readr::write_csv(combined_ecdf, csv_path)
      }

      ecdf_by_category
    },

    #' @description Print a short description of the table.
    #' @param ... Unused; present for print compatibility.
    #' @return The table object, invisibly.
    print = function(...) {
      cli::cli_h3("CLIF table: {self$table_name} (CLIF {self$clif_version})")
      if (is.null(self$df)) {
        cli::cli_text("No data loaded")
      } else {
        cli::cli_text("{.val {nrow(self$df)}} rows x {.val {ncol(self$df)}} columns")
        if (private$has_been_validated) {
          if (length(self$errors) == 0) {
            cli::cli_alert_success("Validated: no errors")
          } else {
            cli::cli_alert_warning("Validated: {.val {length(self$errors)}} error(s)")
          }
        } else {
          cli::cli_text("Not yet validated")
        }
      }
      invisible(self)
    }
  ),
  private = list(
    has_been_validated = FALSE,

    # Overridden by subclasses needing checks beyond the schema, for example
    # MicrobiologyCulture's timestamp ordering. Returns a list of error records.
    run_table_specific_validations = function() {
      list()
    },

    save_validation_errors = function() {
      if (length(self$errors) == 0) {
        return(invisible(NULL))
      }
      # The output directory is created in the constructor, but it can disappear
      # between construction and validation (a caller passing a temp dir, a cleanup
      # hook), so re-create it rather than letting the write abort a validation whose
      # results are otherwise complete.
      if (!dir.exists(self$output_directory)) {
        dir.create(self$output_directory, recursive = TRUE, showWarnings = FALSE)
      }
      errors_frame <- validation_errors_to_frame(self$errors)
      error_file <- file.path(
        self$output_directory,
        paste0("validation_errors_", self$table_name, ".csv")
      )
      readr::write_csv(errors_frame, error_file)
      invisible(error_file)
    }
  )
)

#' Load a CLIF table from file
#'
#' Mirrors clifpy's `Table.from_file` classmethod. Resolves configuration from
#' explicit arguments, then a config file, then package defaults, loads the data and
#' returns the corresponding table object.
#'
#' @param table_name snake_case CLIF table name, e.g. `"labs"`.
#' @param data_directory Directory containing the CLIF data files.
#' @param filetype Either `"csv"` or `"parquet"`.
#' @param timezone Olson timezone for datetime columns.
#' @param config_path Optional path to a JSON or YAML config file.
#' @param output_directory Directory for logs and outputs.
#' @param sample_size Optional maximum number of rows to read.
#' @param columns Optional character vector of columns to read.
#' @param filters Optional named list of equality filters applied at read time.
#' @param verbose Whether to emit loading messages.
#' @param clif_version CLIF schema version. Overrides any value in the config file.
#'
#' @return A table object of the class matching `table_name`.
#' @export
#'
#' @examples
#' \dontrun{
#' labs <- clif_table_from_file("labs", "data/clif", "parquet", "US/Central")
#' }
clif_table_from_file <- function(table_name,
                                 data_directory = NULL,
                                 filetype = NULL,
                                 timezone = NULL,
                                 config_path = NULL,
                                 output_directory = NULL,
                                 sample_size = NULL,
                                 columns = NULL,
                                 filters = NULL,
                                 verbose = FALSE,
                                 clif_version = NULL) {
  resolved_config <- get_config_or_params(
    config_path = config_path,
    data_directory = data_directory,
    filetype = filetype,
    timezone = timezone,
    output_directory = output_directory
  )

  resolved_version <- clif_version %||% resolved_config$clif_version %||% DEFAULT_CLIF_VERSION

  loaded_data <- load_data(
    table_name = table_name,
    table_path = resolved_config$data_directory,
    table_format_type = resolved_config$filetype,
    sample_size = sample_size,
    columns = columns,
    filters = filters,
    site_tz = resolved_config$timezone,
    verbose = verbose
  )

  table_generator <- get_table_class(table_name)
  table_generator$new(
    data_directory = resolved_config$data_directory,
    filetype = resolved_config$filetype,
    timezone = resolved_config$timezone,
    output_directory = resolved_config$output_directory %||% output_directory,
    data = loaded_data,
    clif_version = resolved_version
  )
}

#' Column names flagged as categorical in a schema
#'
#' @param schema Parsed table schema.
#' @return Character vector of category column names.
#' @keywords internal
schema_category_columns <- function(schema) {
  if (!is.null(schema$category_columns) && length(schema$category_columns) > 0) {
    return(unlist(schema$category_columns, use.names = FALSE))
  }
  if (is.null(schema$columns)) {
    return(character(0))
  }
  is_category <- vapply(
    schema$columns,
    function(column) isTRUE(column$is_category_column),
    logical(1)
  )
  vapply(schema$columns[is_category], function(column) column$name, character(1))
}

#' Permissible values declared for a schema column
#'
#' @param schema Parsed table schema.
#' @param column_name Name of the column whose permissible values are wanted.
#' @return Character vector of permissible values, empty when the column is absent
#'   or declares none.
#' @keywords internal
schema_permissible_values <- function(schema, column_name) {
  if (is.null(schema$columns)) {
    return(character(0))
  }
  for (column_definition in schema$columns) {
    if (identical(column_definition$name, column_name)) {
      return(unlist(column_definition$permissible_values, use.names = FALSE) %||% character(0))
    }
  }
  character(0)
}

#' Numeric summary matching pandas describe()
#'
#' Produces the same fields pandas' `describe()` returns so summaries compare
#' directly against the Python baselines.
#'
#' @param values A numeric vector.
#' @return A named list with count, mean, std, min, 25%, 50%, 75% and max.
#' @keywords internal
describe_numeric_column <- function(values) {
  non_missing_values <- values[!is.na(values)]
  if (length(non_missing_values) == 0) {
    return(list(
      count = 0, mean = NA_real_, std = NA_real_, min = NA_real_,
      `25%` = NA_real_, `50%` = NA_real_, `75%` = NA_real_, max = NA_real_
    ))
  }
  quantile_values <- stats::quantile(
    non_missing_values,
    probs = c(0.25, 0.5, 0.75),
    names = FALSE,
    type = 7
  )
  list(
    count = length(non_missing_values),
    mean = mean(non_missing_values),
    std = stats::sd(non_missing_values),
    min = min(non_missing_values),
    `25%` = quantile_values[1],
    `50%` = quantile_values[2],
    `75%` = quantile_values[3],
    max = max(non_missing_values)
  )
}

#' Flatten validation error records into a data frame
#'
#' Nested `details` are serialized to JSON so the CSV round-trips without losing
#' information, matching how clifpy writes its error CSV.
#'
#' @param errors List of validation error records.
#' @return A tibble with one row per error.
#' @keywords internal
validation_errors_to_frame <- function(errors) {
  if (length(errors) == 0) {
    return(dplyr::tibble())
  }
  dplyr::bind_rows(lapply(errors, function(error_record) {
    flattened <- lapply(error_record, function(field_value) {
      if (is.null(field_value)) {
        NA_character_
      } else if (is.list(field_value) || length(field_value) > 1) {
        as.character(jsonlite::toJSON(field_value, auto_unbox = TRUE, null = "null"))
      } else {
        as.character(field_value)
      }
    })
    dplyr::as_tibble(flattened)
  }))
}

#' Null-coalescing operator
#'
#' @param lhs Value to use when not `NULL`.
#' @param rhs Fallback value.
#' @return `lhs` when it is not `NULL`, otherwise `rhs`.
#' @name null-coalesce
#' @keywords internal
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}
