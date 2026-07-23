#' Outlier detection and handling
#'
#' Port of `clifpy/utils/outlier_handler.py`. Values that fall outside the
#' acceptable ranges declared in `inst/schemas/outlier_config.yaml` are replaced
#' with `NA`. Three range styles are supported, exactly as in clifpy:
#'
#' * **simple** — `column: {min, max}` (e.g. `respiratory_support$peep_set`),
#' * **category-dependent** — `column: {category: {min, max}}`, used for
#'   `vitals$vital_value`, `labs$lab_value_numeric` and
#'   `patient_assessments$numerical_value`,
#' * **medication** — `med_dose: {med_category: {med_dose_unit: {min, max}}}`
#'   for `medication_admin_continuous` and `medication_admin_intermittent`.
#'
#' Ranges are always read from the YAML config; nothing is hardcoded here.
#'
#' @name clif-outlier-handler
NULL

# Tables whose configured value column is category-dependent, and the column
# holding the category. Mirrors the branch conditions in clifpy's
# `_build_column_expression`.
OUTLIER_CATEGORY_COLUMNS <- list(
  vitals = list(value_column = "vital_value", category_column = "vital_category"),
  labs = list(value_column = "lab_value_numeric", category_column = "lab_category"),
  patient_assessments = list(
    value_column = "numerical_value",
    category_column = "assessment_category"
  )
)

OUTLIER_MEDICATION_TABLES <- c(
  "medication_admin_continuous",
  "medication_admin_intermittent"
)

#' Load the outlier configuration
#'
#' @param outlier_config_path Optional path to a custom YAML config. When `NULL`
#'   the packaged CLIF standard config is used.
#' @return Parsed config as a list, or `NULL` when it cannot be read.
#' @keywords internal
load_outlier_config <- function(outlier_config_path = NULL) {
  tryCatch(
    {
      if (is.null(outlier_config_path)) {
        load_shared_config("outlier_config.yaml")
      } else {
        if (!file.exists(outlier_config_path)) {
          cli::cli_alert_warning(
            "Outlier configuration file not found: {.file {outlier_config_path}}"
          )
          return(NULL)
        }
        yaml::read_yaml(outlier_config_path)
      }
    },
    error = function(condition) {
      cli::cli_alert_warning("Error loading outlier configuration: {conditionMessage(condition)}")
      NULL
    }
  )
}

# TRUE when a config node is a leaf range, i.e. carries both min and max.
is_outlier_range <- function(config_node) {
  is.list(config_node) && !is.null(config_node$min) && !is.null(config_node$max)
}

# Row mask marking values outside [min, max]. NA values and NA categories never
# match, matching Polars' null-propagating comparisons.
outlier_mask <- function(numeric_values, minimum_value, maximum_value) {
  mask <- (numeric_values < minimum_value) | (numeric_values > maximum_value)
  mask & !is.na(mask)
}

#' Apply outlier handling to a CLIF table object
#'
#' Sets every configured numeric value that falls outside its acceptable range to
#' `NA`.
#'
#' **The input object is modified.** clifR table objects are R6 objects with
#' reference semantics, so `table_obj$df` is replaced in place exactly as clifpy
#' replaces `table_obj.df`; the returned value is the same object, not a copy.
#' Take a copy of `table_obj$df` beforehand if you need the original values.
#'
#' @param table_obj A clifR table object (a [BaseTable] subclass) with `$df` and
#'   `$table_name`.
#' @param outlier_config_path Optional path to a custom outlier configuration
#'   YAML. When `NULL`, the packaged CLIF standard configuration is used.
#'
#' @return The same table object, invisibly, with out-of-range values in `$df`
#'   set to `NA`.
#' @export
#'
#' @examples
#' \dontrun{
#' vitals <- clif_table_from_file("vitals", "data/clif", "parquet", clif_version = "3.0")
#' apply_outlier_handling(vitals)
#' }
apply_outlier_handling <- function(table_obj, outlier_config_path = NULL) {
  if (is.null(table_obj$df) || nrow(table_obj$df) == 0) {
    cli::cli_alert_info("No data to process for outlier handling.")
    return(invisible(table_obj))
  }

  outlier_config <- load_outlier_config(outlier_config_path)
  if (is.null(outlier_config)) {
    cli::cli_alert_warning("Failed to load outlier configuration.")
    return(invisible(table_obj))
  }

  if (is.null(outlier_config_path)) {
    cli::cli_alert_info("Using CLIF standard outlier ranges")
  } else {
    cli::cli_alert_info("Using custom outlier ranges from: {.file {outlier_config_path}}")
  }

  table_config <- outlier_config$tables[[table_obj$table_name]]
  if (is.null(table_config) || length(table_config) == 0) {
    cli::cli_alert_info("No outlier configuration found for table: {table_obj$table_name}")
    return(invisible(table_obj))
  }

  configured_columns <- intersect(names(table_config), names(table_obj$df))
  if (length(configured_columns) == 0) {
    cli::cli_alert_info("No configured columns found in dataframe.")
    return(invisible(table_obj))
  }

  for (column_name in configured_columns) {
    table_obj$df <- apply_outlier_handling_to_column(
      data = table_obj$df,
      table_name = table_obj$table_name,
      column_name = column_name,
      column_config = table_config[[column_name]]
    )
  }

  invisible(table_obj)
}

# Dispatch a single configured column to the matching range style. Mirrors
# clifpy's `_build_column_expression`; columns that do not match any supported
# style are left untouched.
apply_outlier_handling_to_column <- function(data, table_name, column_name, column_config) {
  category_spec <- OUTLIER_CATEGORY_COLUMNS[[table_name]]

  if (!is.null(category_spec) && identical(column_name, category_spec$value_column)) {
    return(apply_category_dependent_outliers(
      data = data,
      column_name = column_name,
      category_column = category_spec$category_column,
      column_config = column_config
    ))
  }

  if (table_name %in% OUTLIER_MEDICATION_TABLES && identical(column_name, "med_dose")) {
    return(apply_medication_outliers(data = data, column_config = column_config))
  }

  apply_simple_range_outliers(
    data = data,
    column_name = column_name,
    column_config = column_config
  )
}

apply_simple_range_outliers <- function(data, column_name, column_config) {
  if (!is_outlier_range(column_config)) {
    return(data)
  }
  numeric_values <- as_outlier_numeric(data[[column_name]], column_name)
  if (is.null(numeric_values)) {
    return(data)
  }
  values_to_null <- outlier_mask(numeric_values, column_config$min, column_config$max)
  data[[column_name]][values_to_null] <- NA
  data
}

apply_category_dependent_outliers <- function(data, column_name, category_column, column_config) {
  if (!category_column %in% names(data)) {
    cli::cli_alert_warning(
      paste0(
        "Category column '{category_column}' not found in dataframe. ",
        "Skipping outlier handling for {column_name}."
      )
    )
    return(data)
  }

  numeric_values <- as_outlier_numeric(data[[column_name]], column_name)
  if (is.null(numeric_values)) {
    return(data)
  }
  category_values <- tolower(as.character(data[[category_column]]))

  for (category_name in names(column_config)) {
    range_config <- column_config[[category_name]]
    if (!is_outlier_range(range_config)) {
      next
    }
    category_matches <- !is.na(category_values) & category_values == tolower(category_name)
    values_to_null <- category_matches &
      outlier_mask(numeric_values, range_config$min, range_config$max)
    data[[column_name]][values_to_null] <- NA
  }

  data
}

apply_medication_outliers <- function(data, column_config) {
  required_columns <- c("med_category", "med_dose_unit")
  if (!all(required_columns %in% names(data))) {
    return(data)
  }

  numeric_values <- as_outlier_numeric(data[["med_dose"]], "med_dose")
  if (is.null(numeric_values)) {
    return(data)
  }
  medication_categories <- tolower(as.character(data[["med_category"]]))
  dose_units <- tolower(as.character(data[["med_dose_unit"]]))

  for (medication_name in names(column_config)) {
    unit_configs <- column_config[[medication_name]]
    if (!is.list(unit_configs)) {
      next
    }
    for (unit_name in names(unit_configs)) {
      range_config <- unit_configs[[unit_name]]
      if (!is_outlier_range(range_config)) {
        next
      }
      combination_matches <-
        !is.na(medication_categories) & medication_categories == tolower(medication_name) &
        !is.na(dose_units) & dose_units == tolower(unit_name)
      values_to_null <- combination_matches &
        outlier_mask(numeric_values, range_config$min, range_config$max)
      data[["med_dose"]][values_to_null] <- NA
    }
  }

  data
}

# Numeric view of a configured column. Non-numeric columns are skipped with a
# warning rather than coerced, so a text column is never silently rewritten.
as_outlier_numeric <- function(column_values, column_name) {
  if (is.numeric(column_values)) {
    return(column_values)
  }
  cli::cli_alert_warning(
    "Column {.field {column_name}} is not numeric; skipping outlier handling for it."
  )
  NULL
}

#' Summarise potential outliers without modifying the data
#'
#' Non-mutating counterpart to [apply_outlier_handling()]. Reports, per
#' configured range, how many non-missing values fall below the minimum and above
#' the maximum.
#'
#' @param table_obj A clifR table object with `$df`, `$table_name` and `$schema`.
#' @param outlier_config_path Optional path to a custom outlier configuration
#'   YAML. When `NULL`, the packaged CLIF standard configuration is used.
#'
#' @return A named list with elements `table_name`, `total_rows`, `config_source`
#'   (`"CLIF standard"` or `"Custom"`) and `outliers` — a list of records, one per
#'   range that has at least one violation, each with `column`, `min_expected`,
#'   `max_expected`, `total_values`, `below_min_count`, `above_max_count`,
#'   `total_outliers` and the corresponding percentages. Category-scoped records
#'   also carry `category` (and `primary_category` / `secondary_category` for
#'   medication ranges). When the table cannot be summarised, a list with a single
#'   `status` element is returned instead.
#' @export
#'
#' @examples
#' \dontrun{
#' vitals <- clif_table_from_file("vitals", "data/clif", "parquet", clif_version = "3.0")
#' summary_result <- get_outlier_summary(vitals)
#' length(summary_result$outliers)
#' }
get_outlier_summary <- function(table_obj, outlier_config_path = NULL) {
  if (is.null(table_obj$df) || nrow(table_obj$df) == 0) {
    return(list(status = "No data to analyze"))
  }

  outlier_config <- load_outlier_config(outlier_config_path)
  if (is.null(outlier_config)) {
    return(list(status = "Failed to load configuration"))
  }

  if (is.null(table_obj$schema)) {
    return(list(status = "Table schema not available"))
  }

  table_config <- outlier_config$tables[[table_obj$table_name]]
  if (is.null(table_config) || length(table_config) == 0) {
    return(list(
      status = sprintf("No outlier configuration for table: %s", table_obj$table_name)
    ))
  }

  outlier_results <- validate_numeric_ranges_from_config(
    data = table_obj$df,
    table_name = table_obj$table_name,
    schema = table_obj$schema,
    outlier_config = outlier_config
  )

  list(
    table_name = table_obj$table_name,
    total_rows = nrow(table_obj$df),
    config_source = if (is.null(outlier_config_path)) "CLIF standard" else "Custom",
    outliers = outlier_results
  )
}

#' Count out-of-range values for every configured range
#'
#' Walks the outlier configuration for a table and reports each range that is
#' violated. Category columns are discovered from the table schema by matching the
#' config's keys against the values actually present in the data, so the same code
#' handles simple, single-category and category/unit ranges.
#'
#' @param data A data frame.
#' @param table_name snake_case CLIF table name.
#' @param schema Parsed table schema, used to locate category columns.
#' @param outlier_config Parsed outlier configuration.
#'
#' @return A list of outlier records; see [get_outlier_summary()] for their shape.
#' @keywords internal
validate_numeric_ranges_from_config <- function(data, table_name, schema, outlier_config) {
  table_config <- outlier_config$tables[[table_name]]
  if (is.null(table_config) || length(table_config) == 0) {
    return(list())
  }

  category_column_candidates <- intersect(schema_category_columns(schema), names(data))
  outlier_results <- list()

  for (column_name in names(table_config)) {
    if (!column_name %in% names(data)) {
      next
    }
    column_config <- table_config[[column_name]]
    numeric_values <- suppressWarnings(as.numeric(data[[column_name]]))

    if (is_outlier_range(column_config)) {
      record <- summarise_outlier_range(
        numeric_values = numeric_values,
        row_mask = rep(TRUE, length(numeric_values)),
        minimum_value = column_config$min,
        maximum_value = column_config$max,
        record_fields = list(column = column_name)
      )
      if (!is.null(record)) {
        outlier_results[[length(outlier_results) + 1]] <- record
      }
      next
    }

    primary_category_column <- find_matching_category_column(
      data = data,
      config_keys = names(column_config),
      candidate_columns = category_column_candidates
    )
    if (is.null(primary_category_column)) {
      next
    }
    primary_values <- tolower(as.character(data[[primary_category_column]]))

    first_config_entry <- column_config[[1]]

    if (is_outlier_range(first_config_entry)) {
      for (category_name in names(column_config)) {
        range_config <- column_config[[category_name]]
        if (!is_outlier_range(range_config)) {
          next
        }
        record <- summarise_outlier_range(
          numeric_values = numeric_values,
          row_mask = !is.na(primary_values) & primary_values == tolower(category_name),
          minimum_value = range_config$min,
          maximum_value = range_config$max,
          record_fields = list(category = category_name, column = column_name)
        )
        if (!is.null(record)) {
          outlier_results[[length(outlier_results) + 1]] <- record
        }
      }
      next
    }

    secondary_category_column <- find_matching_category_column(
      data = data,
      config_keys = names(first_config_entry),
      candidate_columns = setdiff(category_column_candidates, primary_category_column)
    )
    if (is.null(secondary_category_column)) {
      next
    }
    secondary_values <- tolower(as.character(data[[secondary_category_column]]))

    for (primary_name in names(column_config)) {
      unit_configs <- column_config[[primary_name]]
      if (!is.list(unit_configs)) {
        next
      }
      for (secondary_name in names(unit_configs)) {
        range_config <- unit_configs[[secondary_name]]
        if (!is_outlier_range(range_config)) {
          next
        }
        record <- summarise_outlier_range(
          numeric_values = numeric_values,
          row_mask = !is.na(primary_values) & primary_values == tolower(primary_name) &
            !is.na(secondary_values) & secondary_values == tolower(secondary_name),
          minimum_value = range_config$min,
          maximum_value = range_config$max,
          record_fields = list(
            primary_category = primary_name,
            secondary_category = secondary_name,
            column = column_name
          )
        )
        if (!is.null(record)) {
          outlier_results[[length(outlier_results) + 1]] <- record
        }
      }
    }
  }

  outlier_results
}

# One outlier record, or NULL when the range is not violated.
summarise_outlier_range <- function(numeric_values,
                                    row_mask,
                                    minimum_value,
                                    maximum_value,
                                    record_fields) {
  selected_values <- numeric_values[row_mask & !is.na(numeric_values)]
  if (length(selected_values) == 0) {
    return(NULL)
  }

  below_minimum_count <- sum(selected_values < minimum_value)
  above_maximum_count <- sum(selected_values > maximum_value)
  if (below_minimum_count == 0 && above_maximum_count == 0) {
    return(NULL)
  }

  total_values <- length(selected_values)
  total_outliers <- below_minimum_count + above_maximum_count

  c(
    list(type = "outlier_validation"),
    record_fields,
    list(
      min_expected = minimum_value,
      max_expected = maximum_value,
      total_values = total_values,
      below_min_count = below_minimum_count,
      above_max_count = above_maximum_count,
      total_outliers = total_outliers,
      outlier_percent = round(total_outliers / total_values * 100, 2),
      below_min_percent = round(below_minimum_count / total_values * 100, 2),
      above_max_percent = round(above_maximum_count / total_values * 100, 2),
      status = "warning"
    )
  )
}

# The first candidate category column whose observed values overlap the config's
# keys. Port of clifpy's `_find_category_column`.
find_matching_category_column <- function(data, config_keys, candidate_columns) {
  config_keys_lower <- tolower(config_keys)
  for (candidate_column in candidate_columns) {
    observed_values <- tolower(as.character(unique(data[[candidate_column]])))
    observed_values <- observed_values[!is.na(observed_values)]
    if (length(intersect(config_keys_lower, observed_values)) > 0) {
      return(candidate_column)
    }
  }
  NULL
}
