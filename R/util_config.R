#' Configuration loading utilities
#'
#' Port of `clifpy/utils/config.py`. Resolves the three settings every CLIF
#' table needs — `data_directory`, `filetype` and `timezone` — from either an
#' explicit set of parameters or a JSON/YAML configuration file, using the same
#' precedence rules as clifpy so both implementations pick the same data.
#'
#' Schema and shared-config loading lives in `R/schemas.R` ([load_schema()],
#' [load_shared_config()]); this file deliberately does not duplicate it.
#'
#' @name clif-config
NULL

# Fields every CLIF configuration file must define.
CONFIG_REQUIRED_FIELDS <- c("data_directory", "filetype", "timezone")

# File names auto-detected in the working directory, in clifpy's order of preference.
CONFIG_AUTODETECT_FILENAMES <- c("config.json", "config.yaml", "config.yml")

CONFIG_SUPPORTED_FILETYPES <- c("csv", "parquet")

#' Lowercased file extension of a path
#'
#' Avoids a dependency on `tools` for a one-line operation.
#'
#' @param path A file path.
#' @return The extension without the leading dot, lowercased, or `""`.
#' @keywords internal
file_extension_of <- function(path) {
  base_name <- basename(path)
  matched <- regmatches(base_name, regexpr("\\.[^.]+$", base_name))
  if (length(matched) == 0) "" else tolower(sub("^\\.", "", matched))
}

#' Signal a missing-configuration-file condition
#'
#' Mirrors Python's `FileNotFoundError`, which [get_config_or_params()] catches
#' separately from validation errors. A dedicated condition class lets the R side
#' make the same distinction.
#'
#' @param message Error message, which may contain cli interpolation.
#' @param .envir Environment in which to evaluate the interpolation. Defaults to
#'   the caller, so messages can reference the caller's local variables.
#' @return Never returns; throws a condition of class `clifr_config_not_found`.
#' @keywords internal
abort_config_not_found <- function(message, .envir = parent.frame()) {
  cli::cli_abort(message, class = "clifr_config_not_found", .envir = .envir)
}

#' Read a configuration file, normalising its field names
#'
#' JSON files are read as-is. YAML files additionally have their `tables_path`
#' key renamed to `data_directory`, which is the name the rest of the package
#' uses. Port of clifpy's `_load_config_file`.
#'
#' @param config_path Path to a `.json`, `.yaml` or `.yml` file.
#' @return A named list of configuration values.
#' @keywords internal
read_config_file <- function(config_path) {
  file_extension <- file_extension_of(config_path)

  configuration <- switch(file_extension,
    json = tryCatch(
      jsonlite::fromJSON(config_path, simplifyVector = TRUE),
      error = function(condition) {
        cli::cli_abort("Invalid JSON in configuration file {.file {config_path}}: {condition$message}")
      }
    ),
    yaml = ,
    yml = tryCatch(
      yaml::read_yaml(config_path),
      error = function(condition) {
        cli::cli_abort("Invalid YAML in configuration file {.file {config_path}}: {condition$message}")
      }
    ),
    cli::cli_abort(c(
      "Unsupported config file format: {.val {file_extension}}",
      "i" = "Supported formats are: .json, .yaml, .yml"
    ))
  )

  configuration <- as.list(configuration)

  if (file_extension %in% c("yaml", "yml") && "tables_path" %in% names(configuration)) {
    configuration$data_directory <- configuration$tables_path
    configuration$tables_path <- NULL
  }

  configuration
}

#' Load a CLIF configuration file
#'
#' Reads a JSON or YAML configuration file and validates that it defines the
#' fields CLIF table loading requires. When `config_path` is `NULL` the working
#' directory is searched for `config.json`, `config.yaml` and `config.yml`, in
#' that order.
#'
#' A YAML `tables_path` key is mapped to `data_directory`, matching clifpy.
#'
#' @param config_path Path to a `.json`, `.yaml` or `.yml` configuration file.
#'   When `NULL`, auto-detects one in the working directory.
#'
#' @return A named list of configuration values. Always contains
#'   `data_directory`, `filetype` and `timezone`; may also contain
#'   `output_directory`, `clif_version`, `site` and any other keys the file
#'   defines.
#' @export
#'
#' @examples
#' \dontrun{
#' configuration <- load_config("config.yaml")
#' configuration$data_directory
#' }
load_config <- function(config_path = NULL) {
  if (is.null(config_path)) {
    working_directory <- getwd()
    for (candidate_filename in CONFIG_AUTODETECT_FILENAMES) {
      candidate_path <- file.path(working_directory, candidate_filename)
      if (file.exists(candidate_path)) {
        config_path <- candidate_path
        break
      }
    }

    if (is.null(config_path)) {
      abort_config_not_found(c(
        "Configuration file not found in {.file {working_directory}}",
        "i" = "Create a config.json or config.yaml file in the working directory",
        "i" = "Or pass {.arg config_path} pointing at your config file",
        "i" = "Or pass {.arg data_directory}, {.arg filetype} and {.arg timezone} directly"
      ))
    }
  }

  if (!file.exists(config_path)) {
    abort_config_not_found(c(
      "Configuration file not found: {.file {config_path}}",
      "i" = "Create a config.json or config.yaml file in the working directory",
      "i" = "Or pass {.arg config_path} pointing at your config file",
      "i" = "Or pass {.arg data_directory}, {.arg filetype} and {.arg timezone} directly"
    ))
  }

  configuration <- read_config_file(config_path)

  missing_fields <- setdiff(CONFIG_REQUIRED_FIELDS, names(configuration))
  if (length(missing_fields) > 0) {
    cli::cli_abort(c(
      "Missing required fields in configuration file {.file {config_path}}: {.field {missing_fields}}",
      "i" = "Required fields are: {.field {CONFIG_REQUIRED_FIELDS}}"
    ))
  }

  configured_data_directory <- configuration$data_directory
  if (!file.exists(configured_data_directory)) {
    cli::cli_abort(c(
      "Data directory specified in config does not exist: {.file {configured_data_directory}}",
      "i" = "Check the {.field data_directory} path in {.file {config_path}}"
    ))
  }

  if (!configuration$filetype %in% CONFIG_SUPPORTED_FILETYPES) {
    cli::cli_abort(c(
      "Unsupported filetype {.val {configuration$filetype}} in {.file {config_path}}",
      "i" = "Supported filetypes are: {.val {CONFIG_SUPPORTED_FILETYPES}}"
    ))
  }

  configuration
}

#' Resolve configuration from parameters or a configuration file
#'
#' Port of clifpy's `get_config_or_params`. Priority, highest first:
#'
#' 1. If `data_directory`, `filetype` and `timezone` are all supplied, they are
#'    used directly and no configuration file is read.
#' 2. Otherwise a configuration file is loaded — from `config_path` when given,
#'    else auto-detected in the working directory.
#' 3. Any explicitly supplied parameter overrides the corresponding file value.
#'
#' Extra keys in the configuration file, notably `clif_version`, are passed
#' through unchanged so callers such as [clif_table_from_file()] can use them.
#'
#' @param config_path Optional path to a JSON or YAML configuration file.
#' @param data_directory Optional directory containing the CLIF data files.
#' @param filetype Optional file type, `"csv"` or `"parquet"`.
#' @param timezone Optional Olson timezone name.
#' @param output_directory Optional directory for logs and outputs.
#'
#' @return A named list with at least `data_directory`, `filetype` and
#'   `timezone`, plus `output_directory` and any other configured keys.
#' @export
#'
#' @examples
#' resolved <- get_config_or_params(
#'   data_directory = tempdir(),
#'   filetype = "parquet",
#'   timezone = "UTC"
#' )
#' resolved$filetype
get_config_or_params <- function(config_path = NULL,
                                 data_directory = NULL,
                                 filetype = NULL,
                                 timezone = NULL,
                                 output_directory = NULL) {
  required_parameters <- list(
    data_directory = data_directory,
    filetype = filetype,
    timezone = timezone
  )
  supplied_parameter_flags <- !vapply(required_parameters, is.null, logical(1))

  if (all(supplied_parameter_flags)) {
    configuration <- list(
      data_directory = data_directory,
      filetype = filetype,
      timezone = timezone
    )
    if (!is.null(output_directory)) {
      configuration$output_directory <- output_directory
    }
    return(configuration)
  }

  configuration <- tryCatch(
    load_config(config_path),
    clifr_config_not_found = function(condition) {
      if (any(supplied_parameter_flags)) {
        missing_parameter_names <- names(required_parameters)[!supplied_parameter_flags]
        cli::cli_abort(c(
          "Incomplete parameters provided. Missing: {.field {missing_parameter_names}}",
          "i" = "Provide all of {.arg data_directory}, {.arg filetype} and {.arg timezone}",
          "i" = "Or create a config.json or config.yaml file",
          "i" = "Or pass a {.arg config_path}"
        ))
      }
      stop(condition)
    }
  )

  if (!is.null(data_directory)) {
    configuration$data_directory <- data_directory
  }
  if (!is.null(filetype)) {
    configuration$filetype <- filetype
  }
  if (!is.null(timezone)) {
    configuration$timezone <- timezone
  }
  if (!is.null(output_directory)) {
    configuration$output_directory <- output_directory
  }

  configuration
}

#' Write an example configuration file
#'
#' Creates a starter JSON or YAML configuration file. The format is taken from
#' the `config_path` extension when it is `.json`, `.yaml` or `.yml`, otherwise
#' from `format`. YAML files use the `tables_path` key name that [load_config()]
#' maps back to `data_directory`.
#'
#' @param data_directory Path to the CLIF data directory.
#' @param filetype File type, `"csv"` or `"parquet"`.
#' @param timezone Olson timezone name.
#' @param output_directory Directory for logs and outputs.
#' @param config_path Where to write the configuration file.
#' @param format Output format, `"json"` or `"yaml"`. Ignored when
#'   `config_path` has a recognised extension.
#'
#' @return The path written, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' create_example_config(data_directory = "./data", config_path = "./config.yaml")
#' }
create_example_config <- function(data_directory = "./data",
                                  filetype = "parquet",
                                  timezone = "UTC",
                                  output_directory = "./output",
                                  config_path = "./config.json",
                                  format = "json") {
  file_extension <- file_extension_of(config_path)
  if (file_extension %in% c("yaml", "yml")) {
    format <- "yaml"
  } else if (file_extension == "json") {
    format <- "json"
  }

  if (tolower(format) == "yaml") {
    example_configuration <- list(
      site = "EXAMPLE_SITE",
      tables_path = data_directory,
      filetype = filetype,
      timezone = timezone,
      output_directory = output_directory,
      clif_version = DEFAULT_CLIF_VERSION
    )
    yaml::write_yaml(example_configuration, config_path, indent = 2)
  } else {
    example_configuration <- list(
      data_directory = data_directory,
      filetype = filetype,
      timezone = timezone,
      output_directory = output_directory,
      clif_version = DEFAULT_CLIF_VERSION
    )
    jsonlite::write_json(example_configuration, config_path, auto_unbox = TRUE, pretty = 2)
  }

  cli::cli_alert_success(
    "Example {toupper(format)} configuration file created at: {.file {config_path}}"
  )
  invisible(config_path)
}

#' Validate that a configuration list has the required fields
#'
#' @param config Named list of configuration values.
#' @param required_fields Character vector of field names that must be present.
#'
#' @return `TRUE`, invisibly, when all fields are present; otherwise an error.
#' @keywords internal
validate_config <- function(config, required_fields) {
  missing_fields <- setdiff(required_fields, names(config))
  if (length(missing_fields) > 0) {
    cli::cli_abort(c(
      "Configuration is missing required fields:",
      "x" = "Missing: {.field {missing_fields}}"
    ))
  }
  invisible(TRUE)
}
