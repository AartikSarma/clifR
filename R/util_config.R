#' Load YAML configuration file
#'
#' @description
#' Load and parse YAML configuration files for CLIF data processing.
#'
#' @param config_path Character. Path to YAML configuration file.
#'
#' @return List containing configuration parameters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_config("path/to/config.yaml")
#' }
load_config <- function(config_path) {
  if (!file.exists(config_path)) {
    cli::cli_abort("Configuration file not found: {.file {config_path}}")
  }

  tryCatch({
    config <- yaml::read_yaml(config_path)
    cli::cli_alert_success("Loaded configuration from {.file {config_path}}")
    return(config)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to parse configuration file",
      "x" = "Error: {e$message}"
    ))
  })
}

#' Load CLIF table schema
#'
#' @description
#' Load YAML schema file for a specific CLIF table.
#'
#' @param table_name Character. Name of the CLIF table (e.g., "patient", "vitals").
#' @param schema_dir Character. Directory containing schema files. If NULL, uses
#'   package default (inst/schemas/).
#'
#' @return List containing schema definition.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' patient_schema <- load_schema("patient")
#' vitals_schema <- load_schema("vitals")
#' }
load_schema <- function(table_name, schema_dir = NULL) {
  if (is.null(schema_dir)) {
    # Use package schema directory
    schema_dir <- system.file("schemas", package = "clifR")
    if (schema_dir == "") {
      # Development mode - schemas not yet installed
      schema_dir <- file.path("inst", "schemas")
    }
  }

  schema_file <- file.path(schema_dir, paste0(table_name, "_schema.yaml"))

  if (!file.exists(schema_file)) {
    cli::cli_abort(c(
      "Schema file not found for table {.val {table_name}}",
      "i" = "Expected location: {.file {schema_file}}"
    ))
  }

  tryCatch({
    schema <- yaml::read_yaml(schema_file)
    cli::cli_alert_info("Loaded schema for {.val {table_name}} table")
    return(schema)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to parse schema file for table {.val {table_name}}",
      "x" = "Error: {e$message}"
    ))
  })
}

#' Load outlier configuration
#'
#' @description
#' Load outlier detection configuration from YAML file.
#'
#' @param config_path Character. Path to outlier config file. If NULL, uses
#'   package default.
#'
#' @return List containing outlier thresholds and rules.
#'
#' @export
load_outlier_config <- function(config_path = NULL) {
  if (is.null(config_path)) {
    config_dir <- system.file("schemas", package = "clifR")
    if (config_dir == "") {
      config_dir <- file.path("inst", "schemas")
    }
    config_path <- file.path(config_dir, "outlier_config.yaml")
  }

  if (!file.exists(config_path)) {
    cli::cli_abort("Outlier configuration file not found: {.file {config_path}}")
  }

  config <- yaml::read_yaml(config_path)
  cli::cli_alert_info("Loaded outlier configuration")
  return(config)
}

#' Load wide table configuration
#'
#' @description
#' Load configuration for wide dataset transformation.
#'
#' @param config_path Character. Path to wide tables config file. If NULL, uses
#'   package default.
#'
#' @return List containing wide table transformation rules.
#'
#' @export
load_wide_config <- function(config_path = NULL) {
  if (is.null(config_path)) {
    config_dir <- system.file("schemas", package = "clifR")
    if (config_dir == "") {
      config_dir <- file.path("inst", "schemas")
    }
    config_path <- file.path(config_dir, "wide_tables_config.yaml")
  }

  if (!file.exists(config_path)) {
    cli::cli_abort("Wide tables configuration file not found: {.file {config_path}}")
  }

  config <- yaml::read_yaml(config_path)
  cli::cli_alert_info("Loaded wide tables configuration")
  return(config)
}

#' Validate configuration structure
#'
#' @description
#' Validate that a configuration object has required fields.
#'
#' @param config List. Configuration object to validate.
#' @param required_fields Character vector. Names of required fields.
#'
#' @return Logical. TRUE if valid, otherwise throws error.
#'
#' @keywords internal
validate_config <- function(config, required_fields) {
  missing_fields <- setdiff(required_fields, names(config))

  if (length(missing_fields) > 0) {
    cli::cli_abort(c(
      "Configuration is missing required fields:",
      "x" = "Missing: {.field {missing_fields}}"
    ))
  }

  return(TRUE)
}
