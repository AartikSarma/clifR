#' CLIF schema registry
#'
#' Version-aware loading of table schema YAML files. Schemas are stored in
#' per-version subdirectories of `inst/schemas`, e.g. `inst/schemas/2.1/patient_schema.yaml`
#' and `inst/schemas/3.0/...`. Shared, version-agnostic configuration files
#' (`validation_rules.yaml`, `outlier_config.yaml`, `wide_tables_config.yaml`) live at
#' the schemas root.
#'
#' This file is the single source of truth for which CLIF versions exist and how a
#' table name maps to a schema file within a version. Both [BaseTable] and the
#' validator load schemas through [load_schema()] rather than re-implementing path
#' logic. Ported from `clifpy/schemas/__init__.py`.
#'
#' @name clif-schemas
NULL

#' Default CLIF version
#'
#' Used when a caller does not specify one. Kept at 2.1 to match clifpy's default.
#' @export
DEFAULT_CLIF_VERSION <- "2.1"

#' CLIF versions for which a schema subdirectory exists
#' @export
SUPPORTED_CLIF_VERSIONS <- c("2.1", "3.0")

# Per-version table-name -> schema-base-name overrides for tables that were renamed
# between versions. The class-derived table_name stays stable; only the on-disk
# schema file differs. The 2.1 ecmo_mcs table was renamed and redesigned as mcs in 3.0.
SCHEMA_NAME_OVERRIDES <- list(
  `3.0` = list(ecmo_mcs = "mcs")
)

schemas_root <- function() {
  system.file("schemas", package = "clifR")
}

validate_clif_version <- function(clif_version) {
  if (!clif_version %in% SUPPORTED_CLIF_VERSIONS) {
    stop(sprintf(
      "Unsupported CLIF version: '%s'. Supported versions are: %s",
      clif_version, paste(SUPPORTED_CLIF_VERSIONS, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' Path to the schema directory for a CLIF version
#'
#' @param clif_version CLIF version string. Defaults to [DEFAULT_CLIF_VERSION].
#' @return Absolute path to the version's schema directory.
#' @export
schema_dir <- function(clif_version = DEFAULT_CLIF_VERSION) {
  validate_clif_version(clif_version)
  file.path(schemas_root(), clif_version)
}

#' Resolve the schema file name for a table
#'
#' Applies any per-version rename overrides.
#'
#' @param table_name snake_case table name, e.g. `"patient"`.
#' @param clif_version CLIF version string.
#' @return Schema file name, e.g. `"patient_schema.yaml"`.
#' @export
resolve_schema_filename <- function(table_name, clif_version = DEFAULT_CLIF_VERSION) {
  validate_clif_version(clif_version)
  version_overrides <- SCHEMA_NAME_OVERRIDES[[clif_version]]
  base_name <- table_name
  if (!is.null(version_overrides) && !is.null(version_overrides[[table_name]])) {
    base_name <- version_overrides[[table_name]]
  }
  paste0(base_name, "_schema.yaml")
}

#' Absolute path to a table's schema file
#'
#' @inheritParams resolve_schema_filename
#' @return Absolute path to the schema YAML file.
#' @export
schema_path <- function(table_name, clif_version = DEFAULT_CLIF_VERSION) {
  file.path(schema_dir(clif_version), resolve_schema_filename(table_name, clif_version))
}

#' Load and parse a table's YAML schema
#'
#' @param table_name snake_case table name, e.g. `"patient"`, `"respiratory_support"`.
#' @param clif_version CLIF version to load. Defaults to [DEFAULT_CLIF_VERSION].
#' @return Parsed schema as a list, or `NULL` if the schema file does not exist.
#' @export
load_schema <- function(table_name, clif_version = DEFAULT_CLIF_VERSION) {
  path <- schema_path(table_name, clif_version)
  if (!file.exists(path)) {
    warning(sprintf("Schema file not found: %s", path), call. = FALSE)
    return(NULL)
  }
  yaml::read_yaml(path)
}

#' Load a shared, version-agnostic configuration file
#'
#' Reads one of the YAML configs living at the schemas root, e.g.
#' `outlier_config.yaml`, `wide_tables_config.yaml`, `validation_rules.yaml`.
#'
#' @param config_name File name of the config, with or without the `.yaml` suffix.
#' @return Parsed config as a list.
#' @export
load_shared_config <- function(config_name) {
  if (!grepl("\\.ya?ml$", config_name)) {
    config_name <- paste0(config_name, ".yaml")
  }
  path <- file.path(schemas_root(), config_name)
  if (!file.exists(path)) {
    stop(sprintf("Shared config file not found: %s", path), call. = FALSE)
  }
  yaml::read_yaml(path)
}

#' Path to a packaged non-schema data resource
#'
#' Resolves files shipped under `inst/extdata`, such as `mdro.yaml` and the
#' comorbidity code definitions.
#'
#' @param ... Path components relative to `inst/extdata`.
#' @return Absolute path to the resource.
#' @export
clif_extdata_path <- function(...) {
  system.file("extdata", ..., package = "clifR")
}
