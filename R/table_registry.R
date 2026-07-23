#' CLIF table registry
#'
#' Single source of truth mapping snake_case CLIF table names to their R6 generator
#' objects. The orchestrator, the batch loaders and the validation helpers all read
#' from here, so adding a table means editing one list rather than a hardcoded vector
#' repeated across several methods. Mirrors `TABLE_CLASSES` in
#' `clifpy/clif_orchestrator.py`.
#'
#' @name clif-table-registry
NULL

#' Names of the CLIF tables clifR implements
#'
#' Ordered to match clifpy's `TABLE_CLASSES` so that any iteration over tables
#' produces the same sequence in both languages.
#'
#' @export
CLIF_TABLE_NAMES <- c(
  "patient",
  "hospitalization",
  "adt",
  "labs",
  "vitals",
  "medication_admin_continuous",
  "medication_admin_intermittent",
  "patient_assessments",
  "respiratory_support",
  "position",
  "hospital_diagnosis",
  "microbiology_culture",
  "crrt_therapy",
  "patient_procedures",
  "microbiology_susceptibility",
  "ecmo_mcs",
  "microbiology_nonculture",
  "code_status"
)

#' Map a CLIF table name to its R6 class generator
#'
#' Resolved lazily by name so the registry does not depend on class definition
#' order at package build time.
#'
#' @param table_name snake_case CLIF table name.
#' @return The R6 generator object for that table.
#' @export
get_table_class <- function(table_name) {
  if (!table_name %in% CLIF_TABLE_NAMES) {
    cli::cli_abort(c(
      "Unknown CLIF table: {.val {table_name}}",
      "i" = "Supported tables: {.val {CLIF_TABLE_NAMES}}"
    ))
  }
  class_name <- snake_to_pascal_case(table_name)
  get(class_name, envir = asNamespace("clifR"))
}

#' Convert a snake_case name to PascalCase
#'
#' @param snake_case_name e.g. `"respiratory_support"`.
#' @return e.g. `"RespiratorySupport"`.
#' @keywords internal
snake_to_pascal_case <- function(snake_case_name) {
  name_parts <- strsplit(snake_case_name, "_", fixed = TRUE)[[1]]
  paste0(toupper(substring(name_parts, 1, 1)), substring(name_parts, 2), collapse = "")
}

#' Convert a PascalCase name to snake_case
#'
#' Used by [BaseTable] to derive its `table_name` from the class name, matching
#' clifpy's constructor behaviour.
#'
#' @param pascal_case_name e.g. `"RespiratorySupport"`.
#' @return e.g. `"respiratory_support"`.
#' @keywords internal
pascal_to_snake_case <- function(pascal_case_name) {
  with_underscores <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", pascal_case_name)
  tolower(with_underscores)
}
