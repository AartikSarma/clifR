#' Bundled CLIF demo data
#'
#' Small sample CLIF dataset shipped with the package for testing, examples and
#' documentation, in the spirit of the toy datasets bundled with modelling packages.
#' Ported from `clifpy/data/loader.py`; the underlying parquet files are the same
#' ones clifpy ships.
#'
#' @name clif-demo-data
NULL

#' Tables available in the bundled demo dataset
#' @export
DEMO_TABLES <- c(
  "patient",
  "hospitalization",
  "adt",
  "labs",
  "vitals",
  "respiratory_support",
  "position",
  "medication_admin_continuous",
  "patient_assessments"
)

#' Directory holding the bundled demo parquet files
#'
#' @return Absolute path to the demo data directory.
#' @keywords internal
demo_data_path <- function() {
  clif_extdata_path("clif_demo")
}

#' Load one demo table
#'
#' @param table_name snake_case CLIF table name.
#' @param return_raw When `TRUE`, return the raw tibble instead of a table object.
#' @param timezone Olson timezone for datetime columns.
#' @return A table object, or a tibble when `return_raw` is `TRUE`.
#' @keywords internal
load_demo_table <- function(table_name, return_raw = FALSE, timezone = "UTC") {
  demo_directory <- demo_data_path()
  file_path <- file.path(demo_directory, paste0("clif_", table_name, ".parquet"))

  if (!file.exists(file_path)) {
    cli::cli_abort("Demo data file not found: {.file {file_path}}")
  }

  demo_frame <- load_data(
    table_name = table_name,
    table_path = demo_directory,
    table_format_type = "parquet",
    site_tz = timezone
  )

  if (return_raw) {
    return(demo_frame)
  }

  table_generator <- get_table_class(table_name)
  table_generator$new(
    data_directory = demo_directory,
    filetype = "parquet",
    timezone = timezone,
    data = demo_frame
  )
}

#' Load an orchestrator populated with demo data
#'
#' @param tables Character vector of tables to load. Defaults to all of [DEMO_TABLES].
#' @param timezone Olson timezone for datetime columns.
#' @param verbose Whether to report which tables were loaded.
#'
#' @return A [ClifOrchestrator] with the requested demo tables loaded.
#' @export
#'
#' @examples
#' demo_orchestrator <- load_demo_clif(tables = c("patient", "vitals"))
#' demo_orchestrator$get_loaded_tables()
load_demo_clif <- function(tables = NULL, timezone = "UTC", verbose = FALSE) {
  if (is.null(tables)) {
    tables_to_load <- DEMO_TABLES
  } else {
    unknown_tables <- setdiff(tables, DEMO_TABLES)
    if (length(unknown_tables) > 0) {
      cli::cli_abort(c(
        "Unknown demo table{?s}: {.val {unknown_tables}}",
        "i" = "Available demo tables: {.val {sort(DEMO_TABLES)}}"
      ))
    }
    tables_to_load <- unique(tables)
  }

  orchestrator <- ClifOrchestrator$new(
    data_directory = demo_data_path(),
    filetype = "parquet",
    timezone = timezone
  )

  if (verbose) {
    cli::cli_alert_info("Loading demo CLIF tables: {.val {tables_to_load}}")
  }

  orchestrator$initialize_tables(tables = tables_to_load)

  if (verbose) {
    cli::cli_alert_success("Loaded tables: {.val {orchestrator$get_loaded_tables()}}")
  }

  orchestrator
}

#' @rdname clif-demo-data
#' @param return_raw When `TRUE`, return the raw tibble instead of a table object.
#' @return A table object, or a tibble when `return_raw` is `TRUE`.
#' @export
#' @examples
#' patient_table <- load_demo_patient()
load_demo_patient <- function(return_raw = FALSE) load_demo_table("patient", return_raw)

#' @rdname clif-demo-data
#' @export
load_demo_labs <- function(return_raw = FALSE) load_demo_table("labs", return_raw)

#' @rdname clif-demo-data
#' @export
load_demo_vitals <- function(return_raw = FALSE) load_demo_table("vitals", return_raw)

#' @rdname clif-demo-data
#' @export
load_demo_respiratory_support <- function(return_raw = FALSE) {
  load_demo_table("respiratory_support", return_raw)
}

#' @rdname clif-demo-data
#' @export
load_demo_position <- function(return_raw = FALSE) load_demo_table("position", return_raw)

#' @rdname clif-demo-data
#' @export
load_demo_adt <- function(return_raw = FALSE) load_demo_table("adt", return_raw)

#' @rdname clif-demo-data
#' @export
load_demo_hospitalization <- function(return_raw = FALSE) {
  load_demo_table("hospitalization", return_raw)
}

#' @rdname clif-demo-data
#' @export
load_demo_medication_admin_continuous <- function(return_raw = FALSE) {
  load_demo_table("medication_admin_continuous", return_raw)
}

#' @rdname clif-demo-data
#' @export
load_demo_patient_assessments <- function(return_raw = FALSE) {
  load_demo_table("patient_assessments", return_raw)
}

#' Describe the bundled demo datasets
#'
#' @return A tibble with one row per demo table giving row and column counts and
#'   the on-disk file size.
#' @export
#'
#' @examples
#' list_demo_datasets()
list_demo_datasets <- function() {
  demo_directory <- demo_data_path()

  dplyr::bind_rows(lapply(DEMO_TABLES, function(table_name) {
    file_path <- file.path(demo_directory, paste0("clif_", table_name, ".parquet"))
    if (!file.exists(file_path)) {
      return(NULL)
    }
    demo_frame <- arrow::read_parquet(file_path)
    dplyr::tibble(
      table_name = table_name,
      n_rows = nrow(demo_frame),
      n_columns = ncol(demo_frame),
      file_size = get_file_size(file_path)
    )
  }))
}

#' Print a summary of the bundled demo datasets
#'
#' @return The summary tibble, invisibly.
#' @export
#'
#' @examples
#' get_demo_summary()
get_demo_summary <- function() {
  demo_summary <- list_demo_datasets()

  cli::cli_h2("clifR demo dataset")
  for (row_index in seq_len(nrow(demo_summary))) {
    summary_row <- demo_summary[row_index, ]
    cli::cli_li(
      "{summary_row$table_name}: {.val {summary_row$n_rows}} rows x {.val {summary_row$n_columns}} columns ({summary_row$file_size})"
    )
  }

  invisible(demo_summary)
}
