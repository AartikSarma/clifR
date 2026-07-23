#' CLIF 2.1 to 3.0 directory migration runner
#'
#' Port of `clifpy/utils/migrate_versions_2_1_to_3.py`. Wraps the already-ported
#' value crosswalk ([crosswalk_file_2_1_to_3_0()]) to migrate an entire site data
#' folder in one pass:
#'
#'   * audits every file in the folder (beta / non-beta / missing beta),
#'   * crosswalks the beta tables (standardized VALUES only; nothing else changes),
#'   * logs every non-beta file but does **not** process or write it (so the PHI
#'     variant and scratch files are never written automatically),
#'   * verifies each conversion preserved row count, column set, and distinct ID
#'     counts, reading parquet schema and metadata rather than full data,
#'   * reports timezone changes; a relabel of tz-aware timestamps to UTC (what the
#'     DuckDB backend does, instants preserved) is **not** treated as a failure,
#'   * wraps each table in a try/catch so one bad table never aborts the run.
#'
#' The runner assumes parquet input (verification reads parquet metadata).
#'
#' @name clif-migrate-versions
NULL

# --------------------------------------------------------------------------- #
# Schema / metadata helpers (no full data load)
# --------------------------------------------------------------------------- #

#' Timezone of each timestamp column in a parquet file
#'
#' Reads only the parquet schema. Timezone-naive timestamp columns are recorded
#' with an `NA` timezone, mirroring pyarrow's `tz=None`.
#'
#' @param path Path to a parquet file.
#' @return A named list mapping timestamp column name to its Olson timezone
#'   string, or `NA_character_` for a timezone-naive column.
#' @keywords internal
tz_map <- function(path) {
  reader <- arrow::ParquetFileReader$create(path)
  schema <- reader$GetSchema()

  timezone_by_column <- list()
  for (field_index in seq_len(schema$num_fields)) {
    field <- schema$field(field_index - 1L)
    if (inherits(field$type, "Timestamp")) {
      column_timezone <- field$type$timezone()
      if (is.null(column_timezone) || identical(column_timezone, "")) {
        column_timezone <- NA_character_
      }
      timezone_by_column[[field$name]] <- column_timezone
    }
  }
  timezone_by_column
}

#' Compact set of timezones across a timezone map
#'
#' @param tzmap A named list as returned by [tz_map()].
#' @return A comma-joined, sorted, de-duplicated string of timezones, with
#'   naive columns shown as `"naive"`. `"-"` when there are no timestamp columns.
#' @keywords internal
zones <- function(tzmap) {
  if (length(tzmap) == 0) {
    return("-")
  }
  zone_labels <- vapply(tzmap, function(column_timezone) {
    if (is.null(column_timezone) || is.na(column_timezone)) "naive" else column_timezone
  }, character(1))
  joined <- paste(sort(unique(zone_labels)), collapse = ",")
  if (nchar(joined) == 0) "-" else joined
}

#' Row count, column set, distinct ID counts, and per-column tz of a parquet file
#'
#' Reads the parquet schema and metadata plus one DuckDB `COUNT(DISTINCT ...)`
#' per present ID column; the full table is never loaded.
#'
#' @param path Path to a parquet file.
#' @param id_cols Character vector of ID column names to count distinct values of.
#' @return A named list with `rows` (integer), `cols` (character vector),
#'   `tz` (named list, per [tz_map()]) and `ids` (named list of distinct counts,
#'   one entry per ID column actually present in the file).
#' @keywords internal
summary_parquet_metadata <- function(path, id_cols) {
  reader <- arrow::ParquetFileReader$create(path)
  schema <- reader$GetSchema()
  column_names <- schema$names

  metadata_summary <- list(
    rows = reader$num_rows,
    cols = column_names,
    tz = tz_map(path),
    ids = list()
  )

  present_id_columns <- id_cols[id_cols %in% column_names]
  if (length(present_id_columns) > 0) {
    connection <- duckdb_connect()
    on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
    for (id_column in present_id_columns) {
      distinct_count <- DBI::dbGetQuery(connection, sprintf(
        'SELECT COUNT(DISTINCT "%s") AS n FROM read_parquet(%s)',
        id_column, sql_quote_value(path)
      ))$n
      metadata_summary$ids[[id_column]] <- as.numeric(distinct_count)
    }
  }

  metadata_summary
}

#' Is a timezone-map value the "None"/naive equivalent?
#'
#' Mirrors Python where both a missing key (`dict.get` returns `None`) and a
#' timezone-naive column (value `None`) read as `None`.
#'
#' @param value A timezone-map value, or `NULL` for an absent key.
#' @return `TRUE` when the value is `NULL` or `NA`.
#' @keywords internal
tz_value_is_none <- function(value) {
  is.null(value) || (length(value) == 1 && is.na(value))
}

#' Describe the timezone change between two timezone maps
#'
#' A plain relabel of a tz-aware column to UTC is what the DuckDB backend does
#' and is instant-preserving, so it is **not** a concern. A real zone shift, or a
#' tz-aware column becoming naive, **is** a concern worth investigating.
#'
#' @param src_tz Source timezone map ([tz_map()] of the input).
#' @param dst_tz Destination timezone map ([tz_map()] of the output).
#' @return A named list with `description` (character) and `is_concern` (logical).
#' @keywords internal
tz_status <- function(src_tz, dst_tz) {
  if (tz_maps_equal(src_tz, dst_tz)) {
    return(list(description = "match", is_concern = FALSE))
  }

  changed_columns <- union(names(src_tz), names(dst_tz))
  differences <- list()
  for (column_name in changed_columns) {
    old_timezone <- if (column_name %in% names(src_tz)) src_tz[[column_name]] else NULL
    new_timezone <- if (column_name %in% names(dst_tz)) dst_tz[[column_name]] else NULL
    if (!tz_values_equal(old_timezone, new_timezone)) {
      differences[[column_name]] <- list(old = old_timezone, new = new_timezone)
    }
  }

  relabel_only <- length(differences) > 0 && all(vapply(differences, function(difference) {
    new_is_utc <- !tz_value_is_none(difference$new) && identical(difference$new, "UTC")
    old_is_not_none <- !tz_value_is_none(difference$old)
    new_is_utc && old_is_not_none
  }, logical(1)))

  if (relabel_only) {
    return(list(description = "relabel->UTC (instants preserved)", is_concern = FALSE))
  }

  difference_descriptions <- vapply(names(differences), function(column_name) {
    difference <- differences[[column_name]]
    format_timezone <- function(value) if (tz_value_is_none(value)) "naive" else value
    sprintf("%s: %s->%s", column_name, format_timezone(difference$old), format_timezone(difference$new))
  }, character(1))
  list(
    description = sprintf("CHANGED {%s}", paste(difference_descriptions, collapse = ", ")),
    is_concern = TRUE
  )
}

#' Equality of two timezone-map values under Python `None` semantics
#'
#' @param a,b Timezone-map values, or `NULL` for absent.
#' @return `TRUE` when both are `None`-equivalent, or both equal strings.
#' @keywords internal
tz_values_equal <- function(a, b) {
  a_is_none <- tz_value_is_none(a)
  b_is_none <- tz_value_is_none(b)
  if (a_is_none || b_is_none) {
    return(a_is_none && b_is_none)
  }
  identical(a, b)
}

#' Dictionary equality of two timezone maps
#'
#' Matches Python `dict` equality: same set of keys, and equal value per key
#' (a naive column present with `NA` differs from an absent column).
#'
#' @param src_tz,dst_tz Timezone maps ([tz_map()]).
#' @return `TRUE` when the maps have the same keys and values.
#' @keywords internal
tz_maps_equal <- function(src_tz, dst_tz) {
  if (!setequal(names(src_tz), names(dst_tz))) {
    return(FALSE)
  }
  for (column_name in names(src_tz)) {
    if (!tz_values_equal(src_tz[[column_name]], dst_tz[[column_name]])) {
      return(FALSE)
    }
  }
  TRUE
}

# Aligned table layout for the per-table result lines (header + rows share it).
MIGRATE_ROW_FORMAT <- "%-30s  %-9s  %-8s  %16s  %-9s  %s"

#' Thousands-separated count, or `a->b` when the two differ
#'
#' @param before,after Integer-valued counts.
#' @return `"1,234"` when equal, else `"1,234->5,678"`.
#' @keywords internal
format_count_pair <- function(before, after) {
  format_with_separators <- function(value) format(value, big.mark = ",", trim = TRUE, scientific = FALSE)
  if (isTRUE(before == after)) {
    format_with_separators(before)
  } else {
    sprintf("%s->%s", format_with_separators(before), format_with_separators(after))
  }
}

#' Migrate a directory of CLIF 2.1 tables to 3.0 value conventions
#'
#' R6 port of clifpy's `CrosswalkMigrationRunner`. Migrates every beta table in a
#' data folder by calling [crosswalk_file_2_1_to_3_0()] and verifies row count,
#' column set, and distinct ID counts before and after each conversion, reporting
#' timezone changes via [tz_status()].
#'
#' @export
#' @importFrom R6 R6Class
CrosswalkMigrationRunner <- R6::R6Class(
  classname = "CrosswalkMigrationRunner",
  public = list(
    #' @field config_path Optional path to a JSON or YAML config file.
    config_path = NULL,
    #' @field data_dir Directory holding the CLIF 2.1 input tables.
    data_dir = NULL,
    #' @field output_dir Directory the migrated CLIF 3.0 tables are written to.
    output_dir = NULL,
    #' @field filetype Data file type. Only `"parquet"` is verified.
    filetype = NULL,
    #' @field log_dir Directory the run log is written to.
    log_dir = NULL,
    #' @field log_path Path of the timestamped run log file.
    log_path = NULL,

    #' @description Construct a migration runner.
    #'
    #' Resolves the data and output directories either from a config file (via a
    #' [ClifOrchestrator], which also carries the filetype) or from explicit
    #' `data_dir` / `output_dir`, then sets up a timestamped run log.
    #'
    #' @param config_path Optional path to a JSON or YAML config file. When given
    #'   and both directories are not, the directories and filetype come from the
    #'   config.
    #' @param data_dir Directory containing the CLIF 2.1 input tables.
    #' @param output_dir Directory to write the migrated CLIF 3.0 tables into.
    #' @param filetype Data file type, defaults to `"parquet"`.
    #' @param log_dir Directory for the run log. Defaults to `<output_dir>/logs`.
    #' @return A new `CrosswalkMigrationRunner`.
    initialize = function(config_path = NULL,
                          data_dir = NULL,
                          output_dir = NULL,
                          filetype = "parquet",
                          log_dir = NULL) {
      self$config_path <- config_path
      self$data_dir <- data_dir
      self$output_dir <- output_dir
      self$filetype <- filetype
      self$log_dir <- log_dir
      private$resolve_paths()
      private$setup_logging()
      invisible(self)
    },

    #' @description Bucket every file in the data directory.
    #'
    #' @return A named list with `all_files` (named list of stem to path),
    #'   `beta` (beta tables present), `non_beta` (present tables that are not
    #'   beta; logged but never written) and `missing` (beta tables absent from
    #'   the folder).
    audit = function() {
      matched_paths <- sort(list.files(
        self$data_dir,
        pattern = sprintf("\\.%s$", self$filetype),
        full.names = TRUE
      ))

      all_files <- list()
      for (file_path in matched_paths) {
        stem <- sub(sprintf("\\.%s$", self$filetype), "", basename(file_path))
        stem <- sub("^clif_", "", stem)
        all_files[[stem]] <- file_path
      }

      present_tables <- names(all_files)
      list(
        all_files = all_files,
        beta = present_tables[present_tables %in% BETA_TABLES],
        non_beta = present_tables[!present_tables %in% BETA_TABLES],
        missing = BETA_TABLES[!BETA_TABLES %in% present_tables]
      )
    },

    #' @description Run the migration.
    #'
    #' @param dry_run When `TRUE`, audit only: report what would change and write
    #'   nothing.
    #' @return `TRUE` on success (no failures and no integrity mismatches).
    #'   `is_complete = FALSE` is not a failure: it means some values need manual
    #'   mapping and are reported, not lost.
    run = function(dry_run = FALSE) {
      audit_result <- self$audit()
      all_files <- audit_result$all_files
      beta_tables <- audit_result$beta

      private$log("INFO", "CLIF 2.1 -> 3.0 migration starting")
      private$log("INFO", sprintf("Data dir : %s", normalizePath(self$data_dir, mustWork = FALSE)))
      private$log("INFO", sprintf("Output   : %s", normalizePath(self$output_dir, mustWork = FALSE)))
      private$log("INFO", sprintf("Crosswalk (beta tables present)       : %s", private$format_list(beta_tables)))
      private$log("INFO", sprintf("Not beta tables (skipped, NOT written): %s", private$format_list(audit_result$non_beta)))
      private$log("INFO", sprintf("Beta tables MISSING from this folder  : %s", private$format_list(audit_result$missing)))
      private$log("INFO", strrep("-", 90))

      if (length(all_files) == 0) {
        private$log("WARN", sprintf(
          "No '*.%s' files found in %s -- nothing to do.", self$filetype, self$data_dir
        ))
        return(TRUE)
      }
      if (dry_run) {
        private$log("INFO", "dry-run: audit only, no files written.")
        return(TRUE)
      }

      private$counts <- list(converted = 0L, mismatch = 0L, skipped = 0L, failed = 0L, incomplete = 0L)
      private$results <- list()

      private$log("INFO", sprintf(
        MIGRATE_ROW_FORMAT, "table", "check", "values", "rows", "tz", "ids"
      ))
      private$log("INFO", sprintf(
        MIGRATE_ROW_FORMAT, strrep("-", 30), strrep("-", 9), strrep("-", 8),
        strrep("-", 16), strrep("-", 9), strrep("-", 20)
      ))

      for (table_name in beta_tables) {
        private$crosswalk_one(table_name, all_files[[table_name]])
      }

      private$report_incomplete()
      private$summarize()
      private$counts$failed == 0L && private$counts$mismatch == 0L
    }
  ),

  private = list(
    id_cols = c("patient_id", "hospitalization_id"),
    counts = NULL,
    results = NULL,

    resolve_paths = function() {
      if (!is.null(self$config_path) && !(!is.null(self$data_dir) && !is.null(self$output_dir))) {
        # The orchestrator reads the config, giving the same data/output dirs and
        # filetype clifpy would resolve.
        orchestrator <- ClifOrchestrator$new(config_path = self$config_path)
        self$data_dir <- self$data_dir %||% orchestrator$data_directory
        self$output_dir <- self$output_dir %||% orchestrator$output_directory
        self$filetype <- orchestrator$filetype %||% self$filetype
      } else if (is.null(self$data_dir) || is.null(self$output_dir)) {
        cli::cli_abort("Provide {.arg config_path}, or both {.arg data_dir} and {.arg output_dir}.")
      }
      if (!dir.exists(self$output_dir)) {
        dir.create(self$output_dir, recursive = TRUE, showWarnings = FALSE)
      }
    },

    setup_logging = function() {
      self$log_dir <- self$log_dir %||% file.path(self$output_dir, "logs")
      if (!dir.exists(self$log_dir)) {
        dir.create(self$log_dir, recursive = TRUE, showWarnings = FALSE)
      }
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      self$log_path <- file.path(self$log_dir, sprintf("crosswalk_2.1_to_3.0_%s.log", timestamp))
      file.create(self$log_path, showWarnings = FALSE)
    },

    # Emit one line to the console (via cli) and append it to the run log file.
    log = function(level, message) {
      log_line <- sprintf(
        "%s  %-7s  %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, message
      )
      if (!is.null(self$log_path)) {
        cat(log_line, "\n", sep = "", file = self$log_path, append = TRUE)
      }
      switch(level,
        WARN = cli::cli_verbatim(paste0("! ", message)),
        ERROR = cli::cli_verbatim(paste0("x ", message)),
        cli::cli_verbatim(message)
      )
      invisible(NULL)
    },

    format_list = function(values) {
      if (length(values) == 0) {
        return("[]")
      }
      sprintf("[%s]", paste(sprintf("'%s'", values), collapse = ", "))
    },

    crosswalk_one = function(table_name, input_path) {
      output_path <- file.path(self$output_dir, basename(input_path))
      if (file.exists(output_path)) {
        private$log("INFO", sprintf("%-32s SKIP (output already exists)", table_name))
        private$counts$skipped <- private$counts$skipped + 1L
        return(invisible(NULL))
      }

      conversion_error <- tryCatch({
        report <- crosswalk_file_2_1_to_3_0(input_path, output_path, table_name)

        source_summary <- summary_parquet_metadata(input_path, private$id_cols)
        destination_summary <- summary_parquet_metadata(output_path, private$id_cols)

        checks <- list(
          rows = isTRUE(source_summary$rows == destination_summary$rows),
          cols = setequal(source_summary$cols, destination_summary$cols)
        )
        for (id_column in private$id_cols) {
          if (id_column %in% names(source_summary$ids)) {
            checks[[id_column]] <- isTRUE(
              source_summary$ids[[id_column]] == destination_summary$ids[[id_column]]
            )
          }
        }
        integrity_ok <- all(vapply(checks, isTRUE, logical(1)))

        tz_result <- tz_status(source_summary$tz, destination_summary$tz)

        check_label <- if (!integrity_ok) {
          "MISMATCH"
        } else if (tz_result$is_concern) {
          "TZ-WARN"
        } else {
          "OK"
        }
        values_label <- if (report$is_complete) "complete" else "REVIEW"

        private$results[[table_name]] <- list(is_complete = report$is_complete, report = report)

        source_zones <- zones(source_summary$tz)
        destination_zones <- zones(destination_summary$tz)
        timezone_display <- if (identical(source_zones, destination_zones)) {
          source_zones
        } else {
          sprintf("%s->%s", source_zones, destination_zones)
        }

        id_labels <- c(patient_id = "pt", hospitalization_id = "hosp")
        id_display <- character(0)
        for (id_column in names(id_labels)) {
          if (id_column %in% names(source_summary$ids)) {
            id_display <- c(id_display, sprintf(
              "%s=%s", id_labels[[id_column]],
              format_count_pair(source_summary$ids[[id_column]], destination_summary$ids[[id_column]])
            ))
          }
        }

        private$log("INFO", sprintf(
          MIGRATE_ROW_FORMAT, table_name, check_label, values_label,
          format_count_pair(source_summary$rows, destination_summary$rows),
          timezone_display, paste(id_display, collapse = "  ")
        ))

        private$counts$converted <- private$counts$converted + 1L
        if (!integrity_ok) {
          private$counts$mismatch <- private$counts$mismatch + 1L
          failed_checks <- names(checks)[!vapply(checks, isTRUE, logical(1))]
          private$log("ERROR", sprintf(
            "   %s INTEGRITY FAILED -> [%s]", table_name,
            paste(sprintf("'%s'", failed_checks), collapse = ", ")
          ))
        }
        if (tz_result$is_concern) {
          private$log("WARN", sprintf("   %s timezone change: %s", table_name, tz_result$description))
        }
        if (!report$is_complete) {
          private$counts$incomplete <- private$counts$incomplete + 1L
        }
        NULL
      }, error = function(condition) condition)

      if (!is.null(conversion_error)) {
        private$counts$failed <- private$counts$failed + 1L
        private$log("ERROR", sprintf(
          "%-32s FAILED to convert\n%s", table_name, conditionMessage(conversion_error)
        ))
        # Drop any partial output so a rerun retries this table cleanly.
        if (file.exists(output_path)) {
          removed <- suppressWarnings(file.remove(output_path))
          if (!isTRUE(removed)) {
            private$log("ERROR", sprintf("   could not remove partial output %s", output_path))
          }
        }
      }

      invisible(NULL)
    },

    report_incomplete = function() {
      incomplete_tables <- Filter(function(result) !result$is_complete, private$results)
      if (length(incomplete_tables) == 0) {
        return(invisible(NULL))
      }
      private$log("INFO", strrep("-", 90))
      private$log("INFO", "VALUES NEEDING MANUAL MAPPING (is_complete=False) -- left as-is in output:")
      for (table_name in names(incomplete_tables)) {
        columns <- incomplete_tables[[table_name]]$report$columns %||% list()
        for (column_name in names(columns)) {
          column_info <- columns[[column_name]]
          flagged <- c(column_info$ambiguous %||% list(), column_info$unresolved %||% list())
          if (length(flagged) > 0) {
            flagged_originals <- vapply(flagged, function(entry) entry$original %||% "", character(1))
            private$log("INFO", sprintf(
              "   %-28s %-24s [%s]", table_name, column_name,
              paste(sprintf("'%s'", flagged_originals), collapse = ", ")
            ))
          }
        }
      }
      invisible(NULL)
    },

    summarize = function() {
      counts <- private$counts
      private$log("INFO", strrep("=", 90))
      private$log("INFO", sprintf(
        "DONE.  converted=%d  skipped=%d  failed=%d  mismatch=%d  needs-review=%d",
        counts$converted, counts$skipped, counts$failed, counts$mismatch, counts$incomplete
      ))
      private$log("INFO", sprintf(
        "CLIF 3.0 output written to: %s", normalizePath(self$output_dir, mustWork = FALSE)
      ))
      private$log("INFO", sprintf(
        "Run log saved to:           %s", normalizePath(self$log_path, mustWork = FALSE)
      ))
      invisible(NULL)
    }
  )
)

#' Migrate a directory of CLIF 2.1 tables to CLIF 3.0
#'
#' Convenience wrapper that constructs a [CrosswalkMigrationRunner] and runs it.
#' Only the standardized category/group/type **values** of the beta tables are
#' converted; column names, row counts and IDs are preserved, and non-beta files
#' are never written.
#'
#' @param data_dir Directory containing the CLIF 2.1 input tables.
#' @param output_dir Directory to write the migrated CLIF 3.0 tables into.
#' @param config_path Optional path to a JSON or YAML config file. When given, the
#'   directories and filetype may be resolved from it.
#' @param filetype Data file type, defaults to `"parquet"`.
#' @param dry_run When `TRUE`, audit only: report what would change, write nothing.
#'
#' @return `TRUE` on success (no failures and no integrity mismatches), invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' migrate_clif_2_1_to_3_0("data/clif_2.1", "data/clif_3.0")
#' }
migrate_clif_2_1_to_3_0 <- function(data_dir,
                                    output_dir,
                                    config_path = NULL,
                                    filetype = "parquet",
                                    dry_run = FALSE) {
  runner <- CrosswalkMigrationRunner$new(
    config_path = config_path,
    data_dir = data_dir,
    output_dir = output_dir,
    filetype = filetype
  )
  invisible(runner$run(dry_run = dry_run))
}
