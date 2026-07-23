#' Labs table
#'
#' @description
#' R6 class for the CLIF `labs` table. Inherits all loading, validation and summary
#' behaviour from [BaseTable] and adds reference-unit inspection and
#' standardization, plus per-category and per-specimen summary statistics.
#' Port of `clifpy.tables.labs.Labs`.
#'
#' @export
#' @examples
#' \dontrun{
#' labs_table <- Labs$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' labs_table$get_lab_reference_units()
#' labs_table$standardize_reference_units(inplace = TRUE)
#' }
Labs <- R6::R6Class(
  classname = "Labs",
  inherit = BaseTable,
  public = list(
    #' @description Create a Labs table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `Labs` instance.
    initialize = function(data_directory = NULL,
                          filetype = NULL,
                          timezone = "UTC",
                          output_directory = NULL,
                          data = NULL,
                          clif_version = DEFAULT_CLIF_VERSION) {
      super$initialize(
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory,
        data = data,
        clif_version = clif_version
      )
      private$load_labs_schema_data()
    },

    #' @description Unique reference units observed in the data, by lab category.
    #' @param save If `TRUE`, write the result to `lab_reference_units.csv`.
    #' @param output_directory Directory for the CSV. When `NULL`, uses the table's
    #'   `output_directory`.
    #' @return A tibble with columns `lab_category`, `reference_unit` and `count`.
    get_lab_reference_units = function(save = FALSE, output_directory = NULL) {
      if (is.null(self$df)) {
        cli::cli_abort("No data")
      }
      missing_columns <- setdiff(c("lab_category", "reference_unit"), names(self$df))
      if (length(missing_columns) > 0) {
        cli::cli_alert_warning(
          "Missing columns: {.val {missing_columns}} - cannot compute reference units"
        )
        return(dplyr::tibble(
          lab_category = character(0),
          reference_unit = character(0),
          count = integer(0)
        ))
      }

      # clifpy's primary (polars) path keeps NULL groups and sorts nulls first.
      reference_unit_counts <- self$df |>
        dplyr::count(.data$lab_category, .data$reference_unit, name = "count") |>
        dplyr::arrange(
          dplyr::desc(is.na(.data$lab_category)),
          .data$lab_category,
          dplyr::desc(is.na(.data$reference_unit)),
          .data$reference_unit
        )

      if (save) {
        save_directory <- output_directory %||% self$output_directory
        if (!dir.exists(save_directory)) {
          dir.create(save_directory, recursive = TRUE, showWarnings = FALSE)
        }
        csv_path <- file.path(save_directory, "lab_reference_units.csv")
        readr::write_csv(reference_unit_counts, csv_path)
        cli::cli_alert_info("Saved lab reference units to {.file {csv_path}}")
      }

      reference_unit_counts
    },

    #' @description Standardize reference unit strings to the schema's target units.
    #'
    #' Uses fuzzy matching to detect variant spellings (e.g. `'mmhg'` vs `'mmHg'`,
    #' `'10*3/ul'` vs `'10^3/uL'`) and rewrites them to the canonical unit defined
    #' in the schema. This does **not** convert values between different unit
    #' systems; unmatched units are reported as warnings.
    #'
    #' @param inplace If `TRUE`, modify `$df` in place and return `NULL` invisibly.
    #' @param save If `TRUE`, write the applied mappings to
    #'   `lab_reference_unit_standardized.csv`.
    #' @param lowercase If `TRUE`, emit all reference units in lowercase instead of
    #'   the schema's original casing.
    #' @param output_directory Directory for the CSV. When `NULL`, uses the table's
    #'   `output_directory`.
    #' @return The standardized tibble when `inplace = FALSE`, otherwise `NULL`
    #'   invisibly.
    standardize_reference_units = function(inplace = TRUE,
                                           save = FALSE,
                                           lowercase = FALSE,
                                           output_directory = NULL) {
      if (is.null(self$df)) {
        cli::cli_abort(c(
          "No data loaded. Please provide data using one of these methods:",
          "1" = "clif_table_from_file('labs', data_directory = ..., filetype = ..., timezone = ...)",
          "2" = "Labs$new(data = your_dataframe)"
        ))
      }
      missing_columns <- setdiff(c("lab_category", "reference_unit"), names(self$df))
      if (length(missing_columns) > 0) {
        cli::cli_abort("Required columns not found: {.val {missing_columns}}")
      }
      if (length(private$lab_reference_units_map) == 0) {
        cli::cli_alert_warning("No lab reference units defined in schema")
        return(invisible(NULL))
      }

      unique_combinations <- dplyr::distinct(
        self$df[, c("lab_category", "reference_unit")]
      )
      mapping_result <- private$build_unit_mapping(unique_combinations, lowercase)
      unit_mapping <- mapping_result$unit_mapping
      mappings_applied <- mapping_result$mappings_applied
      unmatched_units <- mapping_result$unmatched_units

      standardized_data <- self$df
      if (nrow(unit_mapping) > 0) {
        join_frame <- unit_mapping |>
          dplyr::rename(
            reference_unit = "source_unit",
            standardized_target_unit = "target_unit"
          )
        standardized_data <- standardized_data |>
          dplyr::left_join(join_frame, by = c("lab_category", "reference_unit")) |>
          dplyr::mutate(
            reference_unit = dplyr::coalesce(
              .data$standardized_target_unit,
              .data$reference_unit
            )
          ) |>
          dplyr::select(-"standardized_target_unit")
      }
      if (lowercase) {
        standardized_data$reference_unit <- tolower(standardized_data$reference_unit)
      }

      if (inplace) {
        self$df <- standardized_data
      }

      if (nrow(unit_mapping) > 0) {
        non_silent_count <- sum(!mappings_applied$silent)
        if (non_silent_count > 0) {
          cli::cli_alert_info("Applied {.val {non_silent_count}} unit standardizations")
        }
      } else if (!lowercase) {
        cli::cli_alert_info("No unit standardizations needed")
      }

      for (unmatched_record in unmatched_units) {
        cli::cli_alert_warning(
          "Unmatched unit {.val {unmatched_record$source_unit}} for {.val {unmatched_record$lab_category}}. Expected one of: {.val {unmatched_record$expected_units}}"
        )
      }

      if (save && nrow(mappings_applied) > 0) {
        save_directory <- output_directory %||% self$output_directory
        if (!dir.exists(save_directory)) {
          dir.create(save_directory, recursive = TRUE, showWarnings = FALSE)
        }
        csv_path <- file.path(save_directory, "lab_reference_unit_standardized.csv")
        readr::write_csv(mappings_applied, csv_path)
        cli::cli_alert_info("Saved unit mappings to {.file {csv_path}}")
      }

      if (!inplace) {
        return(standardized_data)
      }
      invisible(NULL)
    },

    #' @description Summary statistics for each lab category.
    #' @return A tibble with one row per `lab_category` and columns `count`,
    #'   `unique` (distinct hospitalizations), `missing_pct`, `mean`, `std`, `min`,
    #'   `q1`, `median`, `q3` and `max`, all rounded to 2 decimals. A named list
    #'   `list(status = "Missing columns")` when required columns are absent.
    get_lab_category_stats = function() {
      if (is.null(self$df) ||
          !"lab_value_numeric" %in% names(self$df) ||
          !"hospitalization_id" %in% names(self$df)) {
        return(list(status = "Missing columns"))
      }
      private$grouped_lab_value_stats("lab_category")
    },

    #' @description Summary statistics for each lab specimen category.
    #'
    #' @note clifpy 0.5.0's `get_lab_specimen_stats` guards on a misspelled column
    #' name (`lab_speciment_category`) but then groups by the correctly spelled
    #' `lab_specimen_category`, so with correctly named data the Python method
    #' always returns `{"status": "Missing columns"}`. clifR implements the correct
    #' spelling (`lab_specimen_category`) in both the guard and the grouping; this
    #' is an intentional divergence from the upstream bug.
    #'
    #' @return A tibble with one row per `lab_specimen_category` and the same
    #'   statistic columns as `get_lab_category_stats()`. A named list
    #'   `list(status = "Missing columns")` when required columns are absent.
    get_lab_specimen_stats = function() {
      if (is.null(self$df) ||
          !"lab_value_numeric" %in% names(self$df) ||
          !"hospitalization_id" %in% names(self$df) ||
          !"lab_specimen_category" %in% names(self$df)) {
        return(list(status = "Missing columns"))
      }
      private$grouped_lab_value_stats("lab_specimen_category")
    }
  ),
  private = list(
    lab_reference_units_map = list(),
    allowed_unit_variants_map = list(),

    load_labs_schema_data = function() {
      if (is.null(self$schema)) {
        return(invisible(NULL))
      }
      private$lab_reference_units_map <- self$schema$lab_reference_units %||% list()
      variants_map <- self$schema$allowed_unit_variants %||% list()
      if (length(variants_map) > 0) {
        names(variants_map) <- tolower(trimws(names(variants_map)))
      }
      private$allowed_unit_variants_map <- variants_map
      invisible(NULL)
    },

    # Returns list(canonical = <chr>, accepted = <chr vector>) for one
    # lab_reference_units entry, expanding canonical strings through
    # allowed_unit_variants (canonical itself always listed first).
    resolve_target_units = function(reference_unit_entry) {
      if (is.character(reference_unit_entry) && length(reference_unit_entry) == 1) {
        canonical_unit <- reference_unit_entry
        variant_key <- tolower(trimws(canonical_unit))
        variant_spellings <- private$allowed_unit_variants_map[[variant_key]]
        if (!is.null(variant_spellings) && length(variant_spellings) > 0) {
          accepted_units <- unique(c(
            canonical_unit,
            unlist(variant_spellings, use.names = FALSE)
          ))
        } else {
          accepted_units <- canonical_unit
        }
        return(list(canonical = canonical_unit, accepted = accepted_units))
      }
      if ((is.list(reference_unit_entry) || is.character(reference_unit_entry)) &&
          length(reference_unit_entry) > 0) {
        accepted_units <- unlist(reference_unit_entry, use.names = FALSE)
        return(list(canonical = accepted_units[1], accepted = accepted_units))
      }
      list(canonical = "", accepted = character(0))
    },

    # Best matching target unit for a source unit, using normalized comparison.
    # Exact matches are returned unchanged; normalized matches return `preferred`.
    find_matching_target_unit = function(source_unit, target_units, preferred = NULL) {
      if (is.na(source_unit) || !nzchar(source_unit) || length(target_units) == 0) {
        return(NULL)
      }
      if (is.null(preferred)) {
        preferred <- target_units[1]
      }
      if (source_unit %in% target_units) {
        return(source_unit)
      }
      normalized_source <- normalize_lab_unit(source_unit)
      for (target_unit in target_units) {
        if (normalize_lab_unit(target_unit) == normalized_source) {
          return(preferred)
        }
      }
      NULL
    },

    # Builds the (lab_category, source_unit) -> target_unit mapping from the
    # unique combinations present in the data. Returns unit_mapping (only rows
    # where the target differs from the source), mappings_applied (with a
    # `silent` flag for cosmetic-only changes) and unmatched_units.
    build_unit_mapping = function(unique_combinations, lowercase) {
      unit_mapping_rows <- list()
      mappings_applied_rows <- list()
      unmatched_units <- list()

      for (row_index in seq_len(nrow(unique_combinations))) {
        lab_category_value <- unique_combinations$lab_category[row_index]
        source_unit <- unique_combinations$reference_unit[row_index]
        if (is.na(source_unit)) {
          next
        }

        reference_unit_entry <- if (is.na(lab_category_value)) {
          NULL
        } else {
          private$lab_reference_units_map[[lab_category_value]]
        }
        resolved_targets <- private$resolve_target_units(reference_unit_entry)
        if (length(resolved_targets$accepted) == 0) {
          next
        }

        matched_target <- private$find_matching_target_unit(
          source_unit,
          resolved_targets$accepted,
          preferred = resolved_targets$canonical
        )

        if (!is.null(matched_target)) {
          final_target <- if (lowercase) tolower(matched_target) else matched_target
          if (!identical(final_target, source_unit)) {
            unit_mapping_rows[[length(unit_mapping_rows) + 1]] <- dplyr::tibble(
              lab_category = lab_category_value,
              source_unit = source_unit,
              target_unit = final_target
            )

            # Cosmetic-only changes (micro-sign vs Greek mu, or pure casing when
            # lowercasing anyway) are flagged silent and not reported.
            is_mu_only_difference <- identical(
              gsub("\u00b5", "\u03bc", source_unit, fixed = TRUE),
              matched_target
            )
            is_case_only_difference <- identical(
              tolower(source_unit),
              tolower(matched_target)
            )
            is_silent <- is_mu_only_difference || (lowercase && is_case_only_difference)

            mappings_applied_rows[[length(mappings_applied_rows) + 1]] <- dplyr::tibble(
              lab_category = lab_category_value,
              source_unit = source_unit,
              target_unit = final_target,
              silent = is_silent
            )

            if (!is_silent) {
              cli::cli_alert_info(
                "Mapping {.val {source_unit}} -> {.val {final_target}} for {.val {lab_category_value}}"
              )
            }
          }
        } else {
          unmatched_units[[length(unmatched_units) + 1]] <- list(
            lab_category = lab_category_value,
            source_unit = source_unit,
            expected_units = resolved_targets$accepted
          )
        }
      }

      list(
        unit_mapping = dplyr::bind_rows(unit_mapping_rows),
        mappings_applied = dplyr::bind_rows(mappings_applied_rows),
        unmatched_units = unmatched_units
      )
    },

    # Shared implementation for get_lab_category_stats / get_lab_specimen_stats:
    # pandas-style grouped describe of lab_value_numeric, rounded to 2 decimals.
    grouped_lab_value_stats = function(group_column) {
      self$df |>
        dplyr::filter(!is.na(.data[[group_column]])) |>
        dplyr::group_by(.data[[group_column]]) |>
        dplyr::summarise(
          count = sum(!is.na(.data$lab_value_numeric)),
          unique = dplyr::n_distinct(.data$hospitalization_id, na.rm = TRUE),
          missing_pct = round(100 * mean(is.na(.data$lab_value_numeric)), 2),
          mean = round(mean(.data$lab_value_numeric, na.rm = TRUE), 2),
          std = round(stats::sd(.data$lab_value_numeric, na.rm = TRUE), 2),
          min = round(column_min_or_na(.data$lab_value_numeric), 2),
          q1 = round(
            stats::quantile(.data$lab_value_numeric, 0.25, na.rm = TRUE, names = FALSE, type = 7),
            2
          ),
          median = round(stats::median(.data$lab_value_numeric, na.rm = TRUE), 2),
          q3 = round(
            stats::quantile(.data$lab_value_numeric, 0.75, na.rm = TRUE, names = FALSE, type = 7),
            2
          ),
          max = round(column_max_or_na(.data$lab_value_numeric), 2),
          .groups = "drop"
        ) |>
        dplyr::arrange(.data[[group_column]])
    }
  )
)

#' Normalize a lab unit string for fuzzy comparison
#'
#' Lowercases, strips whitespace and applies clifpy's exact sequence of regex
#' substitutions (bracket removal, trailing `calc` removal, mu/micro to `u`,
#' caret/asterisk removal, `hours`->`hr`, `seconds`->`sec`, lone `s`->`sec`,
#' `minutes`->`min`, `iu`->`u`, `grams`/`gm`->`g`, `k/ul`->`103/ul`,
#' `10e3`/`x10e3`/`x103`->`103`, `pg/ml`->`ng/l`, comma removal). The order of
#' substitutions matters and must match `clifpy.tables.labs._UNIT_REPLACEMENTS`.
#'
#' @param unit_string A character vector of unit strings.
#' @return A character vector of normalized units (`""` for `NA` input).
#' @noRd
normalize_lab_unit <- function(unit_string) {
  normalized <- as.character(unit_string)
  normalized[is.na(normalized)] <- ""
  normalized <- tolower(trimws(normalized))
  normalized <- gsub("[][()]", "", normalized)
  normalized <- gsub("\\s*calc\\s*$", "", normalized)

  normalized <- gsub("\\s+", "", normalized)
  normalized <- gsub("\u03bc|\u00b5", "u", normalized)
  normalized <- gsub("^", "", normalized, fixed = TRUE)
  normalized <- gsub("*", "", normalized, fixed = TRUE)
  normalized <- gsub("hours?", "hr", normalized)
  normalized <- gsub("seconds?", "sec", normalized)
  normalized <- gsub("^s$", "sec", normalized)
  normalized <- gsub("minutes?", "min", normalized)
  normalized <- gsub("iu", "u", normalized, fixed = TRUE)
  normalized <- gsub("\\bgrams?\\b", "g", normalized, perl = TRUE)
  normalized <- gsub("\\bgm\\b", "g", normalized, perl = TRUE)
  normalized <- gsub("k/ul", "103/ul", normalized, fixed = TRUE)
  normalized <- gsub("10e3", "103", normalized, fixed = TRUE)
  normalized <- gsub("x10e3", "103", normalized, fixed = TRUE)
  normalized <- gsub("x103", "103", normalized, fixed = TRUE)
  normalized <- gsub("\\bpg/ml\\b", "ng/l", normalized, perl = TRUE)
  normalized <- gsub(",", "", normalized, fixed = TRUE)

  normalized
}
