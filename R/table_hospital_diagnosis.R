#' HospitalDiagnosis table
#'
#' @description
#' R6 class for the CLIF `hospital_diagnosis` table. These are finalized billing
#' diagnosis codes, appropriate for comorbidity scoring but not for use as
#' features predicting an inpatient event. Inherits all loading, validation and
#' summary behaviour from [BaseTable] and adds diagnosis-, format- and
#' present-on-admission summaries.
#' Port of `clifpy.tables.hospital_diagnosis.HospitalDiagnosis`.
#'
#' @export
#' @examples
#' \dontrun{
#' diagnosis_table <- HospitalDiagnosis$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' diagnosis_table$get_diagnosis_summary()
#' }
HospitalDiagnosis <- R6::R6Class(
  classname = "HospitalDiagnosis",
  inherit = BaseTable,
  public = list(
    #' @description Create a HospitalDiagnosis table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `HospitalDiagnosis` instance.
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
    },

    #' @description Comprehensive summary statistics for the diagnosis data.
    #' @return A named list with total diagnoses, unique hospitalizations and codes,
    #'   plus format, primary/secondary and present-on-admission counts when the
    #'   corresponding columns exist.
    get_diagnosis_summary = function() {
      if (is.null(self$df)) {
        return(list())
      }

      diagnosis_summary <- list(
        total_diagnoses = nrow(self$df),
        unique_hospitalizations = if ("hospitalization_id" %in% names(self$df)) {
          dplyr::n_distinct(self$df$hospitalization_id, na.rm = TRUE)
        } else {
          0L
        },
        unique_diagnosis_codes = if ("diagnosis_code" %in% names(self$df)) {
          dplyr::n_distinct(self$df$diagnosis_code, na.rm = TRUE)
        } else {
          0L
        }
      )

      if ("diagnosis_code_format" %in% names(self$df)) {
        diagnosis_summary$diagnosis_format_counts <-
          value_counts_named_list(self$df$diagnosis_code_format)
      }

      if ("diagnosis_primary" %in% names(self$df)) {
        diagnosis_summary$primary_diagnosis_counts <- list(
          primary = sum(self$df$diagnosis_primary == 1, na.rm = TRUE),
          secondary = sum(self$df$diagnosis_primary == 0, na.rm = TRUE)
        )
      }

      if ("poa_present" %in% names(self$df)) {
        diagnosis_summary$poa_counts <- list(
          present_on_admission = sum(self$df$poa_present == 1, na.rm = TRUE),
          not_present_on_admission = sum(self$df$poa_present == 0, na.rm = TRUE)
        )
      }

      diagnosis_summary
    },

    #' @description Counts of primary diagnoses by code and code format.
    #' @return A tibble with columns `diagnosis_code`, `diagnosis_code_format` and
    #'   `count`, sorted by count descending. Empty tibble when
    #'   `diagnosis_primary` is absent or no primary diagnoses exist.
    get_primary_diagnosis_counts = function() {
      if (is.null(self$df) || !"diagnosis_primary" %in% names(self$df)) {
        return(dplyr::tibble())
      }

      primary_diagnoses <- dplyr::filter(
        self$df,
        !is.na(.data$diagnosis_primary) & .data$diagnosis_primary == 1
      )
      if (nrow(primary_diagnoses) == 0) {
        return(dplyr::tibble())
      }

      primary_diagnoses |>
        dplyr::count(.data$diagnosis_code, .data$diagnosis_code_format, name = "count") |>
        dplyr::arrange(dplyr::desc(.data$count))
    },

    #' @description Present-on-admission statistics, overall and by diagnosis type.
    #' @return A named list with an `overall` element and, when data exist,
    #'   `primary` and `secondary` elements, each holding total diagnoses, POA
    #'   present/not-present counts and the POA rate as a percentage.
    get_poa_statistics = function() {
      if (is.null(self$df) ||
          !"poa_present" %in% names(self$df) ||
          !"diagnosis_primary" %in% names(self$df)) {
        return(list())
      }

      poa_statistics <- list()
      total_diagnoses <- nrow(self$df)
      poa_present_count <- sum(self$df$poa_present == 1, na.rm = TRUE)
      poa_not_present_count <- sum(self$df$poa_present == 0, na.rm = TRUE)

      poa_statistics$overall <- list(
        total_diagnoses = total_diagnoses,
        poa_present_count = poa_present_count,
        poa_not_present_count = poa_not_present_count,
        poa_present_rate = if (total_diagnoses > 0) {
          poa_present_count / total_diagnoses * 100
        } else {
          0
        }
      )

      diagnosis_type_values <- list(primary = 1, secondary = 0)
      for (diagnosis_type in names(diagnosis_type_values)) {
        diagnosis_value <- diagnosis_type_values[[diagnosis_type]]
        subset_rows <- dplyr::filter(
          self$df,
          !is.na(.data$diagnosis_primary) & .data$diagnosis_primary == !!diagnosis_value
        )
        if (nrow(subset_rows) == 0) {
          next
        }
        subset_total <- nrow(subset_rows)
        subset_poa_present <- sum(subset_rows$poa_present == 1, na.rm = TRUE)
        subset_poa_not_present <- sum(subset_rows$poa_present == 0, na.rm = TRUE)
        poa_statistics[[diagnosis_type]] <- list(
          total_diagnoses = subset_total,
          poa_present_count = subset_poa_present,
          poa_not_present_count = subset_poa_not_present,
          poa_present_rate = if (subset_total > 0) {
            subset_poa_present / subset_total * 100
          } else {
            0
          }
        )
      }

      poa_statistics
    },

    #' @description Summary statistics grouped by diagnosis code format.
    #' @return A named list keyed by code format (e.g. `"ICD10CM"`), each element a
    #'   list of total diagnoses, unique codes, unique hospitalizations and, where
    #'   available, primary/secondary and POA counts.
    get_diagnosis_by_format = function() {
      if (is.null(self$df) || !"diagnosis_code_format" %in% names(self$df)) {
        return(list())
      }

      format_statistics <- list()
      for (format_type in unique(self$df$diagnosis_code_format)) {
        subset_rows <- if (is.na(format_type)) {
          dplyr::filter(self$df, is.na(.data$diagnosis_code_format))
        } else {
          dplyr::filter(
            self$df,
            !is.na(.data$diagnosis_code_format) & .data$diagnosis_code_format == !!format_type
          )
        }

        format_summary <- list(
          total_diagnoses = nrow(subset_rows),
          unique_diagnosis_codes = if ("diagnosis_code" %in% names(subset_rows)) {
            dplyr::n_distinct(subset_rows$diagnosis_code, na.rm = TRUE)
          } else {
            0L
          },
          unique_hospitalizations = if ("hospitalization_id" %in% names(subset_rows)) {
            dplyr::n_distinct(subset_rows$hospitalization_id, na.rm = TRUE)
          } else {
            0L
          }
        )

        if ("diagnosis_primary" %in% names(subset_rows)) {
          format_summary$primary_count <- sum(subset_rows$diagnosis_primary == 1, na.rm = TRUE)
          format_summary$secondary_count <- sum(subset_rows$diagnosis_primary == 0, na.rm = TRUE)
        }
        if ("poa_present" %in% names(subset_rows)) {
          format_summary$poa_present_count <- sum(subset_rows$poa_present == 1, na.rm = TRUE)
          format_summary$poa_not_present_count <- sum(subset_rows$poa_present == 0, na.rm = TRUE)
        }

        format_statistics[[as.character(format_type)]] <- format_summary
      }

      format_statistics
    },

    #' @description Diagnosis counts per hospitalization.
    #' @return A tibble with columns `hospitalization_id`, `total_diagnoses`,
    #'   `primary_diagnoses`, `poa_present_diagnoses` and `secondary_diagnoses`,
    #'   sorted by total diagnoses descending. Empty tibble when
    #'   `hospitalization_id` is absent.
    get_hospitalization_diagnosis_counts = function() {
      if (is.null(self$df) || !"hospitalization_id" %in% names(self$df)) {
        return(dplyr::tibble())
      }
      has_poa_column <- "poa_present" %in% names(self$df)

      self$df |>
        dplyr::filter(!is.na(.data$hospitalization_id)) |>
        dplyr::group_by(.data$hospitalization_id) |>
        dplyr::summarise(
          total_diagnoses = sum(!is.na(.data$diagnosis_code)),
          primary_diagnoses = sum(.data$diagnosis_primary == 1, na.rm = TRUE),
          poa_present_diagnoses = if (has_poa_column) {
            sum(.data$poa_present == 1, na.rm = TRUE)
          } else {
            0L
          },
          .groups = "drop"
        ) |>
        dplyr::mutate(
          secondary_diagnoses = .data$total_diagnoses - .data$primary_diagnoses
        ) |>
        dplyr::arrange(dplyr::desc(.data$total_diagnoses))
    }
  )
)
