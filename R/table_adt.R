#' Adt table
#'
#' @description
#' R6 class for the CLIF `adt` (admission/discharge/transfer) table. Inherits all
#' loading, validation and summary behaviour from [BaseTable] and adds a check for
#' overlapping admissions within a hospitalization.
#' Port of `clifpy.tables.adt.Adt`.
#'
#' @export
#' @examples
#' \dontrun{
#' adt_table <- Adt$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' adt_table$check_overlapping_admissions()
#' }
Adt <- R6::R6Class(
  classname = "Adt",
  inherit = BaseTable,
  public = list(
    #' @description Create an Adt table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `Adt` instance.
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

    #' @description Check for overlapping admissions within the same hospitalization.
    #'
    #' Identifies cases where a patient has overlapping stays in different locations
    #' within the same hospitalization, i.e. the `out_dttm` of one location falls
    #' after the `in_dttm` of the next location.
    #'
    #' @param save_overlaps If `TRUE`, write detailed overlap information to
    #'   `overlapping_admissions.csv`.
    #' @param overlaps_output_directory Directory for the overlaps CSV. When `NULL`,
    #'   uses the table's `output_directory`.
    #' @return Integer count of unique hospitalizations with overlapping admissions.
    check_overlapping_admissions = function(save_overlaps = FALSE,
                                            overlaps_output_directory = NULL) {
      if (is.null(self$df)) {
        return(0L)
      }
      if (!"hospitalization_id" %in% names(self$df)) {
        cli::cli_abort("Error checking time overlap: hospitalization_id is missing.")
      }

      consecutive_stays <- self$df |>
        dplyr::arrange(.data$hospitalization_id, .data$in_dttm) |>
        dplyr::group_by(.data$hospitalization_id) |>
        dplyr::mutate(
          next_in_dttm = dplyr::lead(.data$in_dttm),
          next_location_name = dplyr::lead(.data$location_name),
          next_location_category = dplyr::lead(.data$location_category)
        ) |>
        dplyr::ungroup()

      # Mirror pandas comparison semantics: NaN != anything is TRUE, while
      # comparisons involving NaT evaluate to FALSE.
      location_differs <- is.na(consecutive_stays$location_name) |
        is.na(consecutive_stays$next_location_name) |
        consecutive_stays$location_name != consecutive_stays$next_location_name
      times_overlap <- !is.na(consecutive_stays$out_dttm) &
        !is.na(consecutive_stays$next_in_dttm) &
        consecutive_stays$out_dttm > consecutive_stays$next_in_dttm

      overlap_rows <- consecutive_stays[
        !is.na(consecutive_stays$next_in_dttm) & location_differs & times_overlap,
        ,
        drop = FALSE
      ]

      if (save_overlaps && nrow(overlap_rows) > 0) {
        overlaps_frame <- dplyr::tibble(
          hospitalization_id = overlap_rows$hospitalization_id,
          `Initial Location` = overlap_rows$location_name,
          `Initial Location Category` = overlap_rows$location_category,
          `Overlapping Location` = overlap_rows$next_location_name,
          `Overlapping Location Category` = overlap_rows$next_location_category,
          `Admission Start` = overlap_rows$in_dttm,
          `Admission End` = overlap_rows$out_dttm,
          `Next Admission Start` = overlap_rows$next_in_dttm
        )
        save_directory <- overlaps_output_directory %||% self$output_directory
        if (!dir.exists(save_directory)) {
          dir.create(save_directory, recursive = TRUE, showWarnings = FALSE)
        }
        readr::write_csv(
          overlaps_frame,
          file.path(save_directory, "overlapping_admissions.csv")
        )
      }

      as.integer(dplyr::n_distinct(overlap_rows$hospitalization_id))
    }
  )
)
