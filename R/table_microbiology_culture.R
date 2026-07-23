#' MicrobiologyCulture table
#'
#' @description
#' R6 class for the CLIF `microbiology_culture` table (organism identification from
#' cultures). Inherits all loading, validation and summary behaviour from
#' [BaseTable], adds a timestamp-ordering check that runs as part of `validate()`,
#' and provides category-to-name maps plus a chi-square outlier screen of fluid
#' against organism.
#' Port of `clifpy.tables.microbiology_culture.MicrobiologyCulture`.
#'
#' @export
#' @examples
#' \dontrun{
#' culture_table <- MicrobiologyCulture$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' culture_table$validate()
#' culture_table$top_fluid_org_outliers()
#' }
MicrobiologyCulture <- R6::R6Class(
  classname = "MicrobiologyCulture",
  inherit = BaseTable,
  public = list(
    #' @field time_order_validation_errors Errors from the last
    #'   `validate_timestamp_order()` run.
    time_order_validation_errors = NULL,

    #' @description Create a MicrobiologyCulture table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `MicrobiologyCulture` instance.
    initialize = function(data_directory = NULL,
                          filetype = NULL,
                          timezone = "UTC",
                          output_directory = NULL,
                          data = NULL,
                          clif_version = DEFAULT_CLIF_VERSION) {
      self$time_order_validation_errors <- list()
      super$initialize(
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory,
        data = data,
        clif_version = clif_version
      )
    },

    #' @description Whether the last validation run found no errors.
    #' @return `TRUE` when neither schema nor timestamp-order errors are present.
    isvalid = function() {
      super$isvalid() && length(self$time_order_validation_errors) == 0
    },

    #' @description Check that `order_dttm` <= `collect_dttm` <= `result_dttm`.
    #'
    #' A one-minute grace period absorbs recording jitter: only differences of at
    #' least one minute in the wrong direction are flagged. Populates
    #' `$time_order_validation_errors` and appends to `$errors`.
    #'
    #' @return A tibble of violating rows (keys plus the three timestamps), or
    #'   `NULL` when nothing is violated.
    validate_timestamp_order = function() {
      self$time_order_validation_errors <- list()

      key_columns <- c("patient_id", "hospitalization_id", "organism_id")
      time_columns <- c("order_dttm", "collect_dttm", "result_dttm")
      missing_columns <- setdiff(time_columns, names(self$df))

      if (length(missing_columns) > 0) {
        message_text <- paste0(
          "Missing required timestamp columns for time order validation: ",
          paste(missing_columns, collapse = ", ")
        )
        self$time_order_validation_errors <- list(list(
          type = "missing_time_order_columns",
          columns = missing_columns,
          message = message_text,
          table = self$table_name
        ))
        self$errors <- c(self$errors, self$time_order_validation_errors)
        cli::cli_alert_warning(message_text)
        return(NULL)
      }

      grace_period_seconds <- 60

      order_after_collect <- !is.na(self$df$order_dttm) & !is.na(self$df$collect_dttm) &
        as.numeric(difftime(self$df$order_dttm, self$df$collect_dttm, units = "secs")) >=
          grace_period_seconds
      collect_after_result <- !is.na(self$df$collect_dttm) & !is.na(self$df$result_dttm) &
        as.numeric(difftime(self$df$collect_dttm, self$df$result_dttm, units = "secs")) >=
          grace_period_seconds

      order_violation_count <- sum(order_after_collect)
      result_violation_count <- sum(collect_after_result)

      if (order_violation_count > 0) {
        self$time_order_validation_errors <- c(
          self$time_order_validation_errors,
          list(list(
            type = "time_order_validation",
            rule = "order_dttm <= collect_dttm, grace 1 min",
            message = sprintf("%d rows have order_dttm > collect_dttm", order_violation_count),
            rows = order_violation_count,
            table = self$table_name
          ))
        )
      }
      if (result_violation_count > 0) {
        self$time_order_validation_errors <- c(
          self$time_order_validation_errors,
          list(list(
            type = "time_order_validation",
            rule = "collect_dttm <= result_dttm, grace 1 min",
            message = sprintf("%d rows have collect_dttm > result_dttm", result_violation_count),
            rows = result_violation_count,
            table = self$table_name
          ))
        )
      }

      if (length(self$time_order_validation_errors) > 0) {
        self$errors <- c(self$errors, self$time_order_validation_errors)
        cli::cli_alert_warning(
          "Found {.val {length(self$time_order_validation_errors)}} range validation errors"
        )
      }

      violating_rows <- order_after_collect | collect_after_result
      if (any(violating_rows)) {
        display_columns <- intersect(c(key_columns, time_columns), names(self$df))
        return(self$df[violating_rows, display_columns, drop = FALSE])
      }

      cli::cli_alert_info("validate_timestamp_order: passed (no violations)")
      NULL
    },

    #' @description Map category values to the raw names recorded under them.
    #'
    #' Names are unique per category (and per group when `group_col` is supplied)
    #' and ordered by descending frequency then alphabetically, or alphabetically
    #' only when `sort = "alpha"`.
    #'
    #' @param df Data frame to summarize.
    #' @param category_col Column holding the category values.
    #' @param name_col Column holding the raw names.
    #' @param group_col Optional grouping column; when supplied the result is a
    #'   three-level list of group -> category -> names.
    #' @param dropna Whether to drop rows with missing values in any used column.
    #' @param sort Either `"freq_then_alpha"` (default) or `"alpha"`.
    #' @param max_names_per_cat Optional cap on the number of names per category.
    #' @param include_counts If `TRUE`, emit `list(name = , n = )` entries instead
    #'   of plain name strings.
    #' @return A named list, two levels deep by default and three when `group_col`
    #'   is supplied.
    cat_vs_name_map = function(df,
                               category_col,
                               name_col,
                               group_col = NULL,
                               dropna = TRUE,
                               sort = c("freq_then_alpha", "alpha"),
                               max_names_per_cat = NULL,
                               include_counts = FALSE) {
      sort <- match.arg(sort)
      if (is.null(df)) {
        return(list())
      }
      required_columns <- c(category_col, name_col, group_col)
      if (!all(required_columns %in% names(df))) {
        return(list())
      }

      subset_data <- df[, required_columns, drop = FALSE]
      if (dropna) {
        subset_data <- subset_data[stats::complete.cases(subset_data), , drop = FALSE]
      }

      grouping_columns <- c(group_col, category_col, name_col)
      value_counts <- subset_data |>
        dplyr::count(dplyr::across(dplyr::all_of(grouping_columns)), name = "n") |>
        dplyr::arrange(dplyr::across(dplyr::all_of(grouping_columns)))

      emit_names <- function(count_block) {
        ordered_block <- if (sort == "alpha") {
          dplyr::arrange(count_block, .data[[name_col]])
        } else {
          dplyr::arrange(count_block, dplyr::desc(.data$n), .data[[name_col]])
        }
        emitted <- if (include_counts) {
          lapply(seq_len(nrow(ordered_block)), function(row_index) {
            list(
              name = as.character(ordered_block[[name_col]][row_index]),
              n = as.integer(ordered_block$n[row_index])
            )
          })
        } else {
          as.character(ordered_block[[name_col]])
        }
        if (!is.null(max_names_per_cat) && length(emitted) > max_names_per_cat) {
          emitted <- emitted[seq_len(max_names_per_cat)]
        }
        emitted
      }

      build_category_map <- function(count_block) {
        category_values <- unique(as.character(count_block[[category_col]]))
        category_map <- lapply(category_values, function(category_value) {
          emit_names(count_block[as.character(count_block[[category_col]]) == category_value, , drop = FALSE])
        })
        stats::setNames(category_map, category_values)
      }

      if (!is.null(group_col)) {
        group_values <- unique(as.character(value_counts[[group_col]]))
        grouped_map <- lapply(group_values, function(group_value) {
          build_category_map(
            value_counts[as.character(value_counts[[group_col]]) == group_value, , drop = FALSE]
          )
        })
        return(stats::setNames(grouped_map, group_values))
      }

      build_category_map(value_counts)
    },

    #' @description Organism names by organism category within organism group.
    #' @param include_counts If `TRUE`, emit `list(name = , n = )` entries.
    #' @param ... Further arguments passed to `cat_vs_name_map()`.
    #' @return A three-level named list: group -> category -> names.
    organism_group_cat_name_map = function(include_counts = FALSE, ...) {
      self$cat_vs_name_map(
        self$df,
        category_col = "organism_category",
        name_col = "organism_name",
        group_col = "organism_group",
        include_counts = include_counts,
        ...
      )
    },

    #' @description Organism names by organism category.
    #' @param include_counts If `TRUE`, emit `list(name = , n = )` entries.
    #' @param ... Further arguments passed to `cat_vs_name_map()`.
    #' @return A named list: category -> names.
    organism_cat_name_map = function(include_counts = FALSE, ...) {
      self$cat_vs_name_map(
        self$df,
        category_col = "organism_category",
        name_col = "organism_name",
        include_counts = include_counts,
        ...
      )
    },

    #' @description Fluid names by fluid category.
    #' @param include_counts If `TRUE`, emit `list(name = , n = )` entries.
    #' @param ... Further arguments passed to `cat_vs_name_map()`.
    #' @return A named list: category -> names.
    fluid_cat_name_map = function(include_counts = FALSE, ...) {
      self$cat_vs_name_map(
        self$df,
        category_col = "fluid_category",
        name_col = "fluid_name",
        include_counts = include_counts,
        ...
      )
    },

    #' @description Fluid-organism pairs that deviate most from independence.
    #'
    #' Builds a contingency table of `fluid_category` against the requested
    #' organism level, computes chi-square standardized residuals
    #' `(observed - expected) / sqrt(expected)`, and returns the largest positive
    #' and negative residuals.
    #'
    #' @param level Either `"organism_group"` or `"organism_category"`.
    #' @param min_count Minimum observed count for a cell to be considered.
    #' @param top_k Number of top positive and negative cells to return.
    #' @return A named list with `top_positive` and `top_negative` tibbles, each
    #'   with columns `fluid_category`, the organism level, `observed`, `expected`
    #'   and `std_resid`.
    top_fluid_org_outliers = function(level = c("organism_group", "organism_category"),
                                      min_count = 0,
                                      top_k = 10) {
      level <- match.arg(level)
      empty_result <- list(top_positive = dplyr::tibble(), top_negative = dplyr::tibble())
      if (is.null(self$df) ||
          !all(c("fluid_category", level) %in% names(self$df))) {
        return(empty_result)
      }

      complete_rows <- !is.na(self$df$fluid_category) & !is.na(self$df[[level]])
      if (!any(complete_rows)) {
        return(empty_result)
      }

      contingency_table <- table(
        fluid_category = as.character(self$df$fluid_category[complete_rows]),
        organism_level = as.character(self$df[[level]][complete_rows])
      )
      if (length(contingency_table) == 0) {
        return(empty_result)
      }

      total_count <- sum(contingency_table)
      expected_counts <- outer(rowSums(contingency_table), colSums(contingency_table)) / total_count
      standardized_residuals <- (contingency_table - expected_counts) / sqrt(expected_counts)

      residual_frame <- dplyr::tibble(
        fluid_category = rep(rownames(contingency_table), each = ncol(contingency_table)),
        organism_level = rep(colnames(contingency_table), times = nrow(contingency_table)),
        observed = as.numeric(t(contingency_table)),
        expected = as.numeric(t(expected_counts)),
        std_resid = as.numeric(t(standardized_residuals))
      )
      names(residual_frame)[2] <- level

      residual_frame <- residual_frame |>
        dplyr::filter(
          !is.na(.data$observed), !is.na(.data$expected), !is.na(.data$std_resid),
          .data$observed >= min_count
        )

      list(
        top_positive = utils::head(
          dplyr::arrange(residual_frame, dplyr::desc(.data$std_resid)),
          top_k
        ),
        top_negative = utils::head(
          dplyr::arrange(residual_frame, .data$std_resid),
          top_k
        )
      )
    }
  ),
  private = list(
    run_table_specific_validations = function() {
      self$validate_timestamp_order()
      # validate_timestamp_order() appends directly to self$errors, matching
      # clifpy, so nothing further is returned to the base validator.
      list()
    }
  )
)
