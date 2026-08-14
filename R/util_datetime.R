#' Datetime standardization utilities
#'
#' R ports of clifpy's `datetime_polars.py`. In clifpy these exist because Polars
#' tracks both a timezone and a time unit (ms/us/ns) per datetime column, so joins
#' can fail on a precision mismatch. R's `POSIXct` carries a single `tzone` attribute
#' and always stores seconds as a double, so there is no time-unit axis to reconcile;
#' the `target_time_unit` argument is accepted for signature compatibility but has no
#' effect. The timezone behaviour is ported faithfully.
#'
#' Because these mirror clifpy's Polars entry points, they are also exported under
#' the `*_polars` names clifpy re-exports at its package root, so code translated
#' from Python keeps working.
#'
#' @name clif-datetime
NULL

#' Standardize datetime columns to a target timezone
#'
#' For each datetime column: a timezone-aware column is converted to
#' `target_timezone` (the underlying instant is unchanged, only the display zone),
#' and a naive column is localized to `target_timezone`. Columns are auto-detected by
#' `POSIXct` class or a `dttm` name when `datetime_columns` is not given, matching
#' clifpy's detection.
#'
#' @param df A data frame.
#' @param target_timezone Olson timezone name, e.g. `"US/Central"`.
#' @param target_time_unit Accepted for clifpy compatibility; ignored in R.
#' @param ambiguous Accepted for clifpy compatibility; R's `POSIXct` resolves DST
#'   ambiguity by its own rules and does not take this option.
#' @param non_existent Accepted for clifpy compatibility; ignored in R.
#' @param datetime_columns Optional character vector of columns to convert. When
#'   `NULL`, all datetime columns are detected automatically.
#'
#' @return The data frame with standardized datetime columns.
#' @export
#'
#' @examples
#' events <- data.frame(recorded_dttm = as.POSIXct("2020-01-01 12:00", tz = "UTC"))
#' standardize_datetime_columns(events, target_timezone = "US/Central")
standardize_datetime_columns <- function(df,
                                         target_timezone,
                                         target_time_unit = "ns",
                                         ambiguous = "earliest",
                                         non_existent = "null",
                                         datetime_columns = NULL) {
  if (is.null(datetime_columns)) {
    is_datetime <- vapply(df, function(column) inherits(column, "POSIXct"), logical(1))
    named_dttm <- grepl("dttm", tolower(names(df)))
    datetime_columns <- names(df)[is_datetime | named_dttm]
  }
  datetime_columns <- intersect(datetime_columns, names(df))

  if (length(datetime_columns) == 0) {
    return(df)
  }

  for (column_name in datetime_columns) {
    column_values <- df[[column_name]]
    if (!inherits(column_values, "POSIXct")) {
      # A character or Date column named *_dttm is parsed as if it were wall-clock
      # time in the target zone, matching clifpy's localize-naive branch.
      column_values <- as.POSIXct(as.character(column_values), tz = target_timezone)
    } else {
      attr(column_values, "tzone") <- target_timezone
    }
    df[[column_name]] <- column_values
  }

  df
}

#' Align two datetime columns to a common timezone
#'
#' Standardizes one datetime column in each of two data frames to `target_timezone`,
#' the R analogue of clifpy preparing frames for a `join_asof`. The time-unit
#' reconciliation clifpy performs is unnecessary in R.
#'
#' @param df1 First data frame.
#' @param df2 Second data frame.
#' @param df1_datetime_col Datetime column name in `df1`.
#' @param df2_datetime_col Datetime column name in `df2`.
#' @param target_timezone Olson timezone name.
#' @param target_time_unit Accepted for clifpy compatibility; ignored in R.
#'
#' @return A named list with elements `df1` and `df2`, each standardized.
#' @export
ensure_datetime_precision_match <- function(df1,
                                            df2,
                                            df1_datetime_col,
                                            df2_datetime_col,
                                            target_timezone,
                                            target_time_unit = "ns") {
  df1 <- standardize_datetime_columns(
    df1,
    target_timezone = target_timezone,
    target_time_unit = target_time_unit,
    datetime_columns = df1_datetime_col
  )
  df2 <- standardize_datetime_columns(
    df2,
    target_timezone = target_timezone,
    target_time_unit = target_time_unit,
    datetime_columns = df2_datetime_col
  )
  list(df1 = df1, df2 = df2)
}

#' Convert a single datetime vector to a timezone for comparison
#'
#' The R analogue of clifpy's `standardize_datetime_for_comparison`, which returns a
#' Polars expression; here it returns the converted vector directly, suitable for use
#' in a `dplyr::filter()` comparison.
#'
#' @param datetime_values A `POSIXct` vector.
#' @param target_timezone Olson timezone name.
#' @param target_time_unit Accepted for clifpy compatibility; ignored in R.
#'
#' @return The vector relabelled to `target_timezone`.
#' @export
standardize_datetime_for_comparison <- function(datetime_values,
                                                target_timezone,
                                                target_time_unit = "ns") {
  if (!inherits(datetime_values, "POSIXct")) {
    datetime_values <- as.POSIXct(as.character(datetime_values), tz = target_timezone)
  } else {
    attr(datetime_values, "tzone") <- target_timezone
  }
  datetime_values
}

#' @rdname clif-datetime
#' @inheritParams standardize_datetime_columns
#' @param df A data frame.
#' @param target_timezone Olson timezone name.
#' @return The data frame with standardized datetime columns.
#' @export
standardize_datetime_columns_polars <- standardize_datetime_columns

#' @rdname clif-datetime
#' @inheritParams ensure_datetime_precision_match
#' @return A named list with elements `df1` and `df2`.
#' @export
ensure_datetime_precision_match_polars <- ensure_datetime_precision_match

#' @rdname clif-datetime
#' @param df A data frame.
#' @param site_tz_str Olson timezone name.
#' @param verbose Whether to emit a conversion summary.
#' @return The data frame with datetime columns in `site_tz_str`.
#' @export
convert_datetime_columns_to_site_tz_polars <- function(df, site_tz_str, verbose = TRUE) {
  convert_datetime_columns_to_site_tz(df, site_tz_str, verbose = verbose)
}
