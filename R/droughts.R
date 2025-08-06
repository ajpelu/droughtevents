#' @title Detect and Summarize Drought Events from Droughts Index Time Series
#'
#' @description
#' Identifies drought events in a time series when a given index (e.g., SPEI) falls below a specified threshold
#' for at least two consecutive months. Returns the processed dataset, the detected drought events,
#' and a summary of their characteristics.
#'
#' @param df A `data.frame` or `tibble` containing the time series data.
#' Must include either:
#' - a `date` column of class `Date` or `POSIXct`, or
#' - separate `year` and `month` columns.
#' - a numeric column specified by `vname` that contains the drought index values.
#'
#' @param vname A string indicating the name of the numeric column to be evaluated (e.g., `"spei12"`).
#'
#' @param threshold A numeric threshold below which values are considered part of a drought event.
#'
#' @return A named list with three elements:
#' * `data`: the original data frame with drought flags and durations.
#' * `drought_events`: only the rows that are part of drought events (duration ≥ 2).
#' * `drought_assessment`: summary of each event, including duration, intensity, severity, and timing.
#'
#' @examples
#' data(spei)
#' # Detect droughts in the SPEI-12 time series with a threshold of -1.28
#' droughts_result <- droughts(spei, vname = "spei12", threshold = -1.28)
#'
#' @importFrom assertthat assert_that
#' @importFrom lubridate make_date month year
#' @importFrom dplyr mutate group_by summarise filter ungroup
#' @importFrom data.table rleid
#' @export

droughts <- function(df, vname, threshold) {

  assertthat::assert_that(
    inherits(df, "data.frame"),
    msg = "The input must be a data frame or tibble."
  )

  assertthat::assert_that(
    vname %in% names(df),
    msg = paste("Variable", vname, "not found in the data frame.")
  )

  assertthat::assert_that(
    is.numeric(df[[vname]]),
    msg = paste("Variable", vname, "must be numeric.")
  )

  assertthat::assert_that(
    is.numeric(threshold),
    msg = "Threshold must be a numeric value."
  )

  df <- .check_date(df)

  min_duration <- 2 # Default minimum duration for drought events

  aux <- df |>
    dplyr::mutate(
      is_drought = ifelse(
        .data[[vname]] < threshold & dplyr::lead(.data[[vname]], default = NA) < threshold,
        1, 0
      ),
      date = lubridate::make_date(year, month)
    )

  events_computation <- aux |>
    dplyr::group_by(index_events = data.table::rleid(is_drought)) |>
    dplyr::mutate(drought_duration = sum(is_drought)) |>
    dplyr::ungroup()

  is_drought_event <- events_computation |>
    dplyr::filter(drought_duration >= min_duration)

  da <- is_drought_event |>
    dplyr::group_by(index_events) |>
    dplyr::summarise(
      d_duration = unique(drought_duration),
      d_intensity = mean(.data[[vname]], na.rm = TRUE),
      d_severity = sum(abs(.data[[vname]]), na.rm = TRUE),
      lowest_spei = min(.data[[vname]]),
      month_peak = month[which.min(.data[[vname]])],
      minyear = min(year),
      maxyear = max(year),
      rangeDate = paste(
        format(min(date, na.rm = TRUE), "%b"),
        "-",
        format(max(date, na.rm = TRUE), "%b")
      ),
      .groups = "drop"
    )

  out <- list(
    data = events_computation,
    drought_events = is_drought_event,
    drought_assessment = da)

  class(out) <- c("droughts", "list")

  return(out)
}









