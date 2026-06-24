#' @title Detect and summarize drought events from SPEI droughts index time series
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
#' @param vname A string indicating the name of the numeric column to be evaluated (e.g., `"spei12"`).
#' @param threshold A numeric threshold below which values are considered part of a drought event.
#' @param min_duration Minimum number of consecutive months below `threshold` required for an
#' event to be considered a drought. Default is 2.
#'
#' @return A named list with three elements:
#' * `data`: the original data frame with drought flags and durations.
#' * `drought_events`: only the rows that are part of drought events (duration ≥ 2).
#' * `drought_assessment`: summary of each event, including duration, intensity, severity, and timing.
#'
#' @examples
#' data(spei_granada)
#' # Detect droughts in the SPEI-12 time series with a threshold of -1.28
#' droughts_result <- droughts(spei_granada, vname = "spei12", threshold = -1.28)
#'
#' @importFrom lubridate make_date
#' @importFrom dplyr mutate group_by summarise filter ungroup
#' @importFrom data.table rleid
#' @importFrom rlang .data
#' @export

droughts <- function(df, vname, threshold, min_duration = 2) {

  if (!inherits(df, "data.frame")) {
    cli::cli_abort("{.arg df} must be a data frame or tibble.")
  }

  if (!vname %in% names(df)) {
    cli::cli_abort("Variable {.val {vname}} not found in the data frame.")
  }

  if (!is.numeric(df[[vname]])) {
    cli::cli_abort("Variable {.val {vname}} must be numeric.")
    }

  if (!is.numeric(threshold)) {
    cli::cli_abort("{.arg threshold} must be a numeric value.")
  }

  if (!is.numeric(min_duration)) {
    cli::cli_abort("{.arg min_duration} must be a numeric value.")
  }

  df <- .check_date(df)

  aux <- df |>
    dplyr::mutate(
      is_drought = ifelse(
        .data[[vname]] < threshold & dplyr::lead(.data[[vname]], default = NA) < threshold,
        1, 0
      ),
      date = lubridate::make_date(.data$year, .data$month)
    )

  events_computation <- aux |>
    dplyr::group_by(index_events = data.table::rleid(.data$is_drought)) |>
    dplyr::mutate(drought_duration = sum(.data$is_drought)) |>
    dplyr::ungroup()

  is_drought_event <- events_computation |>
    dplyr::filter(.data$drought_duration >= min_duration)

  if (nrow(is_drought_event) == 0) {
    # No events detected, build empty summary with the right columns
    da <- data.frame(
      index_events = integer(0),
      d_duration = numeric(0),
      d_intensity = numeric(0),
      d_severity = numeric(0),
      lowest_spei = numeric(0),
      month_peak = integer(0),
      minyear = integer(0),
      maxyear = integer(0),
      rangeDate = character(0)
    )
  } else {
    da <- is_drought_event |>
      dplyr::group_by(.data$index_events) |>
      dplyr::summarise(
        d_duration = unique(.data$drought_duration),
        d_intensity = mean(.data[[vname]], na.rm = TRUE),
        d_severity = sum(abs(.data[[vname]]), na.rm = TRUE),
        lowest_spei = min(.data[[vname]]),
        month_peak = .data$month[which.min(.data[[vname]])],
        minyear = min(.data$year),
        maxyear = max(.data$year),
        rangeDate = paste(
          format(min(.data$date, na.rm = TRUE), "%b"),
          "-",
          format(max(.data$date, na.rm = TRUE), "%b")
        ),
        .groups = "drop"
      )
  }

  out <- list(
    data = events_computation,
    drought_events = is_drought_event,
    drought_assessment = da)

  class(out) <- c("droughts", "list")

  return(out)
}









