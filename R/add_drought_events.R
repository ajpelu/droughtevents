#' @title Add drought period (bands or markers) to a spei time series plot
#'
#' @description
#' Adds shaded polygons, vertical lines, or both to a `ggplot` object to highlight drought events,
#' based on a drought assessment summary (as returned by `droughts()`). Optionally,
#' labels or points can be added to indicate drought severity.
#'
#' @param p A `ggplot` object created by `plot_drought_ts()`.
#' @param drought_assessment A data frame with columns `minyear`, `month_peak`, `d_duration`,
#' `d_severity`, and others, as returned by `droughts()`.
#' @param which_events Display `"all"` droughts or only the `"top"` events.
#' @param metric If `which_events = "top"`, use this column to rank events. Options:
#' `"duration"`, `"severity"`, `"intensity"`, `"lowest_index"`.
#' @param top_n Number of top events to display when `which_events = "top"`. Default to 5.
#' @param type Type of marker to display: `"polygon"` (shaded bands), `"line"` (vertical lines at peak), or `"both"`.
#' @param line_col Color of vertical line (if used).
#' @param line_type Line type of vertical line (e.g. `"dashed"`, `"solid"`).
#' @param pol_fill Fill color of polygon (if used).
#' @param pol_alpha Transparency of polygon fill.
#' @param show_severity Logical. If `TRUE`, plots a point and label for severity at top of each event.
#'
#' @return A modified `ggplot` object.
#'
#' @importFrom dplyr mutate arrange slice_max slice_min slice_head
#' @importFrom lubridate make_date %m+% months
#' @importFrom ggplot2 annotate
#' @importFrom rlang .data
#' @export

add_drought_events <- function(p,
                               drought_assessment,
                               which_events = c("all", "top"),
                               metric = c("duration", "severity", "intensity", "lowest_index"),
                               top_n = 5,
                               type = c("line", "polygon", "both"),
                               line_col = "black",
                               line_type = "dashed",
                               pol_fill = "gray70",
                               pol_alpha = 0.5,
                               show_severity = FALSE
                               ) {

  which_events <- match.arg(which_events)
  metric <- match.arg(metric)
  type <- match.arg(type)

  if (!ggplot2::is_ggplot(p)) {
    cli::cli_abort("{.arg p} must be a ggplot object created by {.fn plot_drought_ts}.")
  }

  if (!inherits(drought_assessment, "data.frame")) {
    cli::cli_abort("{.arg drought_assessment} must be a data frame or tibble")
  }

  if (nrow(drought_assessment) == 0) {
    cli::cli_abort("{.arg drought_assessment} has no rows. There is nothing to plot.")
  }

  required_cols <- c("minyear", "month_peak", "d_duration")
  missing_cols <- setdiff(required_cols, names(drought_assessment))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "{.arg drought_assessment} is missing required {cli::qty(missing_cols)} column{?s}:",
      "*" = "{.field {missing_cols}}"
    ))
  }

  if (!is.numeric(pol_alpha)) {
    cli::cli_abort("{.arg pol_alpha} must be a number between 0 and 1, not {.val {pol_alpha}}.")
  }
  if (pol_alpha < 0 || pol_alpha > 1) {
    cli::cli_abort("{.arg pol_alpha} must be a number between 0 and 1, not {.val {pol_alpha}}.")
  }

  if (!is.logical(show_severity) || length(show_severity) != 1 || is.na(show_severity)) {
    cli::cli_abort("{.arg show_severity} must be {.val TRUE} or {.val FALSE}.")
  }

  if (!is.character(line_col) || length(line_col) != 1) {
    cli::cli_abort("{.arg line_col} must be a single character string (a valid color).")
  }

  if (!is.character(pol_fill) || length(pol_fill) != 1) {
    cli::cli_abort("{.arg pol_fill} must be a single character string (a valid color).")
  }

  # check column for top events filtering
  if (which_events == "top") {
    metric_col <- switch(metric,
                         duration = "d_duration",
                         severity = "d_severity",
                         intensity = "d_intensity",
                         lowest_index = "lowest_spei"
    )

    if (!metric_col %in% names(drought_assessment)) {
      cli::cli_abort(
        "Metric {.val {metric}} requires column {.field {metric_col}}, which is not present in {.arg drought_assessment}."
      )
    }

    if (!is.numeric(top_n) || length(top_n) != 1) {
      cli::cli_abort("{.arg top_n} must be a single positive integer.")
    }

    if (top_n <= 0 || top_n %% 1 != 0) {
      cli::cli_abort("{.arg top_n} must be a positive integer, not {.val {top_n}}.")
    }

    if (top_n > nrow(drought_assessment)) {
      cli::cli_abort(
        "{.arg top_n} must be less than or equal to the number of drought events in {.arg drought_assessment}. Currently {.val {nrow(drought_assessment)}} drought events."
      )
    }
  }

  # check column for severity labels
  if (show_severity && !"d_severity" %in% names(drought_assessment)) {
    cli::cli_abort(
      "{.code show_severity = TRUE} requires a {.field d_severity} column in {.arg drought_assessment}."
    )
  }

  # Calculate date range of drought events
  x <- drought_assessment |>
    dplyr::mutate(
      start_date = lubridate::make_date(.data$minyear, .data$month_peak, 1),
      end_date = .data$start_date %m+% months(.data$d_duration - 1)
    )

  # Top events filtering. Use max or min depending on metric
  if (which_events == "top") {
    x <- if (metric %in% c("duration", "severity")) {
      dplyr::slice_max(x, .data[[metric_col]], n = top_n)
    } else {
      dplyr::slice_min(x, .data[[metric_col]], n = top_n)
    }
  }

  # Try to estimate ymax from plot to place labels if needed
  y_max <- tryCatch({
    max(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range, na.rm = TRUE)
  }, error = function(e) {
    1  # fallback if not plottable yet
  })

  # Add the plot elements
  for (i in seq_len(nrow(x))) {
    if (type %in% c("polygon", "both")) {
      p <- p + ggplot2::annotate(
        "rect",
        xmin = x$start_date[i],
        xmax = x$end_date[i],
        ymin = -Inf,
        ymax = Inf,
        fill = pol_fill,
        alpha = pol_alpha
      )
    }

    if (type %in% c("line", "both")) {
      drought_month <- lubridate::make_date(
        year = x$minyear[i],
        month = x$month_peak[i],
        day = 1
      )

      p <- p + ggplot2::annotate(
        "segment",
        x = drought_month,
        xend = drought_month,
        y = -Inf,
        yend = Inf,
        color = line_col,
        linetype = line_type
      )
    }

    # Add severity point/label
    if (show_severity) {
      midpoint <- x$start_date[i] + (x$end_date[i] - x$start_date[i]) / 2
      label <- round(x$d_severity[i], 1)

      scaled_size <- scales::rescale(x$d_severity[i], to = c(2, 6), from = range(x$d_severity, na.rm = TRUE))

      p <- p + ggplot2::annotate(
        "point",
        x = midpoint,
        y = y_max * 0.95,
        size = scaled_size,
        shape = 21,
        fill = "black"
      ) +
        ggplot2::annotate(
          "text",
          x = midpoint,
          y = y_max,
          label = label,
          size = 3,
          vjust = 0
        )
    }
  }

  return(p)
}
