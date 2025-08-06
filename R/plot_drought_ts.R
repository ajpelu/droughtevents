#' @title Plot Drought Index Time Series with Positive/Negative Bars
#'
#' @description
#' Creates a bar plot of a drought-related time series (e.g., SPEI or SPI),
#' where values above and below zero are colored differently.
#' Optionally, the time series can be aggregated and displayed by year.
#'
#' @param df A `data.frame` or `tibble` containing the time series data.
#'
#' @param vname A string. Name of the numeric column representing the drought index.
#'
#' @param title An optional character string for the plot title.
#'
#' @param date_col A string. Name of the column containing date information (default is `"date"`).
#'
#' @param pos_col Color used for positive values (default is `"blue"`).
#'
#' @param neg_col Color used for negative values (default is `"red"`).
#'
#' @param zero_line_col Color of the horizontal line at zero (default is `"black"`).
#'
#' @param zero_line_linetype Line type for the horizontal zero line (default is `"solid"`).
#'
#' @param by_year Logical. If `TRUE`, aggregates the drought index by year and plots annual means (default is `FALSE`).
#'
#' @param y_axis_title Optional label for the y-axis. If `NULL`, the name of `vname` is used.
#'
#' @param ... Additional arguments passed to `ggplot2::ggplot()`.
#'
#' @return A `ggplot` object representing the drought index time series as a bar plot.
#'
#' @details
#' This plot is useful for visualizing the temporal dynamics of drought indices,
#' highlighting positive (wet) and negative (dry) periods. When `by_year = TRUE`,
#' the function averages the index per year and plots one bar per year.
#'
#' @examples
#' data(spei)
#' plot_drought_ts(spei, vname = "spei12", title = "SPEI-12 Time Series")
#' plot_drought_ts(spei, vname = "spei12", title = "SPEI-12 Annual Mean", by_year = TRUE)
#'
#' @seealso [droughts()]
#'
#' @importFrom assertthat assert_that
#' @importFrom lubridate year make_date
#' @importFrom dplyr mutate filter group_by summarise
#' @importFrom ggplot2 ggplot aes geom_col geom_hline labs scale_fill_manual theme_bw theme element_rect element_text
#' @export

plot_drought_ts <- function(df,
                      vname,
                      title = NULL,
                      date_col = "date",
                      pos_col = "blue",
                      neg_col = "red",
                      zero_line_col = "black",
                      zero_line_linetype = "solid",
                      by_year = FALSE,
                      y_axis_title = NULL,
                      ...) {

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
    date_col %in% names(df),
    msg = paste("Variable", date_col, "not found in the data frame.")
  )

  if (is.null(y_axis_title)) {
    y_axis_title <- vname
  }

  df <- df |> dplyr::filter(!is.na(.data[[vname]]))

  # by year?
  if (by_year) {
    df <- df |>
      dplyr::mutate(year = lubridate::year(.data[[date_col]])) |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        value = mean(.data[[vname]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(date = lubridate::make_date(year = year, month = 1, day = 1))

    vname <- "value"  # renombra para usar en el plot
    date_col <- "date"
    title <- paste(title, "(groupped by year)")
  }

  # compute sign
  df <- df |>
    dplyr::mutate(sign = dplyr::if_else(.data[[vname]] >= 0, "pos", "neg"))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[date_col]], y = .data[[vname]], fill = sign)) +
    ggplot2::geom_col(color = NA) +
    ggplot2::scale_fill_manual(values = c(pos = pos_col, neg = neg_col)) +
    ggplot2::geom_hline(yintercept = 0, linetype = zero_line_linetype, color = zero_line_col) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = toupper(y_axis_title)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      strip.background = ggplot2::element_rect(fill = "white"),
      plot.title = ggplot2::element_text(face = "bold")
    )

  return(p)
}



