.check_date <- function(df) {

  if (!inherits(df, "data.frame")) {
    cli::cli_abort("{.arg df} must be a data frame or tibble.")
  }

  has_year  <- "year" %in% names(df)
  has_month <- "month" %in% names(df)

  # check year and month columns present
  if (has_year && has_month) {
    return(df)
  }

  # only one of year / month present
  # regardless of whether a 'date' column also exists
  if (has_year || has_month) {
    missing_one <- if (has_year) "month" else "year"
    cli::cli_abort(
      "{.arg df} has a {.field {if (has_year) 'year' else 'month'}} column but is missing {.field {missing_one}}. Provide both, or use a {.field date} column instead."
    )
  }

  # neither year nor month present
  if ("date" %in% names(df) && inherits(df$date, c("Date", "POSIXct"))) {
    df$year <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)
    return(df)
  }

  # neither year/month nor a valid date column
  cli::cli_abort(
    "{.arg df} must contain {.field year} and {.field month} columns, or a {.field date} column of class {.cls Date} or {.cls POSIXct}."
  )

}

#' @export
print.droughts <- function(x, ...) {
  cat("Drought Assessment Summary:\n")
  print(x$drought_assessment, ...)
  invisible(x)
}
