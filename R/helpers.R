.check_date <- function(df) {
  # check year and month columns
  if (all(c("year", "month") %in% names(df))) {
    return(df)
  }
  # check date column
  if ("date" %in% names(df) && inherits(df$date, c("Date", "POSIXct"))) {
    df$year <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)
    return(df)
  }

  # error if neither condition is met
  assertthat::assert_that(
    FALSE,
    msg = "Data frame must contain 'year' and 'month' columns, or a 'date' column of class Date or POSIXct."
  )
}

#' @export
print.droughts <- function(x, ...) {
  cat("Drought Assessment Summary:\n")
  print(x$drought_assessment, ...)
  invisible(x)
}
