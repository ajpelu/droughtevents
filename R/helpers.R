.check_date <- function(df) {

  if (!inherits(df, "data.frame")) {
    cli::cli_abort("{.arg df} must be a data frame or tibble")
  }

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
