# Create a auxiliary functions to run the tests
# this avoid to create data frames every test

make_base_plot <- function() {
  df <- data.frame(
    date = seq(as.Date("1989-01-01"), as.Date("2002-01-01"), by = "month"),
    spei = sin(seq(0, 10, length.out = 157))
  )
  ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$spei)) +
    ggplot2::geom_col()
}

make_drought_assessment <- function() {
  data.frame(
    minyear      = c(1990, 1995, 2000),
    month_peak   = c(3, 7, 11),
    d_duration   = c(4, 6, 2),
    d_severity   = c(1.5, 3.2, 0.8),
    d_intensity  = c(0.5, 1.1, 0.3),
    lowest_spei  = c(-1.5, -2.3, -0.9)
  )
}
