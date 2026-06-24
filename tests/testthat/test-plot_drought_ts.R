set.seed(12345)
make_ts_df <- function() {
  data.frame(
    date  = seq(as.Date("2000-01-01"), as.Date("2002-12-01"), by = "month"),
    spei  = sample(seq(-3.5, 3.5, length.out = 36))
  )
}


test_that("df is a data frame", {
  expect_error(
    plot_drought_ts(df = list(a = 1), vname = "spei"),
    "must be a data frame or tibble"
  )
})

test_that("vname is a column in df", {
  expect_error(
    plot_drought_ts(df = make_ts_df(), vname = "nothing"),
    "not found in the data frame"
  )
})

test_that("vname column is numeric", {
  df <- make_ts_df()
  df$spei <- as.character(df$spei)
  expect_error(
    plot_drought_ts(df = df, vname = "spei"),
    "must be numeric"
  )
})

test_that("date_col is a column of df", {
  df <- make_ts_df()
  expect_error(
    plot_drought_ts(df = df, vname = "spei", date_col = "nothing"),
    "is not present in"
  )
})

test_that("date_col is of class Date or POSIXct", {
  df <- make_ts_df()
  df$date <- as.character(df$date)
  expect_error(
    plot_drought_ts(df = df, vname = "spei"),
    "must be of class"
  )
})

test_that("returns a ggplot object on valid input", {
  result <- plot_drought_ts(df = make_ts_df(), vname = "spei")
  expect_s3_class(result, "ggplot")
})


test_that("uses vname as y_axis_title when y_axis_title is NULL", {
  result <- plot_drought_ts(df = make_ts_df(), vname = "spei", y_axis_title = NULL)
  expect_equal(result$labels$y, toupper("spei"))
})

test_that("uses the custom y_axis_title when provided", {
  result <- plot_drought_ts(df = make_ts_df(), vname = "spei", y_axis_title = "My index")
  expect_equal(result$labels$y, toupper("My index"))
})

test_that("uses the provided title", {
  result <- plot_drought_ts(df = make_ts_df(), vname = "spei", title = "custom title")
  expect_equal(result$labels$title, "custom title")
})

test_that("by_year = TRUE aggregates data into one row per year", {
  df <- make_ts_df()  # 3 years of montlhy data (2000-2002)
  result <- plot_drought_ts(df = df, vname = "spei", by_year = TRUE)

  # the data frame used to plot must contains 3 rows (one per year)
  expect_equal(nrow(result$data), 3)
})

test_that("by_year = TRUE appends '(grouped by year)' to the title", {
  result <- plot_drought_ts(df = make_ts_df(), vname = "spei", title = "SPEI", by_year = TRUE)
  expect_equal(result$labels$title, "SPEI (grouped by year)")
})

test_that("by_year = TRUE with no title still works and shows the suffix", {
  result <- plot_drought_ts(df = make_ts_df(), vname = "spei", by_year = TRUE)
  expect_equal(result$labels$title, "(grouped by year)")
})



