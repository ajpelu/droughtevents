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
