# aux
make_drought_df <- function() {
  data.frame(
    year  = rep(1990:1992, each = 12),
    month = rep(1:12, times = 3),
    index = c(
      0, 0, 0, 0, -2, -2, -2, 0, 0, 0, 0, 0,   # 1990: drought at months 5-7 (3 consecutive)
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,       # 1991: no drought
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0        # 1992: no drought
    )
  )
}

test_that("df is a data frame", {
  expect_error(
    droughts(df = list(a = 1), vname = "index", threshold = -1),
    "must be a data frame or tibble"
  )
})

test_that("vname is a column in df", {
  expect_error(
    droughts(df = make_drought_df(), vname = "unnamed_column", threshold = -1),
    "not found in the data frame"
  )
})

test_that("vname column is numeric", {
  df <- make_drought_df()
  df$index <- as.character(df$index)
  expect_error(
    droughts(df = df, vname = "index", threshold = -1),
    "must be numeric"
  )
})

test_that("threshold is numeric", {
  expect_error(
    droughts(df = make_drought_df(), vname = "index", threshold = "no_number"),
    "threshold.*must be a numeric value"
  )
})

test_that("min_duration is numeric", {
  expect_error(
    droughts(df = make_drought_df(), vname = "index", threshold = -1, min_duration = "dos"),
    "min_duration.*must be a numeric value"
  )
})


test_that("droughts returns a list with class 'droughts' with right names", {
  result <- droughts(df = make_drought_df(), vname = "index", threshold = -1)

  expect_s3_class(result, "droughts")
  expect_named(result, c("data", "drought_events", "drought_assessment"))
})

test_that("droughts detects exactly one drought event with correct stats", {
  result <- droughts(df = make_drought_df(), vname = "index", threshold = -1)
  da <- result$drought_assessment

  expect_equal(nrow(da), 1)
  expect_equal(da$d_duration, 2)
  expect_equal(da$lowest_spei, -2)
  expect_equal(da$minyear, 1990)
  expect_equal(da$month_peak, 5)
})

test_that("droughts returns no events when nothing crosses the threshold", {
  df <- make_drought_df()
  df$index <- 0  # nothing below threshold

  result <- droughts(df = df, vname = "index", threshold = -1)

  expect_equal(nrow(result$drought_assessment), 0)
})


test_that("droughts returns no events when nothing crosses the threshold, without warnings", {
  df <- make_drought_df()
  df$index <- 0

  expect_no_warning(
    result <- droughts(df = df, vname = "index", threshold = -1)
  )
  expect_equal(nrow(result$drought_assessment), 0)
})


test_that("min_duration controls the minimum length required for an event", {
  df <- make_drought_df()

  # if min_duration = 2 (default), the may-jul 1990 event is considered
  result_default <- droughts(df = df, vname = "index", threshold = -1)
  expect_equal(nrow(result_default$drought_assessment), 1)

  # if min_duration = 3, the event is_drought = 1,1) ya NO debería contar
  result_strict <- droughts(df = df, vname = "index", threshold = -1, min_duration = 3)
  expect_equal(nrow(result_strict$drought_assessment), 0)
})


