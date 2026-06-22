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


test_that("p is not a ggplot object", {
  expect_error(
    add_drought_events(p = "not a plot", drought_assessment = make_drought_assessment()),
    "must be a ggplot object"
  )
})

test_that("drought_assessment is not a data frame", {
  expect_error(
    add_drought_events(p = make_base_plot(), drought_assessment = list(a = 1)),
    "must be a data frame or tibble"
  )
})

test_that("drought_assessment has zero rows", {
  empty_df <- make_drought_assessment()[0, ]
  expect_error(
    add_drought_events(p = make_base_plot(), drought_assessment = empty_df),
    "has no rows"
  )
})

test_that("drought_assesment has required names col",{
  x <- make_drought_assessment()
  names(x)[names(x) == "minyear"] <- "year"
  expect_error(
    add_drought_events(p = make_base_plot(), drought_assessment = x),
    "missing required column"
  )
})

test_that("drought_assesment has required names col",{
  x <- make_drought_assessment()
  names(x)[names(x) == "month_peak"] <- "mp"
  expect_error(
    add_drought_events(p = make_base_plot(), drought_assessment = x),
    "missing required column"
  )
})

test_that("drought_assesment has required names col",{
  x <- make_drought_assessment()
  names(x)[names(x) == "d_duration"] <- "duration"
  expect_error(
    add_drought_events(p = make_base_plot(), drought_assessment = x),
    "missing required column"
  )
})

test_that("top_n is not numeric", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      which_events = "top",
      top_n = "five"
    ),
    "positive integer"
  )
})

test_that("top_n is below 0", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      which_events = "top",
      top_n = -1
    ),
    "positive integer"
  )
})

test_that("top_n is not a whole number", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      which_events = "top",
      top_n = 2.5
    ),
    "positive integer"
  )
})

test_that("top_n is higher than drought events", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      which_events = "top",
      top_n = 5
    ),
    "less than or equal to the number of drought events"
  )
})

test_that("pol_alpha is a number", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      which_events = "all",
      pol_alpha = "number"
    ),
    "must be a number"
  )
})

test_that("pol_alpha is a number within 0 and 1 both included", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      pol_alpha = -.5
    ),
    "must be a number between 0 and 1"
  )
})

test_that("pol_alpha is a number within 0 and 1 both included", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      pol_alpha = 1.5
    ),
    "must be a number between 0 and 1"
  )
})

test_that("show_severity is TRUE or FALSE", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      show_severity = "no"
    ),
    'must be "TRUE" or "FALSE"'
  )
})

test_that("show_severity error if NA", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      show_severity = NA
    ),
    'must be "TRUE" or "FALSE"'
  )
})

test_that("show_severity errors if length is greater than 1", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      show_severity = c(TRUE, FALSE)
    ),
    'must be "TRUE" or "FALSE"'
  )
})

test_that("line_col is a string", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      line_col = 1
    ),
    "must be a single character string"
  )
})

test_that("line_col error if length is greater than 1", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      line_col = c("black", "green")
    ),
    "must be a single character string"
  )
})


