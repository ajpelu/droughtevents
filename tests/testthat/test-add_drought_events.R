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

test_that("returns a ggplot object on valid input", {
  result <- add_drought_events(
    p = make_base_plot(),
    drought_assessment = make_drought_assessment()
  )
  expect_s3_class(result, "ggplot")
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

test_that("drought_assessment has required names col minyear",{
  x <- make_drought_assessment()
  names(x)[names(x) == "minyear"] <- "year"
  expect_error(
    add_drought_events(p = make_base_plot(), drought_assessment = x),
    "missing required column"
  )
})

test_that("drought_assessment has required names col month_peak",{
  x <- make_drought_assessment()
  names(x)[names(x) == "month_peak"] <- "mp"
  expect_error(
    add_drought_events(p = make_base_plot(), drought_assessment = x),
    "missing required column"
  )
})

test_that("drought_assessment has required names col d_duration",{
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

test_that("pol_alpha is a number within 0 and 1 both included (lower than 0)", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      pol_alpha = -.5
    ),
    "must be a number between 0 and 1"
  )
})

test_that("pol_alpha is a number within 0 and 1 both included (higher than 1)", {
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

test_that("pol_fill is a string", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      pol_fill = 1
    ),
    "must be a single character string"
  )
})

test_that("pol_fill error if length is greater than 1", {
  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = make_drought_assessment(),
      pol_fill = c("black", "green")
    ),
    "must be a single character string"
  )
})


test_that("show_severity = TRUE requires a d_severity column", {
  da <- make_drought_assessment()
  da$d_severity <- NULL

  expect_error(
    add_drought_events(
      p = make_base_plot(),
      drought_assessment = da,
      show_severity = TRUE
    ),
    "requires a.*d_severity.*column"
  )
})


test_that("errors if the metric column is missing, for each metric option", {
  base_da <- make_drought_assessment()
  p <- make_base_plot()

  # duration -> d_duration es una columna OBLIGATORIA desde el principio,
  # así que su ausencia dispara el check de required_cols, no el de metric_col
  da <- base_da
  da$d_duration <- NULL
  expect_error(
    add_drought_events(p, da, which_events = "top", metric = "duration"),
    "missing required column"
  )

  # severity -> d_severity (no es obligatoria de entrada, así que SÍ llega al check de metric_col)
  da <- base_da
  da$d_severity <- NULL
  expect_error(
    add_drought_events(p, da, which_events = "top", metric = "severity"),
    "requires column.*d_severity"
  )

  # intensity -> d_intensity
  da <- base_da
  da$d_intensity <- NULL
  expect_error(
    add_drought_events(p, da, which_events = "top", metric = "intensity"),
    "requires column.*d_intensity"
  )

  # lowest_index -> lowest_spei
  da <- base_da
  da$lowest_spei <- NULL
  expect_error(
    add_drought_events(p, da, which_events = "top", metric = "lowest_index"),
    "requires column.*lowest_spei"
  )
})


test_that("type = 'line' adds one layer per drought event", {
  p <- make_base_plot()
  da <- make_drought_assessment()
  result <- add_drought_events(p, da, type = "line")
  expect_equal(length(result$layers) - length(p$layers), nrow(da))
})

test_that("type = 'polygon' adds one layer per drought event", {
  p <- make_base_plot()
  da <- make_drought_assessment()
  result <- add_drought_events(p, da, type = "polygon")
  expect_equal(length(result$layers) - length(p$layers), nrow(da))
})

test_that("type = 'both' adds two layers per drought event", {
  p <- make_base_plot()
  da <- make_drought_assessment()
  result <- add_drought_events(p, da, type = "both")
  expect_equal(length(result$layers) - length(p$layers), 2 * nrow(da))
})

test_that("show_severity = TRUE adds two extra layers per event", {
  p <- make_base_plot()
  da <- make_drought_assessment()
  result_without <- add_drought_events(p, da, type = "line", show_severity = FALSE)
  result_with    <- add_drought_events(p, da, type = "line", show_severity = TRUE)
  expect_equal(
    length(result_with$layers) - length(result_without$layers),
    2 * nrow(da)
  )
})

test_that("which_events = 'top' keeps only top_n events", {
  p <- make_base_plot()
  da <- make_drought_assessment()
  result <- add_drought_events(p, da,
    which_events = "top",
    metric = "severity",
    top_n = 2,
    type = "line"
  )
  expect_equal(length(result$layers) - length(p$layers), 2)
})

test_that("pol_alpha accepts the boundary values 0 and 1 without error", {
  p <- make_base_plot()
  da <- make_drought_assessment()
  expect_no_error(add_drought_events(p, da, pol_alpha = 0))
  expect_no_error(add_drought_events(p, da, pol_alpha = 1))
})

test_that("show_severity accepts TRUE and FALSE without error", {
  p <- make_base_plot()
  da <- make_drought_assessment()
  expect_no_error(add_drought_events(p, da, show_severity = TRUE))
  expect_no_error(add_drought_events(p, da, show_severity = FALSE))
})

test_that("top_n equal to the number of available events is valid", {
  p <- make_base_plot()
  da <- make_drought_assessment()  # 3 eventos
  expect_no_error(
    add_drought_events(p, da, which_events = "top", top_n = 3)
  )
})

test_that("line_col and pol_fill accept valid single color strings", {
  p <- make_base_plot()
  da <- make_drought_assessment()
  expect_no_error(add_drought_events(p, da, line_col = "red", pol_fill = "blue"))
})

test_that("which_events = 'top' actually keeps the highest-severity events", {
  p <- make_base_plot()
  da <- make_drought_assessment()
  # severity: 1990 -> 1.5, 1995 -> 3.2, 2000 -> 0.8

  result <- add_drought_events(
    p, da,
    which_events = "top",
    metric = "severity",
    top_n = 2,
    type = "line"
  )

  new_layers <- result$layers[(length(p$layers) + 1):length(result$layers)]
  plotted_dates <- vapply(new_layers, function(l) as.character(l$data$x), character(1))

  # The most severity events are 1995-07 (3.2) and 1990-03 (1.5)
  expect_setequal(plotted_dates, c("1995-07-01", "1990-03-01"))

  # The less severity events is 2000-11 (0.8)
  expect_false("2000-11-01" %in% plotted_dates)
})

test_that("metric = 'lowest_index' works in the happy path", {
  p <- make_base_plot()
  da <- make_drought_assessment()
  expect_no_error(
    add_drought_events(p, da, which_events = "top", metric = "lowest_index", top_n = 2, type = "line")
  )
})

test_that("which_events = 'top', type = 'both', and show_severity = TRUE work together", {
  p <- make_base_plot()
  da <- make_drought_assessment()
  result <- add_drought_events(
    p, da,
    which_events = "top",
    metric = "duration",
    top_n = 2,
    type = "both",
    show_severity = TRUE
  )

  # top_n = 2 events .
  # type = "both" -> 2 layers (rect + segment) per event .
  # show_severity = TRUE -> 2 extra layer (point + text) per evento.
  # Total expected: 2 events x (2 + 2) layer = 8.
  expect_equal(length(result$layers) - length(p$layers), 8)
})
