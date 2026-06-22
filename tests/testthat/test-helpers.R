# create aux objects
make_df_year_month <- function() {
  data.frame(year = c(2020, 2020), month = c(1, 2), index = c(0.5, -1.2))
}

make_df_date <- function() {
  data.frame(date = as.Date(c("2020-01-01", "2020-02-01")), index = c(0.5, -1.2))
}

test_that(".check_date is a data frame", {
  expect_error(
    .check_date(list(year = 2020, month = 1)),
    "must be a data frame or tibble"
  )
})

test_that(".check_date errors if only year is present (month missing)", {
  df <- data.frame(year = 2020, index = 0.5)
  expect_error(
    .check_date(df),
    "missing month"
  )
})

test_that(".check_date errors if only month is present (year missing)", {
  df <- data.frame(month = 3, index = 0.5)
  expect_error(
    .check_date(df),
    "missing year"
  )
})

test_that(".check_date returns df unchanged when year and month are both present", {
  df <- make_df_year_month()
  result <- .check_date(df)
  expect_identical(result, df)
})


test_that(".check_date derives year and month from a Date column", {
  df <- make_df_date()
  result <- .check_date(df)

  expect_true(all(c("year", "month") %in% names(result)))
  expect_equal(result$year, c(2020, 2020))
  expect_equal(result$month, c(1, 2))
})

test_that(".check_date errors if there is no year/month nor a valid date column", {
  df <- data.frame(index = c(0.5, -1.2))
  expect_error(
    .check_date(df),
    "must contain.*year.*month.*date"
  )
})

test_that(".check_date errors if date column exists but is not Date/POSIXct", {
  df <- data.frame(date = c("2020-01-01", "2020-02-01"), index = c(0.5, -1.2))
  expect_error(
    .check_date(df),
    "must contain.*year.*month.*date"
  )
})
