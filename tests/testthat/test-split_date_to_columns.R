test_that("split_date_to_columns does nothing if column is empty", {

  dat <- data.frame(
    period = NA,
    value = c(1:2)
  )

  expect_warning(
    result <- suppressMessages(split_date_to_columns(dat, "period"))
  )

  expect_equal(result, dat)
})


test_that("split_date_to_columns does nothing if column is NA", {

  dat <- data.frame(
    period = c("2017-01-08", "2019-08-14"),
    value = c(1:2)
  )

  result <- split_date_to_columns(dat, NA)

  expect_equal(result, dat)

})


test_that("split_date_to_columns gives expected output", {

  dat <- data.frame(
    period = c("2017-01-08", "2019-08-14"),
    value = c(1:2)
  )

  expected <- data.frame(
    period = c("2017-01-08", "2019-08-14"),
    value = c(1:2),
    day = c(8, 14),
    month = c("Jan", "Aug"),
    year = c(2017, 2019)
  )
  actual <- suppressMessages(split_date_to_columns(dat, "period"))

  expect_equal(expected, actual)

})

test_that("split_date_to_columns fails with error when dates are ambiguous", {

  mixed_dates <- data.frame(
    period = c("08.01.2017", "2019-08-14"),
    value = c(1:2)
  )

  impossible_month <- data.frame(
    period = c("2017-15-01", "2019-08-14"),
    value = c(1:2)
  )

  slash <- data.frame(
    period = c("08/01/2017", "08/01/2017"),
    value = c(1:2)
  )

  dots <- data.frame(
    period = c("08.01.2017", "08.01.2017"),
    value = c(1:2)
  )

  expect_error(
    suppressMessages(split_date_to_columns(mixed_dates, "period")),
    "is not being read into R as an unambiguous date"
    )
  expect_error(
    suppressMessages(split_date_to_columns(impossible_month, "period")),
    "is not being read into R as an unambiguous date"
  )
  expect_error(
    suppressMessages(split_date_to_columns(slash, "period")),
    "is not being read into R as an unambiguous date"
    )
  expect_error(
    suppressMessages(split_date_to_columns(dots, "period")),
    "is not being read into R as an unambiguous date"
    )

})
