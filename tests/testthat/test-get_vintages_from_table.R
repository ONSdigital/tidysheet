test_that("get_vintages_from_table returns NA and a warning if dat is too large", {
    dat <- data.frame(
      row = rep(1:21, each = 2),
      col = rep(1:2, times = 21),
      character = NA
      )

  expect_warning(
    result <- suppressMessages(get_vintages_from_table(dat)),
    "dat is larger than expected so will be ignored for vintage"
    )

  expect_equal(result, NA)

})


test_that("get_vintages_from_table returns expected output when a single vintage is found", {

  dat <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    character = c("Title of the data", NA, "forecast data", NA)
  )
  result <- get_vintages_from_table(dat)
  expect_equal(result, "budget")

})


test_that("get_vintages_from_table returns expected output when one vintage is found multiple times", {

  dat <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    character = c("Title of the forecast data", NA, "budget data", NA)
  )
  result <- get_vintages_from_table(dat)
  expect_equal(result, "budget")

})


test_that("get_vintages_from_table returns NA and a warning when different vintages are found", {

  dat <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    character = c("Title of the final data", NA, "budget data", NA)
  )
  expect_warning(
    result <- get_vintages_from_table(dat),
    "More than one.*'final', 'budget'."
  )
  expect_equal(result, NA)

})
