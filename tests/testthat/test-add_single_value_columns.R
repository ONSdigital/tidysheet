test_that("add_single_value_columns raises an error if column names and column
fillers are of different lengths", {

  dat <- data.frame(a = 1:3, b = 4:6)
  expect_error(suppressMessages(
      add_single_value_columns(dat, c("desc_1", "desc_2"), "hello")
      ), "Names and values must be the same length"
  )

})


test_that("add_single_value_columns creates NA as a column filler if it is 'NA'", {

  name <- "description"
  filler <- "NA"

  dat <- data.frame(a = 1:3, b = 4:6)
  expected <- mutate(dat, description = NA)
  result <- suppressMessages(add_single_value_columns(dat, name, filler))

  expect_equal(result, expected)

})


test_that("add_single_value_columns works correctly for a single new column", {

  name <- "description"
  filler <- "net current expenditure"

  dat <- data.frame(a = 1:3, b = 4:6)
  expected <- mutate(dat, description = "net current expenditure")
  result <- suppressMessages(add_single_value_columns(dat, name, filler))

  expect_equal(result, expected)

})

test_that("add_single_value_columns works correctly for multiple new columns", {

  column_names <- c("description", "geography")
  fillers <- c("net current expenditure", "UK")

  dat <- data.frame(a = 1:3, b = 4:6)

  expected <- dat %>%
    mutate(description = "net current expenditure",
           geography = "UK")

  result <- suppressMessages(
    add_single_value_columns(dat, column_names, fillers)
  )

  expect_equal(result, expected)

})


test_that("add_single_value_columns does nothing when neither arg is specified", {
  column_names <- NA
  fillers <- NA

  dat <- data.frame(a = 1:3, b = 4:6)

  result <- suppressMessages(
    add_single_value_columns(dat, column_names, fillers)
  )

  expect_equal(result, dat)
})


test_that("add_single_value_columns handles a missing value or missing name", {

  dat <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    suppressMessages(add_single_value_columns(dat, NA, "some value"))
  )
  expect_error(
    suppressMessages(add_single_value_columns(dat, "some column", NA))
  )

})


test_that("add_single_value_columns does not overwrite existing data", {

  dat <- data.frame("vintage" = "final")

  expect_warning(
    result <- suppressMessages(
      add_single_value_columns(dat, "vintage", "budget")
    ),
    "'vintage' is specified, but it already contains values in the data"
  )

  expect_equal(result, dat)

})

