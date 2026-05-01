test_that("get_year_col_to_drop_from_quarter preferentially returns calendar year", {
  dat <- data.frame(
    year = c("2023", "2024"),
    financial_year = c("2023-24", "2023-24"),
    quarter = c("2023Q4", "2024Q1"),
    Value = c(1, 2)
  )
  expect_warning(
    result <- suppressMessages(get_year_col_to_drop_from_quarter(dat, "year")),
    "'year', 'financial_year'"
  )
  expect_equal(result, "year")

})


test_that("get_year_col_to_drop_from_quarter returns mixed over financial", {
  dat <- data.frame(
    year = c("2023", "2023-2024"),
    financial_year = c("2023-24", "2023-24"),
    quarter = c("2023Q4", "2023-24Q1"),
    Value = c(1, 2)
  )
  warnings <- capture_warnings(
    result <- suppressMessages(get_year_col_to_drop_from_quarter(dat, "year"))
  )
  expect_equal(
    warnings[1],
    "More than one year column matching the year col pattern identified: 'financial_year', 'year'."
  )
  expect_equal(
    warnings[2],
    "Some years have been identified as financial years. Check that year and quarter are as expected. If not, contact a developer."
  )
  expect_equal(result, "year")

})


test_that("get_year_col_to_drop_from_quarter returns financial if no calendar years found", {
  dat <- data.frame(
    financial_year = c("2023-24", "2023-24"),
    quarter = c("2023Q4", "2024Q1"),
    Value = c(1, 2)
  )

  result <- suppressMessages(get_year_col_to_drop_from_quarter(dat, "year"))
  expect_equal(result, "financial_year")

})


test_that("get_year_col_to_drop_from_quarter returns NA if no year", {
  dat <- data.frame(
    quarter = c("2023Q4", "2024Q1"),
    Value = c(1, 2)
  )

  warnings <- capture_warnings(
    suppressMessages(result <- get_year_col_to_drop_from_quarter(dat, "year"))
  )

  expect_equal(
    warnings[2],
    "No valid year column found, so year will not be dropped from the quarter column. Please contact a developer to update the settings."
  )

  expect_equal(result, NA)

})

