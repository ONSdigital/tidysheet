dat <- tibble(
  year = c(2020, NA, 2020, 2021, NA),
  value = c(100, NA, 200, NA, 300),
  description = c(NA, "B", NA, NA, "D"),
  address = c("A2", "A3", "A5", "B4", "B5"),
  row = c(2, 3, 5, 4, 5),
  col = c(1, 1, 1, 2, 2),
  stringsAsFactors = FALSE
)

test_that("drop_rows_with_NA returns original data when patterns is not provided (default NA)", {
  result <- drop_rows_with_NA(dat)
  expect_equal(result, dat) # Should return the original data
})


test_that("drop_rows_with_NA returns original data when patterns is an empty vector", {
  result <- drop_rows_with_NA(dat, c())
  expect_equal(result, dat)
})


test_that("drop_rows_with_NA returns original data when patterns contains no matching columns", {
  expect_warning(result <- suppressMessages(
    drop_rows_with_NA(dat, "nonexistent")
  ),
  "no columns.*match the pattern 'nonexistent'"
  )
  expect_equal(result, dat)
})


test_that("drop_rows_with_NA removes rows with NA in any of the matching columns and provides useful warnings", {
 warnings_year <- capture_warnings(
    result_year <- suppressMessages(
      drop_rows_with_NA(dat, patterns = "year")
    )
  )
  expect_equal(
    warnings_year[1], "The following source data rows have been removed: 3."
    )
  expect_equal(
    warnings_year[2],
    "The following cells have been removed even though there are values in cells in both the same column and row: 'B5'."
    )
  expected_year <- filter(dat, !address %in% c("A3", "B5"))
  expect_equal(result_year, expected_year)

  expect_warning(
    result_value <- suppressMessages(
      drop_rows_with_NA(dat, patterns = "(?i)VAL")
    ), "rows have been removed.*3, 4"
  )
  expected_value <- filter(dat, row %in% c(2, 5))
  expect_equal(result_value, expected_value)

  warnings_both <- capture_warnings(
    result_both <- suppressMessages(
      drop_rows_with_NA(dat, patterns = c("value", "year"))
    )
  )
  expect_equal(
    warnings_both[1], "The following source data rows have been removed: 3, 4."
  )
  expect_equal(
    warnings_both[2], "The following source data columns have been removed: B."
  )
  expected_both <- filter(dat, address %in% c("A2", "A5"))
  expect_equal(tibble(result_both), expected_both)

})


test_that("drop_rows_with_NA returns an empty dataframe if all rows are filtered out", {

  warnings <- capture_warnings(
    result <- suppressMessages(
      drop_rows_with_NA(dat, c("year", "value", "desc"))
    )
  )
  expect_equal(
    warnings[1],
    "The following source data rows have been removed: 2, 3, 5, 4."
    )
  expect_equal(
    warnings[2],
    "The following source data columns have been removed: A, B."
  )

  expect_equal(nrow(result), 0)
})


test_that("drop_rows_with_NA returns original data when no rows have NA in columns matching the pattern", {
  dat <- data.frame(
    year = c(2020, 2021),
    value = c(100, 200),
    description = c("A", "B"),
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(
    drop_rows_with_NA(dat, "year")
  )

  expect_equal(result, dat)
})
