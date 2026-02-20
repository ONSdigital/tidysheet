test_that("get_column_with_most_valid_years returns the string in the same position as the highest count", {
  expect_warning(
    result_second <- get_column_with_most_valid_years(c("y1", "y2"), c(2, 25)),
    "The column with the most valid year strings is 'y2'"
  )
  expect_equal(result_second, "y2")

  expect_warning(
    result_second <- get_column_with_most_valid_years(c("y1", "y2"), c(3, 2)),
    "The column with the most valid year strings is 'y1'"
  )
  expect_equal(result_second, "y1")
})


test_that("get_column_with_most_valid_years selects the first column when two columns that have the same frequency", {
  expect_warning(
    result <- get_column_with_most_valid_years(c("y1", "y2"), c(2, 2)),
    "More than one of these had the maximum number of valid years"
  )
  expect_equal(result, "y1")

})
