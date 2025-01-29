context('sheet_matcher returns the correct sheet')

# Test case 1: No matching sheets
test_that("No matching sheets should return NULL", {
  matching_sheets <- list()
  result <- sheet_matcher(matching_sheets)
  expect_is(result, "NULL")
})

# Test case 2: One matching sheet
test_that("One matching sheet should return the sheet name", {
  matching_sheets <- list("Sheet1")
  result <- sheet_matcher(matching_sheets)
  expect_equal(result, "Sheet1")
})

# Test case 3: Multiple matching sheets
test_that("Multiple matching sheets should return the first instance in matching_sheets", {
  matching_sheets <- list("Sheet1", "Sheet2")
  result <- suppressWarnings(sheet_matcher(matching_sheets))
  expect_equal(result, "Sheet1")
  expect_warning(sheet_matcher(matching_sheets),"Multiple sheets match with the given regex pattern. The first match has been used. If the first match is the wrong sheet, please contact a developer so that they can correct.")
})