context("get_vintage_from")
# extracts a string match used as vintage from a list of potential matches. It only does this if sheet_vintage is NA
test_that("get_vintage_from initial string positive match", {
  # Setup initial variables
  sheet_vintage <- "NA"
  vintage_check_item <- "2023/24 Forecast"
  expected_result <- "forecast"
  actual_result <- get_vintage_from(vintage_check_item, sheet_vintage)
  expect_equal(actual_result, expected_result)
  
  vintage_check_item <- "2023/24 Budget"
  expected_result <- "budget"
  sheet_vintage <- "NA"
  actual_result <- get_vintage_from(vintage_check_item, sheet_vintage)
  expect_equal(actual_result, expected_result)
})
test_that("get_vintage_from doesn't overwrite existing sheet_vintage", {
  # Setup initial variables
  sheet_vintage <- "Pre-existing-value" # this value should NOT be overwritten buy the function
  vintage_check_item <- "2023/24 Forecast"
  expected_result <- "Pre-existing-value"
  actual_result <- get_vintage_from(vintage_check_item, sheet_vintage)
  expect_equal(actual_result, expected_result)
})
test_that("get_vintage_from no match is found and NA is returned", {
  # Setup initial variables
  sheet_vintage <- "NA" 
  vintage_check_item <- "not a match"
  expected_result <- "NA"
  sheet_vintage <- get_vintage_from(vintage_check_item, sheet_vintage)
  expect_equal(expected_result, sheet_vintage)
})


