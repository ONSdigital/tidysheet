#-------------------------------------------------------------------------------
context("get_first_header_row")
test_that("get_first_header_row works correctly", {
  # Load the common variables 
  simple <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/single_row_header_testdat.xlsx", sheets = "simple")
  file_part <- 1
  
  # Test case 1: Using header_identifier only, first row <- 1
  header_identifier <- c('LA')
  second_identifier <- NA
  
  # Test case: Expecting a specific message
  expect_message(
    result <- get_first_header_row(simple, header_identifier, second_identifier, file_part),
    "The first header row has been identified as row 1"
  )
  expect_equal(result, 1)
  
  
  # Test case 2: Using second_identifier only, first row <- 1
  header_identifier <- NA
  second_identifier <- c("Cornwall")
  
  # Test case: Expecting a specific message
  expect_message(
    result <- get_first_header_row(simple, header_identifier, second_identifier, file_part),
    "The first header row has been identified as row 1"
  )
  expect_equal(result, 1)
  
  # Test case 3: Using header_identifier only, first row <- 2
  header_identifier <- c("Cornwall")
  second_identifier <- NA
  
  # Test case: Expecting a specific message
  expect_message(
    result <- get_first_header_row(simple, header_identifier, second_identifier, file_part),
    "The first header row has been identified as row 2"
  )
  expect_equal(result, 2)
  
  # Test case 4: not found header
  header_identifier <- c("NOTFOUND")
  second_identifier <- NA
  
  # Test case: Expecting a specific message
  expect_error(
    result <- get_first_header_row(simple, header_identifier, second_identifier, file_part),
    "First header row has not been identified. Please see warnings."
  )
})