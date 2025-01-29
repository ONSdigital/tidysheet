context("split_column_by_pattern")

testthat::test_that("split_column_by_pattern gives expected output", {
  # testing splitting columns by using multiple patterns 
  
  test_dataset <- tibble::tibble(
    "constant" = c("no pattern", "one pattern", "two pattern",
                   "pattern variation", "pattern at start",
                   "pattern at end" ),
    "info" = c("chair", "chair \r\n (falling)", "chair \r\n (falling) \r\n (rising)",
               "chair \r\n \r\n (falling)", "\r\n (falling)",
               "chair \r\n (")
  )
  
  expected_result <- tibble::tibble(
    "constant" = c("no pattern", "one pattern", "two pattern",
                   "pattern variation", "pattern at start",
                   "pattern at end" ),
    "info" = c("chair", "chair", "chair",
               "chair", "",
               "chair"), 
    "direction" = c("", "falling)", "falling) \r\n (rising)",
                    "falling)", "falling)",
                    "")
  )
  
  
  
  result <- split_column_by_pattern(dat = test_dataset, original_col = "info" , new_col = "direction", 
                                    pattern = "\r\n\\s*\\(")
  
  expect_equal(result, expected_result)
  
})


testthat::test_that("split_column_by_pattern but stops overwritting the same column name", {
  #testing if the column split throws an error when the new column name already exist in the data
  test_dataset <- tibble::tibble(
    "constant" = "one pattern",
    "info" = "chair \r\n (falling)",
    "direction" = "west"
  )
  
  result <- suppressWarnings(split_column_by_pattern(dat = test_dataset, original_col = "info" , new_col = "direction", 
                                                     pattern = "\r\n\\s*\\("))
  
  
  expect_equal(result,test_dataset)
  
  expect_warning(split_column_by_pattern(dat = test_dataset, original_col = "info" , new_col = "direction", 
                                         pattern = "\r\n\\s*\\("), 
                 "direction is already in dat")
  
})