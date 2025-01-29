# Define the test
test_that("get_info_above_table works correctly", {
  # Create a sample data frame
  main_table <- data.frame(
    row = 1:10,
    character = c(NA, "A", "B", NA, "C", NA, "D", "E", NA, "F")
  )
  
  # Test case 1: Information found above the header row
  result1 <- get_info_above_table(main_table, 5)
  expect_equal(nrow(result1), 2)  # Rows 2 and 3 have non-NA characters
  expect_equal(result1$character, c("A", "B"))
  
  # Test case 2: No information found above the header row
  expect_warning(
    result2 <- get_info_above_table(main_table, 2),
    "No information found above the first header row. If there is info above the headers in the raw data, header_identifier in the data dictionary may need updating. Please contact a developer."
  )
  expect_equal(nrow(result2), 0)  # No rows with non-NA characters above row 2
  
  # Test case 3: Information found above the header row with different header_row value
  result3 <- get_info_above_table(main_table, 7)
  expect_equal(nrow(result3), 3)  # Rows 2, 3, 5, and 7 have non-NA characters
  expect_equal(result3$character, c("A", "B", "C"))
  
  # Test case 4: Read in an Excel sheet:
  
  some_info_above_header <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/single_row_header_testdat.xlsx", sheets = "info_above_header")
  result4 <- get_info_above_table(some_info_above_header, 3)
  expect_equal(nrow(result4), 6)
  
})
