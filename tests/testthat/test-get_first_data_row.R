context("get_first_data_row")

test_that("get_first_data_row returns expected output", {
  blank_row <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/get_first_data_row_testdat.xlsx", sheets = "blank_row")
  numbers_in_header <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/get_first_data_row_testdat.xlsx", sheets = "numbers_in_header")
  missing_row <- filter(blank_row, row != 3)
  too_few_data_cols <- filter(blank_row, col != 4)
  
  expect_equal(get_first_data_row(blank_row), 2)
  expect_equal(get_first_data_row(numbers_in_header), 6)
  expect_equal(get_first_data_row(missing_row), 2)
  
  expect_error(get_first_data_row(too_few_data_cols))
  
})
