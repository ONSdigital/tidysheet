context("get_left_headers")

test_that("get_left_headers gives the expected result", {
  
  sample_data <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/single_row_header_testdat.xlsx", sheets = "simple")
  space_blank_above_headers <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/two_row_header_testdat.xlsx", sheets = "left_low")
  space_blank_below_headers <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/two_row_header_testdat.xlsx", sheets = "left_high")
  
  first_right_header_col <- 3
  left_headers <- NA
  
  first_header_missing <- sample_data %>% 
    mutate(character = ifelse(
      character=="LA", NA,
      character
    ))
  
  second_header_missing <- sample_data %>% 
    mutate(character = ifelse(
      character=="Region", NA,
      character
    ))
  
  all_headers_missing <- first_header_missing %>% 
    mutate(character = ifelse(
      character=="Region", NA,
      character
    ))
  
  
  all_supplied_but_headers_in_data <- get_left_headers(sample_data, 3, c("this", "that"))
  all_supplied_first_header_missing <- get_left_headers(first_header_missing, 3, c("this", "that"))
  all_supplied_second_header_missing <- get_left_headers(second_header_missing, 3, c("this", "that"))
  none_supplied_first_header_missing <- get_left_headers(first_header_missing, 3, NA)
  all_supplied_all_headers_missing <- get_left_headers(all_headers_missing, 3, c("this", "that"))
  none_supplied_all_headers_missing <- get_left_headers(all_headers_missing, 3, NA)
  too_many_supplied_all_headers_missing <- suppressWarnings(
    get_left_headers(all_headers_missing, 3, c("this", "that", "the_other"))
  )
  too_few_supplied_all_headers_missing <- suppressWarnings(
    get_left_headers(all_headers_missing, 3, c("this"))
  )
  spaces_above <- get_left_headers(space_blank_above_headers, 3, NA)
  spaces_below <- get_left_headers(space_blank_below_headers, 3, NA)
  
  expect_equal(all_supplied_but_headers_in_data, c("LA", "Region"))
  expect_equal(all_supplied_first_header_missing, c("column_1", "Region"))
  expect_equal(all_supplied_second_header_missing, c("LA", "column_2"))
  expect_equal(none_supplied_first_header_missing, c("column_1", "Region"))
  expect_equal(all_supplied_all_headers_missing, c("this", "that"))
  expect_equal(none_supplied_all_headers_missing, c("column_1", "column_2"))
  expect_equal(too_many_supplied_all_headers_missing, c("column_1", "column_2"))
  expect_equal(too_few_supplied_all_headers_missing, c("column_1", "column_2"))
  expect_equal(spaces_above, c("LA", "Region"))
  expect_equal(spaces_below, c("LA", "Region"))
  
  expect_warning(get_left_headers(all_headers_missing, 3, c("this", "that", "the_other")))
  expect_warning(get_left_headers(all_headers_missing, 3, c("this")))
  
})