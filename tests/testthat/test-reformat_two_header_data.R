context("reformat_two_header_data")

test_that('reformat_two_header_data returns expected output',{
  
  left_headers_low <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/two_row_header_testdat.xlsx", sheets = "left_low")
  left_headers_high <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/two_row_header_testdat.xlsx", sheets = "left_high")
  no_left_headers <- left_headers_low %>% 
    filter(character %in% c("LA", "Region") == FALSE)
  one_left_header <- left_headers_low %>% 
    filter(character %in% c("Region") == FALSE)
  
  
  actual_left_headers_low <- reformat_two_header_data(
    dat=left_headers_low,
    group_col="service", 
    nested_column_1="subservice", 
    left_headers = c("LA", "Region")
  ) %>% 
    select(numeric, character, service, subservice, LA, Region)
  
  actual_left_headers_high <- reformat_two_header_data(
    dat=left_headers_low,
    group_col="service", 
    nested_column_1="subservice", 
    left_headers = c("LA", "Region")
  ) %>% 
    select(numeric, character, service, subservice, LA, Region)
  
  actual_no_left_headers_given <- reformat_two_header_data(
    dat=left_headers_high,
    group_col="service", 
    nested_column_1="subservice",
    left_headers=NA
  ) %>% 
    select(numeric, character, service, subservice, LA, Region)
  
  actual_no_left_headers <- reformat_two_header_data(
    dat=no_left_headers,
    group_col="service", 
    nested_column_1="subservice",  
    left_headers=NA
  ) %>% 
    select(numeric, character, service, subservice, column_1, column_2)
  
  actual_one_left_header <- suppressWarnings(
    reformat_two_header_data(
      dat=left_headers_low,
      group_col="service", 
      nested_column_1="subservice", 
      left_headers=c("LA")
    )) %>% 
    select(numeric, character, service, subservice, LA, Region)
  
  
  expected_output <- data.frame(
    numeric = c(1, 2, 5, 6, 3, 4, 7, 8),
    character = as.character(NA),
    service = rep(c('Education', 'Transport'), each = 4),
    subservice = c(rep(c('primary', 'secondary'), 2), rep(c('bus', 'rail'), 2)),
    LA = rep(c('Cornwall','Sussex'), each=2),
    Region = rep(c('SW','SE'), each=2)
  )
  expected_output_no_left_headers <- expected_output %>% 
    rename(column_1 = LA,
           column_2 = Region)
  
  expect_equal(actual_left_headers_low, dplyr::as_tibble(expected_output))
  expect_equal(actual_left_headers_high, dplyr::as_tibble(expected_output))
  expect_equal(actual_no_left_headers_given, dplyr::as_tibble(expected_output))
  expect_equal(actual_no_left_headers, dplyr::as_tibble(expected_output_no_left_headers))
  expect_equal(actual_one_left_header, dplyr::as_tibble(expected_output))
})
