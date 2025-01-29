context("reformat_three_header_data")

test_that('reformat_three_header_data returns expected output',{
  
  left_headers_low <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/three_row_header_testdat.xlsx", sheets = "left_low")
  left_headers_mid <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/three_row_header_testdat.xlsx", sheets = "left_mid")
  left_headers_high <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/three_row_header_testdat.xlsx", sheets = "left_high")
  no_left_headers <- left_headers_low %>% 
    filter(character %in% c("LA", "Region") == FALSE)
  one_left_header <- left_headers_low %>% 
    filter(character %in% c("Region") == FALSE)
  
  actual_left_headers_low <- reformat_three_header_data(
    dat=left_headers_low,
    left_headers = c("LA", "Region"),
    group_col="service", 
    nested_column_1="subservice", 
    nested_column_2="subservice_code"
  ) %>% 
    select(numeric, character, service, subservice_code, subservice, LA, Region)
  
  actual_left_headers_mid <- reformat_three_header_data(
    dat=left_headers_mid,
    left_headers = c("LA", "Region"),
    group_col="service", 
    nested_column_1="subservice", 
    nested_column_2="subservice_code"
  ) %>% 
    select(numeric, character, service, subservice_code, subservice, LA, Region) 
  
  actual_left_headers_high <- reformat_three_header_data(
    dat=left_headers_high,
    left_headers = c("LA", "Region"),
    group_col="service", 
    nested_column_1="subservice", 
    nested_column_2="subservice_code"
  ) %>% 
    select(numeric, character, service, subservice_code, subservice, LA, Region) 
  
  actual_no_left_headers_given <- reformat_three_header_data(
    dat=left_headers_high,
    left_headers=NA,
    group_col="service", 
    nested_column_1="subservice", 
    nested_column_2="subservice_code"
  ) %>% 
    select(numeric, character, service, subservice_code, subservice, LA, Region)
  
  actual_no_left_headers <- reformat_three_header_data(
    dat=no_left_headers,
    left_headers=NA,
    group_col="service", 
    nested_column_1="subservice", 
    nested_column_2="subservice_code"
  ) %>% 
    select(numeric, character, service, subservice_code, subservice, column_1, column_2)
  
  actual_one_left_header <- suppressWarnings(
    reformat_three_header_data(
      dat=left_headers_low,
      left_headers=c("LA"),
      group_col="service", 
      nested_column_1="subservice", 
      nested_column_2="subservice_code"
    )) %>% 
    select(numeric, character, service, subservice_code, subservice, LA, Region)
  
  expected_output <- data.frame(
    numeric = c(1, 5, 2, 6, 3, 7, 4, 8),
    character = as.character(NA),
    service = rep(c('Education', 'Transport'), each = 4),
    subservice_code = rep(c(123, 456, 789, 987), each = 2),
    subservice = rep(c('primary', 'secondary', 'bus', 'rail'), each = 2),
    LA = c('Cornwall','Sussex'),
    Region = c('SW','SE')
  )
  expected_output_mid <- mutate(expected_output, 
                                subservice_code = as.character(subservice_code))
  expected_output_no_left_headers <- expected_output %>% 
    rename(column_1 = LA,
           column_2 = Region)
  
  expect_equal(actual_left_headers_low, dplyr::as_tibble(expected_output))
  expect_equal(actual_left_headers_mid, dplyr::as_tibble(expected_output_mid))
  expect_equal(actual_left_headers_high, dplyr::as_tibble(expected_output))
  expect_equal(actual_no_left_headers_given, dplyr::as_tibble(expected_output))
  expect_equal(actual_no_left_headers, dplyr::as_tibble(expected_output_no_left_headers))
  expect_equal(actual_one_left_header, dplyr::as_tibble(expected_output))
  
})
