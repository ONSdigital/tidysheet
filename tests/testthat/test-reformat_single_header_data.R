context("reformat_single_header_data")

test_that('reformat_single_header_data returns expected output',{
  
  simple <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/single_row_header_testdat.xlsx", sheets = "simple")
  splits <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/single_row_header_testdat.xlsx", sheets = "splits")
  end_cols <- tidyxl::xlsx_cells("D:/coding_repos/pub_sec/R_test_data/single_row_header_testdat.xlsx", sheets = "end_cols")
  no_left_headers <- simple %>% 
    filter(character %in% c("LA", "Region") == FALSE)
  one_left_header <- simple %>% 
    filter(character %in% c("Region") == FALSE)
  
  
  actual_simple <- reformat_single_header_data(
    dat=simple,
    columns="subservice", 
    left_headers = c("LA", "Region"),
    first_header_row = 1,
    split_points = NA
  ) %>% 
    select(numeric, character, subservice, LA, Region)
  
  actual_splits <- reformat_single_header_data(
    dat=splits,
    columns=c("service", "subservice"), 
    left_headers = c("LA", "Region"),
    first_header_row = 1,
    split_points = "(\r\n|\r|\n)"
  ) %>% 
    select(numeric, character, service, subservice, LA, Region)
  
  actual_end_cols <- reformat_single_header_data(
    dat=end_cols,
    columns="subservice", 
    left_headers = c("LA", "Region"),
    first_header_row = 1,
    split_points = NA
  ) %>% 
    select(numeric, character, subservice, LA, Region, notes)
  
  actual_no_left_headers_given <- reformat_single_header_data(
    dat=simple,
    columns="subservice", 
    left_headers = NA,
    first_header_row = 1,
    split_points = NA
  ) %>% 
    select(numeric, character, subservice, LA, Region)
  
  actual_no_left_headers <- reformat_single_header_data(
    dat=no_left_headers,
    columns="subservice", 
    left_headers = NA,
    first_header_row = 1,
    split_points = NA
  ) %>% 
    select(numeric, character, subservice, column_1, column_2)
  
  actual_one_left_header <- suppressWarnings(
    reformat_single_header_data(
      dat=simple,
      columns="subservice", 
      left_headers = "LA",
      first_header_row = 1,
      split_points = NA
    )) %>% 
    select(numeric, character, subservice, LA, Region)
  
  
  expected_output <- data.frame(
    numeric = c(1, 2, 5, 6),
    character = as.character(NA),
    subservice = rep(c('primary education', 'secondary education'), 2),
    LA = rep(c('Cornwall','Sussex'), each=2),
    Region = rep(c('SW','SE'), each=2)
  )
  expected_output_splits <- data.frame(
    numeric = c(1, 2, 5, 6),
    character = as.character(NA),
    service = 'education',
    subservice = rep(c('primary', 'secondary'), 2),
    LA = rep(c('Cornwall','Sussex'), each=2),
    Region = rep(c('SW','SE'), each=2)
  )
  
  expected_output_end_cols <- expected_output %>% 
    mutate(notes = c("S", "S", NA, NA))
  expected_output_no_left_headers <- expected_output %>% 
    rename(column_1 = LA,
           column_2 = Region)
  
  
  expect_equal(actual_simple, dplyr::as_tibble(expected_output))
  expect_equal(actual_splits, dplyr::as_tibble(expected_output_splits))
  expect_equal(actual_end_cols, dplyr::as_tibble(expected_output_end_cols))
  expect_equal(actual_no_left_headers_given, dplyr::as_tibble(expected_output))
  expect_equal(actual_no_left_headers, dplyr::as_tibble(expected_output_no_left_headers))
  expect_equal(actual_one_left_header, dplyr::as_tibble(expected_output))
  
})
