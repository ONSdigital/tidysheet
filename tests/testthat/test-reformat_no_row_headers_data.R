context("reformat_no_row_headers_data")

test_that('reformat_no_row_headers_data returns expected output',{
  
  sample_data <- data.frame(
    address = c('B2', 'C2', 'A3', 'B3', 'C3', 'A4', 'B4', 'C4', 'A5', 'B5', 'C5'),
    row = c(2, 2, rep(3:5, each = 3)),
    col = c(2, 3, rep(1:3, 3)),
    data_type = c(rep('character', 3), 
                  rep(c(rep('numeric', 2), 'character'), 2), rep('numeric', 2)),
    numeric = c(rep(NA, 3), 1, 2, NA, 3, 4, NA, 5, 6),
    character = c('measure 1', 'measure 2', 'primary', rep(NA, 2),
                  'secondary', rep(NA, 2), 'Total education', rep(NA, 2))
  )
  
  bad_fomatted_total <- sample_data %>% 
    dplyr::mutate(character = ifelse(character == 'Total education', 
                                     ' ToTal   education ', character))
  
  expected_output <- dplyr::tibble(
    data.frame(address = c('B3', 'C3', 'B4', 'C4', 'B5', 'C5'),
               row = as.double(c(rep(3:5, each = 2))),
               col = as.double(c(rep(2:3, times = 3))),
               data_type = rep('numeric', 6),
               numeric = as.double(1:6),
               character = rep(as.character(NA), 6),
               subservice = rep(c('primary', 'secondary','total education'), each = 2),
               transaction = rep(c('measure 1', 'measure 2'), times = 3),
               service = c(rep('education', times = 6)))
  )
  
  output <- reformat_no_row_headers_data(sample_data, 'service', 'subservice', 'transaction')
  bad_fomatted_output <- reformat_no_row_headers_data(bad_fomatted_total, 
                                                      'service', 'subservice', 'transaction')
  expect_equal(output, expected_output)
  expect_equal(output, bad_fomatted_output)
})