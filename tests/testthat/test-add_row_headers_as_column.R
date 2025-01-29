context("add_row_headers_as_column")

test_that("add_row_headers_as_column gives expected result", {
  
  sample_data_down <- data.frame(
    address = c('B3', 'B4', 'B5', 'B6', 'B7'),
    row = c(3:7),
    col = 2,
    is_blank = c(TRUE, rep(FALSE, 2), TRUE, FALSE),
    data_type = c('blank', rep('numeric', 2), 'blank', 'numeric'), 
    numeric = c(NA, 1, 3, NA, 5),
    subservice = c('education', 'primary',  'secondary', 'Transport', 'roads')
  )
  
  sample_data_up <- data.frame(
    address = c('B3', 'B4', 'B5', 'B6', 'B7'),
    row = c(3:7),
    col = 2,
    is_blank = c(rep(FALSE, 2), TRUE, FALSE, TRUE),
    data_type = c(rep('numeric', 2), 'blank', 'numeric', 'blank'), 
    numeric = c( 1, 3, NA, 5, NA),
    subservice = c( 'primary',  'secondary', 'education', 'roads', 'Transport')
  )  
  
  sample_data_na_col <- data.frame(
    address = c('B3', 'B4', 'B5', 'B6', 'B7'),
    row = c(3:7),
    col = 2,
    is_blank = c(TRUE, rep(FALSE, 2), TRUE, FALSE),
    data_type = c('blank', rep('numeric', 2), 'blank', 'numeric'), 
    numeric = c(NA, 1, 3, NA, 5),
    subservice = c('education', 'primary',  'secondary', 'Transport', 'roads'),
    transaction = c(NA, "CoE", "CoE", NA, "CoE")
  )
  
  expected_result_down <- sample_data_down
  expected_result_down$service <- c(rep('education', 3), rep('Transport', 2))
  
  expected_result_up <- sample_data_up
  expected_result_up$service <- c(rep('education', 3), rep('Transport', 2))
  
  result_down <- add_row_headers_as_column(sample_data_down, 'service', 'subservice', 'down')
  result_up <- add_row_headers_as_column(sample_data_up, 'service', 'subservice', 'up')
  
  expect_equal(expected_result_down, result_down)
  expect_equal(expected_result_up, result_up)
})
