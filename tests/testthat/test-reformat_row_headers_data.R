context("reformat_row_headers_data")

test_that('reformat_row_headers_data returns expected output',{
  
  sample_data <- as_tibble(
    data.frame(
      address = c('B2', 'C2', 'A3', 'B3', 'C3', 
                  'A4', 'B4', 'C4', 'A5', 'B5', 'C5', 
                  'A6', 'B6', 'C6', 'A7', 'B7', 'C7'),
      row = c(2, 2, rep(3:7, each = 3)),
      col = c(2, 3, rep(1:3, 5)),
      data_type = c(rep('character', 3), rep('blank', 2), 'character',
                    rep(c(rep('numeric', 2), 'character'), 2), rep('blank', 2),
                    'character', rep('numeric', 2)),
      numeric = c(rep(NA, 6), 1, 2, NA, 3, 4, rep(NA, 4), 5, 6),
      character = c('measure 1', 'measure 2', 'education', rep(NA, 2), 'primary', rep(NA, 2),
                    'secondary', rep(NA, 2), 'Transport', rep(NA, 2), 'roads', rep(NA, 2))
    )
  )%>% 
    mutate(is_blank = ifelse(data_type == "blank", TRUE, FALSE))
  
  expected_output <- dplyr::tibble(
    data.frame(address = c('B3', 'C3', 'B4', 'C4', 'B5', 'C5', 'B6', 'C6', 'B7', 'C7'),
               row = as.double(c(rep(3:7, each = 2))),
               col = as.double(c(rep(2:3, times = 5))),
               data_type = c(rep('blank', 2), rep('numeric', 4), rep('blank', 2), rep('numeric', 2)),
               numeric = as.double(c(rep(NA, 2), 1:4, rep(NA, 2), 5:6)),
               character = rep(as.character(NA), 10),
               subservice = rep(c('education','primary', 'secondary',
                                  'Transport', 'roads'), each = 2),
               transaction = rep(c('measure 1', 'measure 2'), times = 5),
               service = c(rep('education', times = 6), rep('Transport', times = 4)))
  )
  
  output <- reformat_row_headers_data(sample_data, 
                                      'service', 
                                      'subservice', 'transaction') %>% 
    select(-is_blank)
  
  expect_equal(output, expected_output)
})