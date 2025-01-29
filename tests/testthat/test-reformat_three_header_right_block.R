context("reformat_three_header_right_block")

test_that("reformat_three_header_right_block gives correct result", {
  sample_data <- data.frame(
    address = c('C1', 'D1', 'C2', 'D2', 'C3', 'D3', 'C4', 'D4'),
    col = rep(c(3:4), times = 4),
    row = rep(c(1:4), each = 2),
    is_blank = c(FALSE, TRUE, FALSE, TRUE, rep(FALSE, 4)),
    data_type = c(rep(c('character', 'blank'), 2) , rep('character', 2),
                  rep('numeric', 2)),
    character = c('Capital', NA, 'Total', NA, 'Payments', 'Receipts', NA, NA),
    numeric = c(rep(NA, 6), 1, 2)
  )
  
  expected_output <- tibble(data.frame(
    address = c("C4", "D4"),
    col = as.integer(c(3, 4)),
    row = as.integer(4),
    is_blank = FALSE,
    data_type = "numeric",
    character = as.character(NA),
    numeric = c(1, 2),
    district_or_capital = "Capital",
    measure_group = "Total",
    measure = c("Payments", "Receipts")
  ))
  
  output <- reformat_three_header_right_block(main_table = sample_data,
                                              group_col = "district_or_capital",
                                              nested_column_1 = "measure",
                                              nested_column_2 = "measure_group")
  
  expect_equal(expected_output, output)
})
