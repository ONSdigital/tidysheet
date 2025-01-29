context("deal_with_left_columns")

test_that("deal_with_left_columns gives expected result", {
  # basic data where no un-pivotting has yet happened   
  sample_data_1 <- data.frame(
    address = c('A2', 'B2', 'C2'),
    row = c(2, 2, 2),
    col = c(1, 2, 3),
    is_blank = FALSE,
    data_type = c(rep('character', 2), 'numeric'),
    numeric = c(NA, NA, 1),
    character = c('England', 'Norfolk', NA))
  
  # data that is partially unpivotted (service code and subservice already unpivotted)
  sample_data_2 <- data.frame(
    address = c('C4', 'D4', 'A4', 'B4'),
    row = 4,
    col = c(3, 4, 1, 2),
    is_blank = FALSE,
    data_type = c(rep('numeric', 2), rep('character', 2)),
    numeric = c(10, 20, NA, NA),
    character = c(NA, NA, 'Cornwall', 'SW'),
    service = c(rep("Education", 2), NA, NA),
    code = c(123, 456, NA, NA),
    subservice = c("primary", "secondary", "LA", "Region"))
  
  expected_result_1 <- tibble(
    address = "C2",
    row = 2,
    col = 3,
    is_blank = FALSE,
    data_type = "numeric",
    numeric = 1,
    character = as.character(NA),
    A = "England",
    B = "Norfolk"
  )
  
  expected_result_2 <- tibble(
    address = c("C4", "D4"),
    row = 4,
    col = c(3, 4),
    is_blank = FALSE,
    data_type = "numeric",
    numeric = c(10, 20),
    character = as.character(NA),
    service = "Education",
    code = c(123, 456),
    subservice = c("primary", "secondary"),
    LA = "Cornwall",
    Region = "SW"
  )
  
  result_1 <- deal_with_left_columns(sample_data_1,  c("A", "B"))
  result_2 <- deal_with_left_columns(sample_data_2,  c("LA", "Region")) 
  
  expect_equal(expected_result_1, result_1)
  expect_equal(expected_result_2, result_2)
})
