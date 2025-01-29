context("concatenate_columns")

test_that("concatenate_columns returns expected output", {
  
  sample_data <- data.frame(
    "notes" = c('Note 1', NA, 'Note 2', NA),
    "info" = c(NA, "Info A", "Info B", NA)
  )
  
  expected_result <- data.frame(
    notes = c('Note 1', "Info A", 'Note 2 | Info B', NA)
  )
  
  result <- concatenate_columns(sample_data, "notes", "info")
  
  expect_equal(result, expected_result)
  
})

test_that("concatenate_columns returns expected errors", {
  
  sample_data <- data.frame(
    notes = c('Note 1', NA, 'Note 2', NA),
    info = c(NA, "Info A", "Info B", NA)
  )
  
  expect_error(concatenate_columns(sample_data, "not_a_col", "info"))
  expect_error(concatenate_columns(sample_data, "notes", "not_a_col"))
  expect_error(concatenate_columns(sample_data, "not_a_col_1", "not_a_col_2"))
  
})