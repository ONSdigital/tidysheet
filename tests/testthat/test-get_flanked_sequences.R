context("get_flanked_sequences")

test_that("get_flanked_sequences returns expected result", {
  
  seq_1 <- c(13, 4, 10, 14, 15, 17)
  seq_2 <- c(3, 5, 6, 8, 9, 16, 12)
  
  expected_seq_1_target <- c(16)
  expected_seq_2_target <- c(4, 13, 14, 15)
  
  result_seq_1_target <- get_flanked_sequences(seq_1, seq_2)
  result_seq_2_target <- get_flanked_sequences(seq_2, seq_1)
  
  expect_equal(expected_seq_1_target, result_seq_1_target)
  expect_equal(result_seq_2_target, result_seq_2_target)
  
})