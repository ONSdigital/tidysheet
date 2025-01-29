context("split_by_consecutives")

test_that("split_by_consecutives gives expected output", {
  ordered <- c(1, 1, 3, 5, 6, 7, 10, 23, 24)
  
  set.seed(001)
  random_order <- sample(ordered, 9)
  
  expected <- list(`1` = 1,
                   `2` = 1, 
                   `3` = 3, 
                   `4` = c(5:7),
                   `5` = 10,
                   `6` = c(23, 24))
  
  actual_ordered <- split_by_consecutives(ordered)
  actual_unordered <- split_by_consecutives(random_order)
  
  expect_equal(actual_ordered, expected)
  expect_equal(actual_unordered, expected)
  
})