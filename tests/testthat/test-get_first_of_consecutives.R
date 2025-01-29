context("get_first_of_consecutives")

test_that("get_first_of_consecutives gives expected output", {
  
  beginning <- get_first_of_consecutives(c(1, 2, 3, 4))
  middle <- get_first_of_consecutives(c(1, 3, 4, 6))
  end <- get_first_of_consecutives(c(1, 3, 5, 6))
  multiple <- get_first_of_consecutives(c(1, 3, 4, 6, 7))
  none <- suppressWarnings(get_first_of_consecutives(c(1, 3, 5)))
  none_starting_later <- suppressWarnings(get_first_of_consecutives(c(3, 5)))
  
  expect_equal(beginning, 1)
  expect_equal(middle, 3)
  expect_equal(end, 5)
  expect_equal(multiple, 3)
  expect_equal(none, 1)
  expect_equal(none_starting_later, 3)
  
  expect_warning(get_first_of_consecutives(c(1, 3, 5)))
}) 
