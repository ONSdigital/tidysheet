test_that("get_col_letters_as_string gives expected results", {

  dat <- data.frame(
    "col" = c(1:3, 1, 4, 27),
    "address" = c("A1", "B1", "C1", "A2", "", "AA1")
  )

  result_two_different <- get_col_letters_as_string(dat, c(1, 3))
  result_two_same <- get_col_letters_as_string(dat, 1)
  result_one <- get_col_letters_as_string(dat, 27)
  result_blank_address <- get_col_letters_as_string(dat, 4)
  result_no_address <- get_col_letters_as_string(dat, 50)

  expect_equal(result_two_different, "A, C")
  expect_equal(result_two_same, "A")
  expect_equal(result_one, "AA")
  expect_equal(result_blank_address, "")

})
