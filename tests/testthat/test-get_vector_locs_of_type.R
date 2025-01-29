context("get_vector_locs_of_type")
test_that("get_vector_locs_of_type returns expected result", {
  
  dat <- data.frame(
    "col" = rep(1:4, each = 5),
    "row" = rep(1:5, times = 4),
    "data_type" = c(
      rep("numeric", 5),
      rep("character", 5), 
      c(rep("numeric", 3), 
        rep("blank", 2), 
        rep("numeric", 3), 
        rep("character", 2))
    ),
    "character" = c(rep(NA, 5), LETTERS[1:5], rep(NA, 8), "a", "b"),
    "numeric" = c(1:5, rep(NA, 5), 
                  rep(c(1:3, rep(NA, 2)), times = 2))
  )
  
  # dat looks like this in excel:
  #
  # | 1 | A | 1 | 1 |
  # | 2 | B | 2 | 2 |
  # | 3 | C | 3 | 3 |
  # | 4 | D |   | a |
  # | 5 | E |   | b |
  
  result_low_tol_col <- get_vector_locs_of_type(dat, "character", 0.3)
  expect_equal(result_low_tol_col, c(2, 4))
  
  result_mid_tol_col <- get_vector_locs_of_type(dat, "numeric", 0.6)
  expect_equal(result_mid_tol_col, c(1, 3, 4))
  
  result_high_tol_col <- get_vector_locs_of_type(dat, "numeric", 1)
  expect_equal(result_high_tol_col, 1)
  
  result_na_included_in_tolerance_calc <- get_vector_locs_of_type(dat, "numeric", 0.9)
  expect_equal(result_na_included_in_tolerance_calc, 1)
  
  result_blank_col <- get_vector_locs_of_type(dat, "blank", 0.3)
  expect_equal(result_blank_col, 3)
  
  result_row <- get_vector_locs_of_type(dat, "numeric", 0.6, "row")
  expect_equal(result_row, 1:3)
  
  result_none <- get_vector_locs_of_type(dat, "character", 0.8, "row")
  expect_equal(result_none, integer(0))
  
})
