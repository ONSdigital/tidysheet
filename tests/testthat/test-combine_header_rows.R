context("combine_header_rows")

test_that("combine_header_rows function works as expected", {
  
  # Sample data for testing looks like this in standard format:
  
  # |data in 2020     ||         ||         |
  # |      || this    ||         || and     |
  # |      ||---------||---------||---------|
  # |      || is one  || one     || another |
  # |      ||---------||---------||---------|
  # | year || heading || here    ||         |
  # |======||=========||=========||=========|
  # | 2020 ||   1     ||  2      ||   3     |
  
  # in xlsx_cells format (order is important)
  dat <- data.frame(
    "row" = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5),
    "col" = c(1, 2, 4, 2, 3, 4, 1, 2, 3, 1, 2, 3, 4),
    "character" = c( "data in 2020", "this", "and", "is one", "one", "another", "year", "heading", "here", rep(NA,4)),
    "numeric" = c(rep(NA, 9), 2020, 1:3),
    "data_type" = c(rep("character", 9), rep("numeric", 4))
  )
  
  # expected result in standard format
  # |data in 2020                ||          ||             |
  # |      || this is one heading||          || and another |
  # |      ||--------------------||----------||-------------|
  # |      ||                    || one here ||             |
  # |      ||--------------------||----------||-------------|
  # | year ||                    ||          ||             |
  # |======||====================||==========||=============|
  # | 2020 ||        1           ||   2      ||      3      |
  
  # in xlsx_cells format (order is important)
  expected_result <- data.frame(
    "row" = c(1, 2, 2, 3, 4, 5, 5, 5, 5),
    "col" = c(1, 2, 4, 3, 1, 1, 2, 3, 4),
    "character" = c( "data in 2020", "this is one heading", "and another", "one here", "year", rep(NA,4)),
    "numeric" = c(rep(NA, 5), 2020, 1:3),
    "data_type" = c(rep("character", 5), rep("numeric", 4))
  )
  
  # use `rectify(result)` if you want to see the result in standard layout
  result <- combine_header_rows(dat, "this", "here")
  expect_equal(result, expected_result)
  
})
