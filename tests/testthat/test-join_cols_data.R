# Unit tests

test_that("join_cols_data concatenates columns correctly", {
  dat <- tibble(
    col1 = c("A", "B", "C"),
    col2 = c("D", "E", "F"),
    col3 = c(NA, "G", "H") # this NA is returned as an empty string.
  )
  
  columns_to_concat <- c("col1", "col2", "col3")
  result <- join_cols_data(dat, columns_to_concat, col_join_name = "description")
  
  expected <- tibble(
    col1 = c("A", "B", "C"),
    col2 = c("D", "E", "F"),
    col3 = c("", "G", "H"), # so we expect the empty string to be returned.
    description = c("A D ", "B E G", "C F H")
  )
  
  expect_equal(result, expected)
})

test_that("join_cols_data handles empty columns_to_concat", {
  dat <- tibble(
    col1 = c("A", "B", "C"),
    col2 = c("D", "E", "F")
  )
  
  columns_to_concat <- c()
  result <- join_cols_data(dat, columns_to_concat, col_join_name = "description")
  
  expect_equal(result, dat)
})

test_that("join_cols_data handles missing values correctly", {
  dat <- tibble(
    col1 = c("A", NA, "C"),
    col2 = c("D", "E", NA)
  )
  
  columns_to_concat <- c("col1", "col2")
  result <- join_cols_data(dat, columns_to_concat, col_join_name = "description")
  
  expected <- tibble(
    col1 = c("A", NA, "C"),
    col2 = c("D", "E", NA),
    description = c("A D", " E", "C ")
  )
  
  expect_equal(result, expected)
})