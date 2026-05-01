test_that("rename_split_columns assigns new names correctly regardless of order", {

  expected_1 <- data.frame(year = "2020", letter = "B", number = "10")
  expected_2 <- data.frame(year = "2020", number = "10", letter = "B")

  to <- c("letter", "number")

  #-----
  dat_1 <- data.frame(year = "2020", tmp_string_1 = "B", tmp_string_2 = "10")
  result_1 <- rename_split_columns(dat_1, to)

  expect_equal(result_1, expected_1)

  #-----
  # switch order so tmp_string_2 appears in the data first
  dat_2 <- data.frame(year = "2020", tmp_string_2 = "10", tmp_string_1 = "B")
  result_2 <- rename_split_columns(dat_2, to)

  expect_equal(result_2, expected_2)

})

test_that("rename_split_columns fails gracefully when no columns start with tmp_string", {

  to <- c("letter", "number")

  #-----
  dat <- data.frame(year = "2020", letter = "B", number = "10")
  result <- suppressWarnings(rename_split_columns(dat, to))

  expect_equal(dat, result)
  expect_warning(rename_split_columns(dat, to), "none found")

})
