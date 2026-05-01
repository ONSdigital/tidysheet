test_that("update_remaining removes information that has already been captured in tmp_string columns from 'tmp_remaining'", {


  dat <- data.frame(
    tmp_remaining = c("split\n 1.this", "example of some other split", "no splits done", NA),
    tmp_string_1 = c("split", "other split", "no splits done", NA)
  )

  expected <- dat %>%
    mutate(tmp_remaining = c("\n 1.this", "example of some ", "no splits done", NA))

  result <- update_remaining(dat, 3, "tmp_string_1", 1)

  expect_equal(expected, result)

  #----

  # and the same again, but using iteration 2
  dat_2 <- rename(dat, tmp_string_2 = tmp_string_1)
  expected_2 <- rename(expected, tmp_string_2 = tmp_string_1)

  result_2 <- update_remaining(dat_2, 3, "tmp_string_2", 2)

  expect_equal(expected_2, result_2)

  #----


})

test_that("update_remaining fails gracefully when required columns are not present", {


  dat <- data.frame(
    tmp_remaining = c("split\n 1.this", "example of some other split", "no splits done"),
    tmp_string_1 = c("split", "other split", "no splits done")
  )

  remaining_missing <- select(dat, -tmp_remaining)
  string_1_missing <- select(dat, -tmp_string_1)

  expect_error(
    update_remaining(remaining_missing, 3, "tmp_string_1", 1),
    "'tmp_remaining' should be a column name, but it was not found"
  )

  expect_error(
    update_remaining(string_1_missing, 3, "tmp_string_1", 1),
    "'tmp_string_1' should be a column name, but it was not found"
  )
})
