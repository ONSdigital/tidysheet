test_that("trim_separators_from_new_cols removes colons and dashes from the 'tmp_string_' columns", {

  dat <- data.frame(
    tmp_other = "- remove none :",
    tmp_string_1 = "-Remove dash -",
    tmp_string_2 = ": Remove_colon:",
    tmp_string_3 = "- remove both :",
    some_other_column = ": remove none -"
  )
  expected <- data.frame(
    tmp_other = "- remove none :",
    tmp_string_1 = "Remove dash",
    tmp_string_2 = "Remove_colon",
    tmp_string_3 = "remove both",
    some_other_column = ": remove none -"
  )

  result <- trim_separators_from_new_cols(dat)
  expect_equal(expected, result)

})

test_that("trim_separators_from_new_cols returns dat when no columns to remove", {

  dat <- data.frame(
    tmp_other = "- remove none :",
    some_other_column = ": remove none -"
  )

  result <- trim_separators_from_new_cols(dat)
  expect_equal(dat, result)

})
