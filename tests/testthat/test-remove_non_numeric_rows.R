test_that("remove_non_numeric_rows removes rows without a numeric value", {

  dat <- data.frame(
    numeric = c(1, 3, 2, NA, NA, NA),
    unwanted_col = c(NA, NA, NA, "a", "a", NA),
    description_1 = c(NA, NA, "this", NA, NA, NA),
    description_2 = c("this", "that", NA, NA, NA, NA)
  )
  expected <- dat[1:3, ]
  result <- remove_non_numeric_rows(dat, "all_headers", c("col1", "col2"))
  expect_equal(result, expected)

})
