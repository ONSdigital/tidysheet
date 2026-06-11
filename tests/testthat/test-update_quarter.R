dat <- data.frame(quarter = c("1", "2", "3", "4"), value = 1:4)

test_that("update_quarter does nothing if data are not on a financial year basis", {

  expect_no_message(
    result <- update_quarter(dat, FALSE,  NA, "quarter")
  )

  expect_equal(result, dat)

})


test_that("update_quarter does nothing if q1_is_jan_to_mar is FALSE", {

  expect_no_message(
    result <- update_quarter(dat, TRUE, FALSE, "quarter")
  )

  expect_equal(result, dat)

})


test_that("update_quarter throws an error if quarter column name is not provided", {

  expect_error(
    result <- update_quarter(dat, TRUE, TRUE, NA),
    "quarter_col_name must be supplied"
  )

})


test_that("update_quarter throws an error if quarter column name is not in the data", {

  expect_error(
    result <- update_quarter(dat, TRUE, TRUE, "Q"),
    "No column found for quarter_col_name "
  )

})


test_that("update_quarter changes 1 to 4, 2, to 1, 3 to 2, and 4 to 3", {

  expected <-  data.frame(quarter = c(4, 1, 2, 3), value = 1:4)
  result <- update_quarter(dat, TRUE, TRUE, "quarter")

  expect_equal(result, expected)

})


test_that("update_quarter works if there are NA values in quarter", {

  dat <- add_row(dat, quarter = NA, value = 5)

  expected <-  data.frame(quarter = c(4, 1, 2, 3, NA), value = 1:5)
  result <- update_quarter(dat, TRUE, TRUE, "quarter")

  expect_equal(result, expected)

})


