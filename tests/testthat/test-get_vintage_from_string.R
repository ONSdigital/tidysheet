test_that("get_vintages_from_string translates forecaset to budget", {
  result <-get_vintage_from_string("this is forecast data")
  expect_equal(result, "budget")
})


test_that("get_vintages_from_string translates 'estimated return' to budget", {
  result <-get_vintage_from_string("this is estimated return data")
  expect_equal(result, "budget")
})


test_that("get_vintages_from_string finds budget but not budgeted", {
  result_true <-get_vintage_from_string("this is budget data")
  expect_equal(result_true, "budget")

  result_false <-get_vintage_from_string("this is budgeted data")
  expect_equal(result_false, NA)
})


test_that("get_vintages_from_string finds provisional but not provisionally", {
  result_true <-get_vintage_from_string("this is provisional data")
  expect_equal(result_true, "provisional")

  result_false <-get_vintage_from_string("we provisionally")
  expect_equal(result_false, NA)
})


test_that("get_vintages_from_string finds -final but not finally", {
  result_true <-get_vintage_from_string("this is -final data")
  expect_equal(result_true, "final")

  result_false <-get_vintage_from_string("finally we")
  expect_equal(result_false, NA)
})


test_that("get_vintages_from_string returns vintage if multiple vintages the same are found", {
  result <-get_vintage_from_string("this is final final data")
  expect_equal(result, "final")
})


test_that("get_vintages_from_string returns NA and a warning if different vintages are found", {
  expect_warning(
    result <-get_vintage_from_string("this is final budget data"),
    "More than one"
  )
  expect_equal(result, NA)
})


test_that("get_vintages_from_string returns NA if string is not supplied", {
  result <-get_vintage_from_string(NA)
  expect_equal(result, NA)
})


test_that("get_vintages_from_string returns NA if no vintage is found", {
  result <-get_vintage_from_string("no vintages here")
  expect_equal(result, NA)
})

test_that("get_vintages_from_string returns vintage regardless of case", {
  result <- get_vintage_from_string("BUDGET")
  expect_equal(result, "budget")
})
