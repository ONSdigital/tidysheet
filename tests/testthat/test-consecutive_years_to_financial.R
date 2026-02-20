test_that("consecutive_years_to_financial correctly converts two consecutive years", {

  result <- suppressMessages(consecutive_years_to_financial(c("2021", "2022")))
  expect_equal(result, "2021-22")
  #' year <- consecutive_years_to_financial(c("2020", "2022"))
  #' year <- consecutive_years_to_financial(c("2020-21", "2021-22"))
})


test_that("consecutive_years_to_financial returns non-consecutive years as they were.", {

  result <- suppressMessages(
    consecutive_years_to_financial(c("2020", "2022"))
  )
  expect_equal(result, c("2020", "2022"))

})


test_that("consecutive_years_to_financial returns input years if non-calendar years are supplied", {

  result <- suppressMessages(
    consecutive_years_to_financial(c("2020-21", "2021-22"))
  )

  expect_equal(result, c("2020-21", "2021-22"))

})


test_that("consecutive_years_to_financial raises an error if input is not of length 2", {

  expect_error(
    consecutive_years_to_financial(c("2020", "2021", "2022")),
    "More than two years found"
  )

  expect_error(
    consecutive_years_to_financial("2020"),
    "Only one year found"
  )

})

