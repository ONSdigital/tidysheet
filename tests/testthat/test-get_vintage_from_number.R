test_that("get_vintage_from_number returns 'final' if number is greater than 1, otherwise return provisional", {
  expect_equal(get_vintage_from_number(1), "provisional")
  expect_equal(get_vintage_from_number(2), "final")
  expect_equal(get_vintage_from_number(100), "final")
})


test_that("get_vintage_from_number returns NA if number is NA", {
  expect_equal(get_vintage_from_number(NA), NA)
})


test_that("get_vintage_from_number raises error more than one number is given", {
  expect_error(get_vintage_from_number(c(1, 2)), "Number must be of length 1.")
})


test_that("get_vintage_from_number raises error if number is not an integer", {
  expect_error(get_vintage_from_number("1"), "Number must be numeric.")
  expect_error(get_vintage_from_number(
    data.frame(number = 1)), "Number must be numeric."
    )
  expect_error(get_vintage_from_number(1.56), "Number must not have decimal places.")
})
