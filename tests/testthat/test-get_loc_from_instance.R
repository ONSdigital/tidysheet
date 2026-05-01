test_that("get_loc_from_instance selects the correct entry when offset_by is 0", {
  result <- get_loc_from_instance(c(3, 6, 9), 2, 0)
  expect_equal(result, 6)
})


test_that("get_loc_from_instance selects the correct entry when offset_by is negative", {
  result <- get_loc_from_instance(c(3, 6, 9), 2, -1)
  expect_equal(result, 7)
})


test_that("get_loc_from_instance selects the correct entry when offset_by is positive", {
  result <- get_loc_from_instance(c(3, 6, 9), 2, 1)
  expect_equal(result, 5)
})


test_that("get_loc_from_instance gives a warning when instance is out of bounds
and returns the first instance", {

  expect_warning(
    result <- get_loc_from_instance(c(2, 6, 9), 4, 0),
    "The specified instance .* exceeds the number of instances found"
  )

  expect_equal(result, 2)
})


test_that("get_loc_from_instance throws an error when instance is negative.", {

  expect_error(
    get_loc_from_instance(c(2, 6, 9), -1, 0),
    "instance.*must be positive. Check that instance and offset_by are correct"
  )

})

test_that("get_loc_from_instance throws an error when instance is 0", {

  expect_error(
    get_loc_from_instance(c(2, 6, 9), 0, 0),
    "instance.*must be positive. Check that instance and offset_by are correct"
  )

})


test_that("get_loc_from_instance throws an error if more than one instance is given", {

  expect_error(
    get_loc_from_instance(c(2, 6, 9), c(1, 2), 0),
    "instance.*must be a single value"
  )

})


test_that("get_loc_from_instance throws an error if more than one offset is given", {

  expect_error(
    get_loc_from_instance(c(2, 6, 9), 2, c(1, 2)),
    "offset.*must be a single value"
  )

})

