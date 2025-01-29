# Unit test for example_function function
test_that("example_function prints correctly", {
  expect_output(example_function(), "I love cats!")
  expect_output(example_function(FALSE), "I guess you dont love cats")
})
