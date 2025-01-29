context("convert_year_format")

test_that("convert_year_format works correctly", {
  expect_equal(convert_year_format("2022/23"), "2022-23")
  expect_equal(convert_year_format("2022.23"), "2022-23")
  expect_equal(convert_year_format("2022_23"), "2022-23")
  expect_equal(convert_year_format("2022-23"), "2022-23") # no change expected
  expect_equal(convert_year_format("2022*23"), "2022-23")
  expect_equal(convert_year_format("2022 23"), "2022-23") 
}) 