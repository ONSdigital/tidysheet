library(testthat)

# Test that get_metadata_column_names returns the expected schema

test_that("get_metadata_column_names returns correct columns", {
  expected <- c("sheet", "title", "supplier", "source", "dataset")
  expect_equal(get_metadata_column_names(), expected)
})
