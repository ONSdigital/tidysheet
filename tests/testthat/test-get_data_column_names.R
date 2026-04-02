library(testthat)

# Test that get_data_column_names returns the expected schema

test_that("get_data_column_names returns correct columns", {
  expected <- c(
    "address", "vintage", "year", "fy_start", "quarter", "month", "date_span",
    "geography_name", "geography_level", "geography_code",
    "description_1", "description_2", "description_3", "description_4", "description_5", "description_6", "description_7",
    "value", "units", "notes"
  )
  expect_equal(get_data_column_names(), expected)
})
