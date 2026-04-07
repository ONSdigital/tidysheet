test_that("refine_metadata_year prefers financial year when preferred is NA", {

  result <- suppressMessages(
    refine_metadata_year(c("2022-23", "2022"), NA)
  )
  expect_equal(result, "2022-23")
})


test_that("refine_metadata_year returns calendar year when preferred is NA and
financial does not exist", {

  result <- suppressMessages(refine_metadata_year(c("2023", "2022"), NA))
  expect_equal(result, c("2022", "2023"))

})


test_that("refine_metadata_year returns years that are the same type as preferred", {
  financial <- suppressMessages(
    refine_metadata_year(c("2021-22", "2022-23", "2022"), "financial")
  )
  calendar <- suppressMessages(
    refine_metadata_year(c("2022-23", "2021-22", "2022"), "calendar")
  )

  expect_equal(financial, c("2021-22", "2022-23"))
  expect_equal(calendar, "2022")
})


test_that("refine_metadata_year raises a warning when one year is provided, but
it is of a different type to preferred, and returns NA", {

  expect_warning(
    financial_result <- suppressMessages(
      refine_metadata_year("2022", "financial")
      ),
    "The year from above the data is not of the same type"
  )

  expect_warning(
    calendar_result <- suppressMessages(
      refine_metadata_year("2022", "financial")
    ),
    "The year from above the data is not of the same type"
  )

  expect_equal(financial_result, NA)
  expect_equal(calendar_result, NA)
})


test_that("refine_metadata_year returns financial years in chronological order", {

  result <- suppressMessages(
    refine_metadata_year(c("2022-23", "2021-22"), NA)
  )
  expect_equal(result, c("2021-22", "2022-23"))
})


test_that("refine_metadata_year returns financial years in chronological order", {

  result <- suppressMessages(
    refine_metadata_year(c("2023", "2022"), NA)
  )
  expect_equal(result, c("2022", "2023"))
})

