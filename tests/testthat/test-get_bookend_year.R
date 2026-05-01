test_that("get_bookend_year works as expected for calendar years", {
  expect_equal(
    suppressMessages(get_bookend_year("2021", -1, "calendar")), "2020"
  )
  expect_equal(
    suppressMessages(get_bookend_year("2021", 1, "calendar")), "2022"
  )
  expect_equal(
    suppressMessages(get_bookend_year("2021", -10, "calendar")), "2011"
  )
  expect_equal(
    suppressMessages(get_bookend_year("2021", 10, "calendar")), "2031"
  )

})


test_that("get_bookend_year works as expected for financial years", {
  expect_equal(
    suppressMessages(get_bookend_year("2021-22", -1, "financial")), "2020-21"
  )
  expect_equal(
    suppressMessages(get_bookend_year("2021-22", 1, "financial")), "2022-23"
  )
  expect_equal(
    suppressMessages(get_bookend_year("2021/22", -10, "financial")), "2011-12"
  )
  expect_equal(
    suppressMessages(get_bookend_year("2021 to 22", 10, "financial")), "2031-32"
  )
})


test_that("get_bookend_year does as expected in a string with more than one calendar year", {
  expect_warning(
    result <- suppressMessages(get_bookend_year("2021 2034", 1, "calendar")),
    "More than one calendar year provided"
  )
  expect_equal(result, NA)
})


test_that("get_bookend_year does as expected in a string with more than one financial year", {
  expect_warning(
    result <- suppressMessages(
      get_bookend_year("2021-22 2023-24", 1, "financial")
    ), "More than one financial year provided"
  )
  expect_equal(result, NA)

})


test_that("get_bookend_year does as expected in a string with no financial year", {
  expect_warning(
    result <- suppressMessages(get_bookend_year("2021", 1, "financial")),
    "No financial year provided"
  )
  expect_equal(result, NA)
})


test_that("get_bookend_year does as expected in a string with no calendar year", {
  expect_warning(
    result <- suppressMessages(get_bookend_year("2021-22", 1, "calendar")),
    "No calendar year provided"
  )
  expect_equal(result, NA)
})
