test_that("get_year_column_names only returns the name of the column that matches pattern", {
  dat <- data.frame(
    calendar_year = 2025,
    financial_year = c("2012-2013", "2013-14 note", NA),
    mixed_year = c("2012-2013", "2013", "2014")
  )
  result <- suppressMessages(get_year_column_names(dat, "financial"))
  expected <- c(calendar = NA, financial = "financial_year", mixed = NA)
  expect_equal(result, expected)

  })


test_that("get_year_column_names handles cases where multiple columns match pattern", {

  dat <- data.frame(
    financial_year = c("2012-2013", "2013-14 note", NA),
    mixed_year = c("2012-2013", "2013", "2014"),
    calendar_year = 2025,
    year_notes = c("", "something about 2021", ""),
    other = 2021
  )

  expect_warning(
     result <- suppressMessages(get_year_column_names(dat, "year")),
     "More than one column was found with the same type of year: 'calendar_year', 'year_notes'"
  )

  expected <- c(
    calendar = "calendar_year",
    financial = "financial_year",
    mixed = "mixed_year"
    )
  expect_equal(result, expected)

})


test_that("get_year_column_names uses (?i)year if pattern is not supplied", {
  dat <- data.frame(
    financial_year = c("2012-2013", "2013-14 note", NA),
    mixed_year = c("2012-2013", "2013", "2014"),
    calendar_year = 2025,
    other = 2021
  )

  result <- suppressMessages(get_year_column_names(dat, NA))
  expected <- c(
    calendar = "calendar_year",
    financial = "financial_year",
    mixed = "mixed_year"
  )
  expect_equal(result, expected)
})


test_that("get_year_column_names only returns columns containing valid years", {
  dat <- data.frame(
    financial_year = c("2012-2013", "2013-14 note", NA),
    year_notes = c("note 1", "note 2", NA),
    calendar_year = 2025
  )

  result <- suppressMessages(get_year_column_names(dat, "year"))
  expected <- c(
    calendar = "calendar_year",
    financial = "financial_year",
    mixed = NA
  )
  expect_equal(result, expected)
})


test_that("get_year_column_names returns the column with the most valid years if there is more than one of the same type", {
  dat <- data.frame(
    year_notes = c("2021 1", "2022 2", NA),
    calendar_year = 2025
  )

  expect_warning(
      result <- suppressMessages(get_year_column_names(dat, "year")),
      "More than one"
  )
  expected <- c(
    calendar = "calendar_year",
    financial = NA,
    mixed = NA
  )
  expect_equal(result, expected)
})
