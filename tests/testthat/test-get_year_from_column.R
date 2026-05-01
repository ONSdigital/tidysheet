test_that("get_year_from_column gives expected output with single calendar years", {
  dat <- data.frame(
    datespan = c("2023")
  )
  expected <- mutate(dat, "year" = "2023")

  result <- suppressMessages(get_year_from_column(dat, "(?i)datespan"))

  expect_equal(result, expected)

})


test_that("get_year_from_column gives expected output with multiple years in some rows", {
  dat <- data.frame(
    datespan = c("jan 2021 to mar 2021", "nov 2021 to jan 2022")
  )
  expected <- mutate(dat, "year" = "2021")

  expect_warning(
    suppressMessages(
      result <- get_year_from_column(dat, "(?i)datespan")
      ),
    "Multiple different years were found for some rows."
  )

  expect_equal(result, expected)

})



test_that("get_year_from_column does not pick up numbers that do not follow year patterns", {
  dat <- data.frame(datespan = c("0123", "20201"))
  expected <- mutate(dat, year = as.character(NA))
  result <- suppressMessages(get_year_from_column(dat, "(?i)datespan"))
  expect_equal(result, expected)

})


test_that("get_year_from_column gives expected output with financial years", {
  dat <- data.frame(
    datespan = c("revised 2021-22", "revised 2022-23")
  )
  expected <- mutate(dat, "year" = c("2021-22", "2022-23"))

  result <- suppressMessages(get_year_from_column(dat, "(?i)datespan"))

  expect_equal(result, expected)

})


test_that("get_year_from_column raises an error if pattern is not matched to a column", {
  dat <- data.frame(
    datespan = c("revised 2021-22", "revised 2022-23")
  )

  expect_error(
    suppressMessages(suppressWarnings(get_year_from_column(dat, "nope"))),
    "Column containing year information not identified",
  )

})


test_that("get_year_from_column renames does not overwrite column if it is already called year", {
  dat <- data.frame(
    year = c("revised 2021-22", "revised 2022-23")
  )
  expected <- dat %>%
    rename(`_year` = year) %>%
    mutate(year = c("2021-22", "2022-23"))

  result <- suppressMessages(suppressWarnings(get_year_from_column(dat, "year")))
  expect_equal(result, expected)

})


