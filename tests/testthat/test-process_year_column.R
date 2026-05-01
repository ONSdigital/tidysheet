test_that("process_year_column raises an error if single year is not a list with year and warn elements", {
  dat <- data.frame(
    year_col = c("2021", "2022"),
    year_notes = c(NA, "2022 is estimated"),
    is_blank = FALSE
  )
  single_year <- c(FALSE)

  expect_error(
    process_year_column(dat, "year", NA, single_year, NA, NA),
    "single_year must be a list with 'year' and 'warn' elements"
    )

  single_year_no_year <- list("warn" = FALSE)

  expect_error(
    process_year_column(dat, "year", NA, single_year_no_year, NA, NA),
    "single_year must include a character string element called 'year'"
  )

  single_year_no_warn <- list("year" = "2021")

  expect_error(
    process_year_column(dat, "year", NA, list(single_year_no_warn), NA, NA),
    "single_year must include a character string element called 'year'"
  )
})


test_that("process year_column correctly uses pattern to identify the year
column even if multiple matching columns exist", {

  dat <- data.frame(
    year_col = c("2021", "2022"),
    year_notes = c(NA, "2022 is estimated"),
    is_blank = FALSE
    )
  single_year <- list("year" = c("2021"), warn = FALSE)

  expected <- dat %>%
    rename(year = year_col) %>%
    mutate(year_type = "calendar")

  expect_warning(
    result <- suppressMessages(
      process_year_column(dat, "year", NA, single_year, NA, NA)
      ),
    "More than one column was found with the same type of year"
  )

  expect_equal(result, expected)

})


test_that("process year_column identifies financial year column over other
options when pattern is not supplied", {

  dat <- data.frame(
    year_start = c("2021", "2022"),
    financial_year = c("2021-22", "2022-23"),
    is_blank = FALSE
    )
  single_year <- list("year" = c("2021"), warn = FALSE)

  expected <- dat %>%
    rename(year = financial_year) %>%
    mutate(year_type = "financial",
           year_notes = NA)

  result <- suppressMessages(
    process_year_column(dat, "year", NA, single_year, NA, NA)
    )

  expect_equal(result, expected)
})


test_that("process year_column uses single_year when pattern is not found and single_year is true", {
  dat <- data.frame(value = 1:5, is_blank = FALSE)

  expected <- dat %>%
    mutate(year = "2021-22",
           year_type = "financial",
           year_notes = NA)

  single_year <- list("year" = c("2021-22"), warn = FALSE)

  expect_warning(
    result <- suppressMessages(
      process_year_column(dat, "year", TRUE, single_year, NA, NA)
      ),
    "No year column found to match the pattern in settings for year column name"
    )

  expect_equal(result, expected)
})


test_that("process year_column uses single_year when pattern is not supplied and
single_year is true", {

  dat <- data.frame(value = 1:5, is_blank = FALSE)
  single_year <- list("year" = c("2021-22"), warn = FALSE)

  expected <- dat %>%
    mutate(year = "2021-22",
           year_type = "financial",
           year_notes = NA)

  result <- suppressMessages(
    process_year_column(dat, NA, TRUE, single_year, NA, NA)
  )

  expect_equal(result, expected)
})


test_that("process year_column doesn't use single_year when single_year is false", {
  dat <- data.frame(value = 1:5, is_blank = FALSE)
  single_year <- list("year" = c("2021-22"), warn = FALSE)

  expected <- dat %>%
    mutate(year = "2021-22",
           year_type = "financial",
           year_notes = NA)

  expect_error(
    suppressWarnings(suppressMessages(
      process_year_column(dat, "year", FALSE, single_year, NA, NA)
      )),
    "No valid year columns have been found"
  )
})


test_that("process year_column uses use single_year when
single_year_overrides_all is true, even if pattern matches a year column", {

  dat <- data.frame(
    year_col = c("2021", "2022"),
    is_blank = FALSE
  )
  single_year <- list("year" = c("2021-22"), warn = FALSE)

  expected <- dat %>%
    mutate(year = c("2021-22"),
           year_type = "financial",
           year_notes = NA)

  result <- suppressMessages(
    process_year_column(dat, "year", TRUE, single_year, TRUE, NA)
  )

  expect_equal(result, expected)
})


test_that("process year_column standardises the year if it is financial", {
  dat <- data.frame(
    year_col = c("2021-2022"),
    is_blank = FALSE
  )
  single_year <- list("year" = c(NA), warn = NA)

  expected <- dat %>%
    mutate(year = "2021-22") %>%
    mutate(year_type = "financial",
           year_notes = NA) %>%
    select(c(year, is_blank, year_type, year_notes))

  result <- suppressMessages(
    process_year_column(dat, "year", NA, single_year, NA, NA)
  )

  expect_equal(result, expected)
})


test_that("irregular year information is not lost if original year column is called year", {
  dat <- data.frame(
    year = c("2021-2022", "2021_22 to 2022-23", "20222"),
    is_blank = FALSE
  )
  single_year <- list("year" = c(NA), warn = NA)

  expected <- dat %>%
    mutate(year = c("2021-22", "2021-22 to 2022-23", NA),
           year_type = c("financial", NA, NA),
           year_notes = c(NA, NA, "'20222' is invalid so removed from 'year'."))

  expect_warning(
    result <- suppressMessages(
      process_year_column(dat, "year", NA, single_year, NA, NA)
    ),
    "Year is not valid for 1 entries: 'year' is set to NA for these rows."
  )

  expect_equal(result, tibble(expected))
})


test_that("process year_column correctly outputs the year if it contains both
calendar and financial year types", {

  dat <- data.frame(
    year_col = c("2021-22", "2021"),
    is_blank = FALSE
  )
  single_year <- list("year" = c(NA), warn = NA)

  expected <- dat %>%
    mutate(year = c("2021-22", "2021"),
           year_type = c("financial", "calendar"),
           year_notes = NA) %>%
    select(year, is_blank, year_type, year_notes)

  result <- suppressMessages(
    process_year_column(dat, "year", NA, single_year, NA, NA)
  )

  expect_equal(result, expected)
})


test_that("process year_column correctly outputs the year if it is calendar", {
  dat <- data.frame(
    year_col = c("2021", "2022"),
    is_blank = FALSE
  )
  single_year <- list("year" = c(NA), warn = NA)

  expected <- dat %>%
    mutate(year = c("2021", "2022"),
           year_type = "calendar",
           year_notes = NA) %>%
    select(year, is_blank, year_type, year_notes)

  result <- suppressMessages(
    process_year_column(dat, "year", NA, single_year, NA, NA)
  )

  expect_equal(result, expected)
})


test_that("process year_column overwrites the original year column if it is called 'year'", {
  dat <- data.frame(
    year = c("year: 2021", "2022"),
    is_blank = FALSE
  )
  single_year <- list("year" = c(NA), warn = NA)

  expected <- dat %>%
    mutate(year = c("2021", "2022"),
           year_type = "calendar",
           year_notes = NA)

  result <- suppressMessages(
    process_year_column(dat, "year", NA, single_year, NA, NA)
  )

  expect_equal(result, expected)
})


test_that("process year_column changes multi_year years and year types to NA if
multi_year_entries_as_na is true", {

  dat <- data.frame(
    year_col = c("2021-22 to 2022-23", "2022"),
    is_blank = FALSE
  )
  single_year <- list("year" = c(NA), warn = NA)

  expected <- tibble(dat) %>%
    mutate(year = c(NA, "2022"),
           year_type = c(NA, "calendar"),
           year_notes = c(
             "'2021-22 to 2022-23' is multiple years so removed from 'year'.", NA)
           ) %>%
    select(year, is_blank, year_type, year_notes)

  result <- suppressMessages(
    process_year_column(dat, "year", NA, single_year, NA, TRUE)
  )

  expect_equal(result, expected)
})


test_that("process year_column changes multi_year year_types to NA but not years
 if multi_year_entries_as_na is false or NA", {

  dat <- data.frame(
    year_col = c("2021-22 to 2022-23", "2022"),
    is_blank = FALSE
  )
  single_year <- list("year" = c(NA), warn = NA)

  expected <- tibble(dat) %>%
    mutate(year = c("2021-22 to 2022-23", "2022"),
           year_type = c(NA, "calendar"),
           year_notes = NA) %>%
    select(year, is_blank, year_type, year_notes)

  result_na <- suppressMessages(
    process_year_column(dat, "year", NA, single_year, NA, NA)
  )
  result_false <- suppressMessages(
    process_year_column(dat, "year", NA, single_year, NA, FALSE)
  )

  expect_equal(result_na, expected)
  expect_equal(result_false, expected)
})

