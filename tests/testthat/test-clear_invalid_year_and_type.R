test_that("clear_invalid_year_and_type removes year_type for invalid year entries", {
  dat <- tibble(
    year = c("2021-22", "2021-22 to 2022-23"),
    year_type = c("financial", "financial"),
    is_blank = FALSE
  )

  expected <- dat %>%
    mutate(year_type = ifelse(year == "2021-22 to 2022-23", NA, year_type),
           year_notes = NA)
  result <- suppressMessages(clear_invalid_year_and_type(dat, FALSE))
  expect_equal(result, expected)
})


test_that("clear_invalid_year_and_type removes invalid entries in year and
year_type when year_to_na is true", {

  dat <- tibble(
    year = c("2021-22", "2021-22 to 2022-23"),
    year_type = c("financial", "financial"),
    is_blank = FALSE
  )

  expected <- tibble(
    year = c("2021-22", NA),
    year_type = c("financial", NA),
    is_blank = FALSE,
    year_notes = c(
      NA, "'2021-22 to 2022-23' is multiple years so removed from 'year'."
    )
  )

  result <- suppressMessages(clear_invalid_year_and_type(dat, TRUE))
  expect_equal(result, expected)
})


test_that("clear_invalid_year_and_type removes all invalid year entries", {
  dat <- tibble(
    year = c(NA, "note", "2021-22 to 2022-23"),
    year_type = c(NA, NA, "financial"),
    is_blank = FALSE
  )
  expected <- tibble(
    year = rep(as.character(NA), 3),
    year_type = rep(as.character(NA), 3),
    is_blank = FALSE,
    year_notes = c(NA,
                   "'note' is invalid so removed from 'year'.",
                   "'2021-22 to 2022-23' is multiple years so removed from 'year'.")
  )
  expect_warning(
    result <- suppressMessages(clear_invalid_year_and_type(dat, TRUE)),
    "Year is not valid for 1 rows: 'year' is set to NA for these rows."
  )
  expect_equal(result, expected)
})
