test_that("add_best_year_column selects financial year before calendar", {

  dat <- data.frame(
    financial_year = c("2021-22 to 2022-23", "2022_2023"),
    calendar_year = c(2021, 2022),
    mixed_year = c("2021", "2022-23")
  )
  possible_cols <- c(
    calendar = "calendar_year",
    financial = "financial_year",
    mixed = "mixed_year",
    single = "year"
  )

  result <- add_best_year_column(dat, possible_cols, "2025")
  expected <- rename(dat, year = financial_year)
  expect_equal(result, expected)
})


test_that("add_best_year_column selects calendar before mixed", {
  dat <- data.frame(
    calendar_year = c(2021, 2022),
    mixed_year = c("2021", "2022-23")
  )
  possible_cols <- c(
    calendar = "calendar_year",
    financial = NA,
    mixed = "mixed_year",
    single = "year"
  )

  expected <- rename(dat, year = calendar_year)
  result <- add_best_year_column(dat, possible_cols, "2025")
  expect_equal(result, expected)
})


test_that("add_best_year_column selects mixed before single", {
  dat <- data.frame(
    mixed_year = c("2021", "2022-23")
  )
  possible_cols <- c(
    calendar = NA,
    financial = NA,
    mixed = "mixed_year",
    single = "year"
  )

  result <- add_best_year_column(dat, possible_cols, "2025")
  expected <- rename(dat, year = mixed_year)

  expect_equal(result, expected)
})


test_that("add_best_year_column selects single if single_overrides_all is true", {
  dat <- data.frame(
    financial_year = c("2021-22 to 2022-23", "2022_2023"),
    calendar_year = c(2021, 2022)
  )
  possible_cols <- c(
    single = "year"
  )
  single_year <- list("year" = "2021-22", "warn" = FALSE)
  result <- suppressMessages(
    add_best_year_column(dat, possible_cols, single_year)
  )
  expected <- mutate(dat, year = "2021-22")
  expect_equal(result, expected)

})


test_that("add_best_year_column gives a warning if single year is used and warn is TRUE", {
  dat <- data.frame(
    financial_year = c("2021-22 to 2022-23", "2022_2023"),
    calendar_year = c(2021, 2022)
  )
  possible_cols <- c(
    single = "year"
  )
  single_year <- list("year" = "2021-22", "warn" = TRUE)
  expect_warning(
    suppressMessages(
      result <- add_best_year_column(dat, possible_cols, single_year)
    ),
    "Year has been taken from either .* but they do not match each other."
  )
  expected <- mutate(dat, year = "2021-22")
  expect_equal(result, expected)

})

