test_that("choose_from_multiple_years returns the first year and warn = TRUE
if multiple unique years are provided", {

  years <- c("2021-22", "2022-23")
  expected <- list("year" = "2021-22", "warn" = TRUE)
  result <- suppressMessages(choose_from_multiple_years(years))

  expect_equal(result, expected)

})


test_that("choose_from_multiple_years returns a single year and warn = FALSE
if only one unique year is provided", {

  year_repeated <- c("2021-22", "2021-22")
  single_year <- "2021-22"
  year_with_NA <- c("2021-22", NA)

  expected <- list("year" = "2021-22", "warn" = FALSE)

  result_repeated <- suppressMessages(choose_from_multiple_years(year_repeated))
  result_single <- suppressMessages(choose_from_multiple_years(single_year))
  result_with_NA <- suppressMessages(choose_from_multiple_years(year_with_NA))

  expect_equal(result_repeated, expected)
  expect_equal(result_single, expected)
  expect_equal(result_with_NA, expected)


})


test_that("choose_from_multiple_years returns NA if no years are provided", {

  result <- choose_from_multiple_years(NA)
  expect_equal(result, NA)

})
