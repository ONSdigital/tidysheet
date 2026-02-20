test_that("standardise_year correctly standardises the financial year", {
  dat <- data.frame(fy = c("2021_22", "2020", "now 2022-2023", "other"))
  expected <- mutate(dat, year = c("2021-22", "2020", "now 2022-23", "other"))
  result <- suppressMessages(standardise_year(dat, "fy"))
  expect_equal(result, expected)
})
