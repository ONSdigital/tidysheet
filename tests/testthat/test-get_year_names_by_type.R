test_that("get_year_names_by_type only returns the column with the highest frequency of the type", {
  dat <- data.frame(
      alternative_financial_years = c("2022-23", NA),
      financial_years = "2022-23",
      random = c("2021", "2022-23")
      )
  expected <- c(calendar = NA, financial = "financial_years", mixed = "random")
  expect_warning(
    result <- get_year_names_by_type(dat, names(dat)),
    "More than one column was found with the same type of year"
  )
  expect_equal(result, expected)
})


test_that("get_year_names_by_type selects the first if two columns that have the same frequency of a type", {
  dat <- data.frame(
    alternative_financial_years = "2023-24",
    financial_years = "2022-23"
  )
  expected <- c(
    calendar = NA, financial = "alternative_financial_years", mixed = NA
    )
  expect_warning(
    result <- get_year_names_by_type(dat, names(dat)),
    "More than one column was found with the same type of year"
  )
  expect_equal(result, expected)
})

