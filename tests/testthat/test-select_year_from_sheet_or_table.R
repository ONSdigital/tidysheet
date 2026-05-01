test_that("select_year_from_sheet_or_table returns one of sheet_year and table_year
if they match", {

  years <- c("2024-25", "2025-26")
  result <- select_year_from_sheet_or_table(years, years)
  expect_equal(result, years)

})


test_that("select_year_from_sheet_or_table returns the other if one is NA", {

  years <- c("2024-25", "2025-26")

  table_NA_default <- select_year_from_sheet_or_table(years, NA)
  sheet_NA_default <- select_year_from_sheet_or_table(NA, years)
  table_NA_prefer_sheet <- select_year_from_sheet_or_table(years, NA, TRUE)
  sheet_NA_prefer_sheet <- select_year_from_sheet_or_table(NA, years, TRUE)

  expect_equal(table_NA_default, years)
  expect_equal(sheet_NA_default, years)
  expect_equal(table_NA_prefer_sheet, years)
  expect_equal(sheet_NA_prefer_sheet, years)

})


test_that("select_year_from_sheet_or_table returns table year by default", {

  expected <- "2024-25"
  result <- suppressMessages(
    select_year_from_sheet_or_table("2023-24", "2024-25")
  )
  specified <- suppressMessages(
    select_year_from_sheet_or_table("2023-24", "2024-25", FALSE)
  )
  expect_equal(result, expected)
  expect_equal(specified, expected)

})


test_that("select_year_from_sheet_or_table returns sheet year when prefer_sheet_year is TRUE", {

  expected <- "2023-24"
  result <- suppressMessages(
    select_year_from_sheet_or_table("2023-24", "2024-25", TRUE)
  )
  expect_equal(result, expected)

})
