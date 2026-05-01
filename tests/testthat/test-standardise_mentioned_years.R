test_that("standardise_mentioned_years gives expected wording for financial years", {
  dat <- data.frame(
      desc_1 = c("Total 2022-23", "Total 2023-24", "Total 2024-25", "Total"),
      desc_2 = c("2022-23", "2023-24", "2024-25", "Total"),
      original_1 = c("Total 2022-23", "Total 2023-24", "Total 2024-25", "Total")
      )
  expected <- data.frame(
    desc_1 = c("Total previous financial year",
               "Total current financial year",
               "Total next financial year", "Total"),
    desc_2 = c("previous financial year",
               "current financial year",
               "next financial year", "Total"),
    original_1 = c("Total 2022-23", "Total 2023-24", "Total 2024-25", "Total")
  )
  result <- suppressMessages(
    standardise_mentioned_years(dat, c("desc_1", "desc_2"), "2023-24")
  )
  expect_equal(result, expected)
})


test_that("standardise_mentioned_years gives expected wording for calendar years", {
  dat <- data.frame(
    desc_1 = c("Total 2022", "Total 2023", "Total 2024", "Total"),
    desc_2 = c("2022", "2023", "2024", "Total"),
    original_1 = c("Total 2022", "Total 2023", "Total 2024", "Total")
  )
  expected <- data.frame(
    desc_1 = c("Total previous year",
               "Total current year",
               "Total next year", "Total"),
    desc_2 = c("previous year",
               "current year",
               "next year", "Total"),
    original_1 = c("Total 2022", "Total 2023", "Total 2024", "Total")
  )
  result <- suppressMessages(
    standardise_mentioned_years(dat, c("desc_1", "desc_2"), "2023")
  )
  expect_equal(result, expected)
})

test_that("standardise_mentioned_years only considers YYYY if given year is YYYY", {
  dat <- data.frame(
    desc_1 = c("Total 2022", "2022 plus 2024",
               "2022-23 and 2023", "Total 2023-24")
  )
  expected <- data.frame(
    desc_1 = c("Total previous year", "previous year plus next year",
               "2022-23 and current year", "Total 2023-24")
  )
  result <- suppressMessages(
    standardise_mentioned_years(dat, "desc_1", "2023")
  )
  expect_equal(result, expected)
})


test_that("standardise_mentioned_years only considers FY if given year is FY and in the same format", {
  dat <- data.frame(
    desc_1 = c("2023", "2022 and 2023-24")
  )
  expected <- data.frame(
    desc_1 = c("2023", "2022 and current financial year")
  )
  result <- suppressMessages(
    standardise_mentioned_years(dat, "desc_1", "2023-24")
  )
  expect_equal(result, expected)
})


test_that("standardise_mentioned_years throws an error if multiple years are provided", {
  dat <- data.frame(
    desc_1 = c("2023", "2022 and 2023-24")
  )
  expect_error(suppressMessages(
    standardise_mentioned_years(dat, "desc_1", c("2023", "2022"))
    ),
    "More than one year has been provided."

  )
})


test_that("standardise_mentioned_years does nothing if columns is not provided or NA", {
  dat <- data.frame(
    desc_1 = c("2023", "2022 and 2023-24")
  )
  result_NA <- standardise_mentioned_years(dat, NA, "2022")
  result <- standardise_mentioned_years(dat = dat, year = "2022")

  expect_equal(result_NA, dat)
  expect_equal(result, dat)
})


test_that("standardise_mentioned_years raises an error if not all columns are in the data", {
  dat <- data.frame(
    desc_1 = c("Total 2022-23", "Total 2023-24", "Total 2024-25", "Total"),
    desc_2 = c("2022-23", "2023-24", "2024-25", "Total"),
    original_1 = c("Total 2022-23", "Total 2023-24", "Total 2024-25", "Total")
  )

  expect_error(
    suppressMessages(
      standardise_mentioned_years(dat, c("desc_1", "desc_3"), "2023-24")
    ),
    "Some columns are not in the data."
  )
})


test_that("standardise_mentioned_years raises an error if year is not valid", {
  dat <- data.frame(
    desc_1 = c("Total 2022-23", "Total 2023-24", "Total 2024-25", "Total"),
    original_1 = c("Total 2022-23", "Total 2023-24", "Total 2024-25", "Total")
  )

  expect_error(
    suppressMessages(
      standardise_mentioned_years(dat, "desc_1", "2")
    ),
    "year is not a valid calendar or financial year"
  )
})


test_that("standardise_mentioned_years raises an error if year is not valid", {
  dat <- data.frame(
    desc_1 = c("Total 2022-23", "Total 2023-24", "Total 2024-25", "Total"),
    original_1 = c("Total 2022-23", "Total 2023-24", "Total 2024-25", "Total")
  )

  expect_error(
    suppressMessages(
      standardise_mentioned_years(dat, "desc_1", c("2021", "2021-22"))
    ),
    "More than one year has been provide"
  )
})

