  # sheet_year, table_year, filename_year=NA, use_filename_year = FALSE,
  # prefer_sheet_year = FALSE, suppress_warning = FALSE

test_that("filename year is used if use_filename_year is TRUE, and warn is
returned as TRUE", {

  expected <- list("year" = "2024-25", "warn" = TRUE)

  result <- suppressMessages(
      get_year_for_use_in_data(
        "2026-27", NA, "2024_25", use_filename_year = TRUE
        )
    )

  expect_equal(result, expected)
  })


test_that("filename year is used if use_filename_year is FALSE but it matches
a single sheet year, and warn is returned as FALSE", {

  expected <- list("year" = "2024-25", "warn" = FALSE)

  result <- suppressMessages(
    get_year_for_use_in_data(
     "2024-25", NA, "2024_25", use_filename_year = FALSE
      )
  )

  expect_equal(result, expected)
})


test_that("Calendar year is returned if the year_from_filename is YYYY", {

  expected <- list("year" = "2024", "warn" = TRUE)


  result <- suppressMessages(
    get_year_for_use_in_data(
      "2024", NA, "2026", use_filename_year = FALSE
      )
  )

  expect_equal(result, expected)
})


test_that("sheet year is used if use_filename_year is FALSE and there is a
mismatch, and warn is returned as TRUE", {

  expected <- list("year" = "2025-26", "warn" = TRUE)

    result <- suppressMessages(
      get_year_for_use_in_data(
        "2025-26", NA, "2024_25", use_filename_year = FALSE
        )
    )

  expect_equal(result, expected)
})


test_that("when none match the file name, use file name is FALSE and multiple
are found, use the earliest", {

  dat <- c("Title 2026", "2025/26 data", "Note 2023-24 data available at...")
  expected <- list("year" = "2025-26", "warn" = TRUE)

  result <- suppressMessages(
    get_year_for_use_in_data(
      c("2026", "2025-26"), NA, "2024_25", use_filename_year = FALSE
      )
  )

  expect_equal(result, expected)
})


test_that("get_year_for_use_in_data is filename year when it matches ANY
sheet year (above the data) when table year is NA) and use_filename_year is TRUE", {

  expected <- list("year" = "2023-24", "warn" = TRUE)

  result <- suppressMessages(
    get_year_for_use_in_data(
      c("2025-26", "2023-24"), NA, "2023_24", use_filename_year = TRUE
      )
  )

  expect_equal(result, expected)

})


test_that("get_year_for_use_in_data gives the earliest sheet year that is not the
filename year when use_filename_year is FALSE (warn is returned as TRUE)", {

  dat <- c("Title 2026", "2025/26 data", "Note 2023-24 data available at...")
  expected <- list("year" = "2025-26", "warn" = TRUE)

  result <- suppressMessages(
    get_year_for_use_in_data(
      c("2026-27", "2025-26"), NA, "2023_24", use_filename_year = FALSE
      )
  )

  expect_equal(result, expected)

})


test_that("get_year_for_use_in_data doesn't give a warning even if there is a
mismatch if suppress_warning is set to TRUE", {

  expected <- list("year" = "2025-26", "warn" = FALSE)

  result <- suppressMessages(
    get_year_for_use_in_data(
      c("2026", "2025-26"), NA, "2024_25", use_filename_year = FALSE,
      suppress_warning = TRUE
      )
  )

  expect_equal(result, expected)

})

