test_that("filename year is used if use_filename_year is TRUE, and warn is
returned as TRUE", {

  dat <- c("Title 2026", "2025/26 data")
  expected <- list("year" = "2024-25", "warn" = TRUE)


  result <- suppressMessages(
      get_year_for_use_in_data(
        dat, "Title 2026", "2024_25", use_filename_year = TRUE
        )
    )

  expect_equal(result, expected)
  })


test_that("filename year is used if use_filename_year is FALSE but it matches
found year, and warn is returned as FALSE", {

  dat <- c("Title 2026", "2024/25 data")
  expected <- list("year" = "2024-25", "warn" = FALSE)


  result <- suppressMessages(
    get_year_for_use_in_data(
      dat, "Title 2026", "2024_25", use_filename_year = FALSE
      )
  )

  expect_equal(result, expected)
})


test_that("Calendar year is returned if the year_from_filename is YYYY", {

  dat <- c("Title", "data")
  expected <- list("year" = "2026", "warn" = FALSE)


  result <- suppressMessages(
    get_year_for_use_in_data(
      dat, "Title 2024", "2026", use_filename_year = FALSE
      )
  )

  expect_equal(result, expected)
})


test_that("found year is used if use_filename_year is FALSE and there is a
mismatch, and warn is returned as TRUE", {

  dat <- c("Title 2026", "2025/26 data")
  expected <- list("year" = "2025-26", "warn" = TRUE)

    result <- suppressMessages(
      get_year_for_use_in_data(
        dat, "Title 2026", "2024_25", use_filename_year = FALSE
        )
    )

  expect_equal(result, expected)
})


test_that("when none match the file name, use file name is FALSE and multiple are found, use the first", {
  dat <- c("Title 2026", "2025/26 data", "Note 2023-24 data available at...")
  expected <- list("year" = "2025-26", "warn" = TRUE)

  result <- suppressMessages(
    get_year_for_use_in_data(
      dat, "Title 2026", "2024_25", use_filename_year = FALSE
      )
  )

  expect_equal(result, expected)
})


test_that("get_year_for_use_in_data is filename year when it matches ANY found
          year and use_filename_year is TRUE", {

  dat <- c("Title 2026", "2025/26 data", "Note 2023-24 data available at...")
  expected <- list("year" = "2023-24", "warn" = FALSE)

  result <- suppressMessages(
    get_year_for_use_in_data(
      dat, "Title 2026", "2023_24", use_filename_year = TRUE
      )
  )

  expect_equal(result, expected)

})


test_that("get_year_for_use_in_data is the first found year that is not the
filename year when use_filename_year is FALSE and warn is returned as TRUE", {

  dat <- c("Title 2026", "2025/26 data", "Note 2023-24 data available at...")
  expected <- list("year" = "2025-26", "warn" = TRUE)


  result <- suppressMessages(
    get_year_for_use_in_data(
      dat, "Title 2026", "2023_24", use_filename_year = FALSE
      )
  )

  expect_equal(result, expected)

})


test_that("get_year_for_use_in_data doesn't give a warningeven if there is a
mismatch if suppress_warning is set to TRUE", {

  dat <- c("Title 2026", "2025/26 data")
  expected <- list("year" = "2025-26", "warn" = FALSE)

  result <- suppressMessages(
    get_year_for_use_in_data(
      dat, "Title 2026", "2024_25", use_filename_year = FALSE,
      suppress_warning = TRUE
      )
  )

  expect_equal(result, expected)

})


test_that("get_year_for_use_in_data uses the title if there is no data above the
first header row", {

  dat <- c()
  expected <- list("year" = "2023-24", "warn" = FALSE)

  result <- suppressMessages(
    get_year_for_use_in_data(
      dat, "2023/24", "2024_25", use_filename_year = FALSE, suppress_warning = TRUE
    )
  )

  expect_equal(result, expected)

})
