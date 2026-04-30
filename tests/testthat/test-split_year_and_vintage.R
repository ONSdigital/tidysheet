test_that("split_year_and_vintage returns unchanged dat if pattern is NA" , {
  dat <- data.frame(
    year_and_vintage = c(
      "2023-24 final", "2024-2025 provisional", "2025_26 budget"
    )
  )

  result <- suppressMessages(split_year_and_vintage(dat, NA))
  expect_equal(result, dat)
})


test_that("split_year_and_vintage raises an error if no columns are found to match pattern" , {
  dat <- data.frame(
    year_and_vintage = c(
      "2023-24 final", "2024-2025 provisional", "2025_26 budget"
    )
  )

  expect_error(
    suppressMessages(split_year_and_vintage(dat, "nope")),
    "(?i)no matching column has been found"
  )
})


test_that("split_year_and_vintage raises an error if multiple columns are found to match pattern" , {
  dat <- data.frame(
    year_and_vintage = c(
      "2023-24 final", "2024-2025 provisional", "2025_26 budget"
    ),
    year = c("2023-24", "2024-2025", "2025_26")
  )

  expect_error(
    suppressMessages(split_year_and_vintage(dat, "year")),
    "(?i)multiple matching column names have been found"
  )
})


test_that("split_year_and_vintage creates new columns when year is financial" , {
  dat <- data.frame(
       year_and_vintage = c(
         "2023-24 final", "2024-2025 provisional", "2025_26 budget"
         )
       )
  expected <- dat %>%
    mutate(year = c("2023-24", "2024-2025", "2025_26"),
           vintage =  c("final", "provisional", "budget"))

  result <- suppressMessages(split_year_and_vintage(dat, "year.*vintage"))
  expect_equal(data.frame(result), data.frame(expected))
})


test_that("split_year_and_vintage creates new columns when year is calendar" , {
  dat <- data.frame(
    year_and_vintage = c("2023 final", "2024 provisional", "2025 forecast")
  )
  expected <- dat %>%
    mutate(year = c("2023", "2024", "2025"),
           vintage =  c("final", "provisional", "forecast"))
  result <- suppressMessages(split_year_and_vintage(dat, "year.*vintage"))
  expect_equal(data.frame(result), data.frame(expected))
})


test_that("split_year_and_vintage does not overwrite year and vintage" , {
  dat <- data.frame(
    year_and_vintage = "2023 final",
    year = "2024",
    vintage = "provisional"
  )

  expected <- dat %>%
    rename(`_year` = year,
           `_vintage` = vintage) %>%
    mutate(year = "2023",
           vintage =  "final")

  result <- suppressMessages(split_year_and_vintage(dat, "year.*vintage"))
  expect_equal(data.frame(result), data.frame(expected))

})


test_that("split_year_and_vintage copes with multiple vintages in one cell" , {
  dat <- data.frame(
    year_and_vintage = "2023 provisional and final"
  )

  expected <- dat %>%
    mutate(year = "2023",
           vintage = "provisional, final")

  result <- suppressMessages(split_year_and_vintage(dat, "year.*vintage"))
  expect_equal(data.frame(result), data.frame(expected))
})

