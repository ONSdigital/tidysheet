cols <- c(
  "financial" = "financial year", "calendar" = NA, "mixed" = NA
)

test_that("add_single_year_to_possibilities returns NA if more than one year is
supplied", {

  expect_error(
    add_single_year_to_possibilities(cols, TRUE, c("2021", "2022"), FALSE),
    "single_year contains more than one year"
  )

})


test_that("add_single_year_to_possibilities adds NA for single when use_single_year is false", {
  expected <- c(
    "financial" = "financial year", "calendar" = NA, "mixed" = NA,
    "single" = NA
  )
  result <- add_single_year_to_possibilities(cols, FALSE, "2021", FALSE)

  expect_equal(result, expected)
})


test_that("add_single_year_to_possibilities appends single year to possibilities if single_overrides_all is false", {

  expected <-   cols <- c(
    "financial" = "financial year",
    "calendar" = NA, "mixed" = NA,
    "single" = "year"
  )
  result <- add_single_year_to_possibilities(cols, TRUE, "2021", FALSE)
  expect_equal(result, expected)
  })


test_that("add_single_year_to_possibilities overwrites possibilities if single_overrides_all is true", {

  expected <- c(
    "single" = "year"
  )
  result <- add_single_year_to_possibilities(cols, TRUE, "2021", TRUE)
  expect_equal(result, expected)
})

