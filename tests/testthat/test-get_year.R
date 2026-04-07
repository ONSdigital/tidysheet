test_that("get_year gives expected output with single calendar years", {
  dat <- data.frame(
    datespan = c("2023")
  )
  expected <- mutate(dat, "year" = "2023")

  result <- get_year(dat, "datespan")

  expect_equal(result, expected)

})


test_that("get_year gives expected output with multiple years in some rows", {
  dat <- data.frame(
    datespan = c("jan 2021 to mar 2021", "nov 2021 to jan 2022")
  )
  expected <- mutate(dat, "year" = "2021")

  expect_warning(
    suppressMessages(
      result <- get_year(dat, "datespan")
      ),
    "Multiple different years were found for some rows."
  )

  expect_equal(result, expected)

})


test_that("get_year does not pick up numbers that do not follow year patterns", {
  dat <- data.frame(datespan = c("0123", "20201"))
  expected <- mutate(dat, year = as.character(NA))
  result <- get_year(dat, "datespan")
  expect_equal(result, expected)

})


test_that("get_year_from_column gives expected output with financial years", {
  dat <- data.frame(
    datespan = c("revised 2021-22", "revised 2022-23")
  )
  expected <- mutate(dat, "year" = c("2021-22", "2022-23"))

  result <- get_year(dat, "datespan")

  expect_equal(result, expected)

})


test_that("get_year_from_column gives financial year only when type is single and prefer is financial", {
  dat <- data.frame(
    datespan = c("2021-22 2023", "2022-23", "2025")
  )
  expected <- mutate(dat, "year" = c("2021-22", "2022-23", NA))

  result <- get_year(dat, "datespan", prefer = "financial", type = "single")

  expect_equal(result, expected)

})


test_that("get_year_from_column gives calendar year only when type is single and prefer is calendar", {
  dat <- data.frame(
    datespan = c("2021-22 2023", "2022-23", "2025")
  )
  expected <- mutate(dat, "year" = c("2023", NA, "2025"))

  result <- get_year(dat, "datespan", prefer = "calendar", type = "single")

  expect_equal(result, expected)

})


test_that("get_year_from_column prefers financial year but gives calendar if financial not available when type is both and prefer is financial", {
  dat <- data.frame(
    datespan = c("2021-22 2023", "2022-23", "2025")
  )
  expected <- mutate(dat, "year" = c("2021-22", "2022-23", "2025"))

  expect_warning(
    result <- get_year(dat, "datespan", prefer = "financial", type = "both"),
    "Multiple different years were found"
  )

  expect_equal(result, expected)

})


test_that("get_year_from_column prefers calendar year but gives financial if calendar not available when type is both and prefer is calendar", {
  dat <- data.frame(
    datespan = c("2021-22 2023", "2022-23", "2025")
  )
  expected <- mutate(dat, "year" = c("2023", "2022-23", "2025"))

  expect_warning(
    result <- get_year(dat, "datespan", prefer = "calendar", type = "both"),
    "Multiple different years were found"
  )

  expect_equal(result, expected)

})


test_that("get_year does not return any years if none of the preferred type
are available and type is single.", {

  result <- get_year("2026", NA, prefer = "financial", type = "single")
  expect_equal(result, as.character())

})
