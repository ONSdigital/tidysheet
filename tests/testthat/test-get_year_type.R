test_that("get_year_type returns NA if more than one type of year is found", {
  expect_equal(get_year_type("2021-22, 2023"), as.character(NA))
})


test_that("get_year_type works with a vector of character strings", {
  expect_equal(get_year_type(c("2021-22", "2023")), c("financial", "calendar"))
  expect_equal(get_year_type(c("2021", "2023")), c("calendar", "calendar"))
  expect_equal(
    get_year_type(c("2021-22", "2023-24")), c("financial", "financial")
    )
})


test_that("get_year_type works with a vector of integers", {
  expect_equal(get_year_type(c(2021, 2023)), c("calendar", "calendar"))
})


test_that("get_year_type works with a dataframe or tibble", {
  dat <- data.frame(year = c("2021", "2021-22", "2021 2021-22"))
  expected <- mutate(dat, type = c("calendar", "financial", NA))
  result <- get_year_type(dat, "year")
  expect_equal(result, expected)
})
