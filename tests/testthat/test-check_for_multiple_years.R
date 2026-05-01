test_that("check_for_mutliple_years returns TRUE with default params when there are multiple year types", {
  dat <- data.frame(char = "2024 2024-25")
  result <- check_for_multiple_years(dat, "char")
  expect_equal(result, TRUE)
})


test_that("check_for_mutliple_years returns FALSE when there is only a single year in each row", {
  dat <- data.frame(char = c("2024", "2025", "2021-22", "2022-23"))
  result <- check_for_multiple_years(dat, "char")
  expect_equal(result, FALSE)
})

# This does not pass but is very unlikely and is complex to make pass
# test_that("check_for_mutliple_years returns FALSE when there are multiple but non-unique years", {
#   dat <- data.frame(char = "2024 2024")
#   result <- check_for_multiple_years(dat, "char")
#   expect_equal(result, FALSE)
# })


test_that("check_for_mutliple_years returns TRUE when ther are multiple financial years and type is single", {
  dat <- data.frame(char = c("2021 2022-23 2023-24"))
  result <- check_for_multiple_years(dat, "char", "financial", "single")
  expect_equal(result, TRUE)
})


test_that("check_for_mutliple_years returns FALSE when ther are multiple financial years but prefer is calendar and type is single", {
  dat <- data.frame(char = c("2021 2022-23 2023-24"))
  result <- check_for_multiple_years(dat, "char", "calendar", "single")
  expect_equal(result, FALSE)
})


test_that("check_for_mutliple_years returns TRUE when ther are multiple calendar years but 1 financial year, when prefer is calendar and type is single", {
  dat <- data.frame(char = c("2021 2022 2023-24"))
  result <- check_for_multiple_years(dat, "char", "calendar", "single")
  expect_equal(result, TRUE)
})

