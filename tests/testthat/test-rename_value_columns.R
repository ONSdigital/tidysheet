test_that("rename_value_columns results in value and non_numeric_value columns", {
  dat <- tibble(
    item = c("ball", "ball"),
    numeric = c(1, NA),
    character = c(NA, "Small sample size")
  )
  expected <- dat %>%
    rename(value = numeric,
           non_numeric_value = character)
  result <- suppressMessages(rename_value_columns(dat))
  expect_equal(result, expected)
})
