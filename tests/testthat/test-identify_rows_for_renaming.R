dat <- data.frame(
  "year" = rep(c("2022-23", "2023-24"), each = 5),
  "item" = rep(c("A", "B", "Total", "C", "Total"), 2),
  "numeric" = c(2, 3, 5, 0.1, 5.1,
                1, 2, 3, 0.4, 3.4)
)

test_that("identify_rows_for_renaming gives expected results when expected frequency matches actual frequency", {

  result <- identify_rows_for_renaming(
    dat,
    cols_to_group_by = c("year", "item"),
    column = "item",
    pattern = "(?i)total",
    index = 1,
    expected_freq = 2
    )

  expected <- dat %>%
    mutate(location = rep(c(1, 1, 1, 1, 2), 2),
           rename_required = rep(c(FALSE, FALSE, TRUE, FALSE, FALSE), 2))

  expect_equal(result, tibble(expected))

})


test_that("identify_rows_for_renaming gives expected results when expected frequency is lower than actual frequency", {

  expect_warning(
    result <- identify_rows_for_renaming(
      dat,
      cols_to_group_by = c("year", "item"),
      column = "item",
      pattern = "(?i)total",
      index = 1,
      expected_freq = 1
    ),
    "More than the expected number of duplicate names were found for 'Total"
  )

  expected <- dat %>%
    mutate(location = rep(c(1, 1, 1, 1, 2), 2),
           rename_required = FALSE)

  expect_equal(result, tibble(expected))

})


test_that("identify_rows_for_renaming gives expected results when expected frequency is higher than actual frequency", {

  expect_warning(
    result <- identify_rows_for_renaming(
      dat,
      cols_to_group_by = c("year", "item"),
      column = "item",
      pattern = "(?i)total",
      index = 1,
      expected_freq = 4
    ),
    "Fewer than the expected number of duplicate names were found for 'Total"
  )

  expected <- dat %>%
    mutate(location = rep(c(1, 1, 1, 1, 2), 2),
           rename_required = FALSE)

  expect_equal(result, tibble(expected))

})
