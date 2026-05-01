#' - Entry in the specified column matches the given pattern.
#' - The entry is in a position given by index e.g.
#'   if index is set to 1, only the entries that relate to the first of the
#'   relevant raw data columns are flagged.
dat <- data.frame(
  "year" = rep(c("2022-23", "2023-24"), each = 5),
  "item" = rep(c("A", "B", "Total", "C", "Total"), 2),
  "numeric" = c(2, 3, 5, 0.1, 5.1,
                1, 2, 3, 0.4, 3.4)
)

test_that("identify_target_rows flags matching patterns in the specified column when in the position given by index", {

  expected <- dat %>%
    mutate(matches_regex = rep(c(FALSE, FALSE, TRUE, FALSE, TRUE), 2),
           location = rep(c(1, 1, 1, 1, 2), 2),
           rename_required = rep(c(FALSE, FALSE, TRUE, FALSE, FALSE), 2))

  result <- identify_target_rows(
    dat,
    cols_to_group_by = c("year", "item"),
    column = "item",
    pattern = "(?i)total",
    index = 1
    )

  expect_equal(result, tibble(expected))

})


test_that("identify_target_rows doesn't flag rows with no match to the pattern", {

  expected <- dat %>%
    mutate(matches_regex = FALSE,
           location = rep(c(1, 1, 1, 1, 2), 2),
           rename_required = FALSE)

  expect_error(
    identify_target_rows(
      dat,
      cols_to_group_by = c("year", "item"),
      column = "item",
      pattern = "no match",
      index = 1
    ),
    "No entries.*match the pattern 'no match'"
  )

})


test_that("identify_target_rows does not flag matching patterns if position given by index does not exist", {

  expected <- dat %>%
    mutate(matches_regex = rep(c(FALSE, FALSE, TRUE, FALSE, TRUE), 2),
           location = rep(c(1, 1, 1, 1, 2), 2),
           rename_required = rep(c(FALSE, FALSE, TRUE, FALSE, FALSE), 2))

  expect_error(
    identify_target_rows(
      dat,
      cols_to_group_by = c("year", "item"),
      column = "item",
      pattern = "(?i)total",
      index = 3
    ),
    "there are only 2 repeats"
  )

})

