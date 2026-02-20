# Three repeats of a single column that matches the regex, and a duplicated
# column that does not match the regex. Each given for two years.
# Expect two instances of the matched one to be renamed (two values given for
# rename_duplicate_index)
# Expect the one that isn't matched to the regex to stay the same.
dat <- data.frame(
  "item_detail" = rep(c("A", "A", "Total A", "C", "Total A", "Total A", "Total C"), 2),
  "year" = rep(c("2021-22", "2022-23"), each = 7),
  "numeric" = rep(c(2, 3, 5, 0.1, 5.1, 10.1, 0.1), 2)
)
expected <- dat %>%
  mutate(
    item_detail = rep(
      c("A", "A", "first Total A", "C", "Total A", "third Total A", "Total C"),
      2),
    rename_note =
      rep(c(
        rep(c(
          NA, NA,
          "item_detail was 'Total A' in the raw data. It was given a prefix because there was more than one instance in the raw data with that name."
        ), 2),
        NA), 2)
  )

cols_to_group_by <- c("year", "item_detail", rep(NA, 3))

test_that("rename_duplicates adds the correct prefix to entries matching a pattern in a specified column but not any other column with a match to the pattern", {


  result <- suppressMessages(
    rename_duplicates(
      dat, cols_to_group_by,
      "item_detail", "(?i)total A", c(1, 3), c("first", "third"), 3
    )
  )
  expect_equal(result, tibble(result))
})


test_that("rename_duplicates returns the original data if the relevant args are not supplied", {

  result_all_missing <- rename_duplicates(
    dat, cols_to_group_by, NA, NA, NA, NA, NA
    )
  expect_equal(dat, result_all_missing)

})


test_that("rename_duplicates throws an error if some but not all of the relevant args are not supplied", {

  expect_error(
    rename_duplicates(
      dat, cols_to_group_by, NA, "(?i)total A", 1, "first", 2
    ),
    "'column' missing"
  )

  expect_error(
    rename_duplicates(
      dat, cols_to_group_by, "item", NA, 1, "first", 2
    ),
    "'pattern' missing"
  )

  expect_error(
    rename_duplicates(
      dat, cols_to_group_by, "item", "(?i)total A", NA, "first", 2
    ),
    "'index' missing"
  )

  expect_error(
    rename_duplicates(
      dat, cols_to_group_by, "item", "(?i)total A", 1, NA, 2
    ),
    "'prefix' missing"
  )

  expect_error(
    rename_duplicates(
      dat, cols_to_group_by, "item", "(?i)total A", 1, "first", NA
    ),
    "'expected_freq' missing"
  )

  expect_error(
    rename_duplicates(
    dat, cols_to_group_by, NA, NA, 1, "first", 2
    ),
    "'column', and 'pattern' missing"
  )

})


test_that("rename_duplicates throws an error if the number of prefixes does not match the number of indexes", {


  expect_error(
    rename_duplicates(
      dat, cols_to_group_by, "item", "(?i)total A", c(1, 2), "first", 2
      ),
    "No renaming done"
  )

  expect_error(
    rename_duplicates(
      dat, cols_to_group_by, "item", "(?i)total A", 1, c("first", "second"), 2
    ),
    "No renaming done"
  )


})


test_that("rename_duplicates only acts on repeated entries when there are multiple matches to a pttern", {

  # "(?i) total" matches Total A and Total C, but only Total A is replicated.
  result <- suppressMessages(
    rename_duplicates(
      dat, cols_to_group_by,
      "item_detail", "(?i)total", c(1, 3), c("first", "third"), 3
    )
  )
  expect_equal(result, tibble(expected))
})


test_that("rename_duplicates throws an error if there are multiple matches to a pttern with repeated entries", {

  expect_error(suppressMessages(
    rename_duplicates(
      dat, cols_to_group_by, "item_detail", "A", c(1, 2), c("first", "second"), 3
    )
  ))

})


test_that("rename_duplicates throws an error if column is not in the data", {

  expect_error(
    rename_duplicates(
      dat, cols_to_group_by, "Goofy", "(?i)total", c(1, 2), c("first", "second"), 3
    ),
    "'Goofy') does not match"
  )

})

