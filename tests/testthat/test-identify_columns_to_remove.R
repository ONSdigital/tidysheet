dat <- data.frame(
  "row" = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  "col" = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  "address" = c("A1", "B1", "C1",
                "A2", "B2", "C2",
                "A3", "B3", "C3"),
  "numeric" = c(NA, NA, NA,
                NA, 1, NA,
                NA, 2, NA),
  "character" = c("name", "id", "ID",
                  "1st row", NA, "a",
                  "2nd row", NA, "b"),
  "data_type" = c(
    rep("character", 4), "numeric", rep("character", 2), "numeric", "character"
  )
)

test_that("identify_columns_to_remove gives expected results with one to one matches", {

  result_one_col <- identify_columns_to_remove(dat, "id")
  result_two_cols <- identify_columns_to_remove(dat, c("name", "id"))

  expect_equal(result_one_col, 2)
  expect_equal(result_two_cols, c(1, 2))

})


test_that("identify_columns_to_remove returns expected result and warnings when more than one col is identified", {

  expect_warning(
    result <- identify_columns_to_remove(dat, "(?i)id"),
    "More than one.*(?i)id"
  )
  expect_equal(result, NULL)

  expect_warning(
    result_mixed <- identify_columns_to_remove(dat, c("(?i)id", "name")),
    "More than one.*(?i)id"
  )
  expect_equal(result_mixed, 1)

})


test_that("identify_columns_to_remove returns expected result and warnings when no columns are identified", {

  expect_warning(
    result <- identify_columns_to_remove(dat, "no match"),
    "No columns matched the pattern 'no match'"
  )
  expect_equal(result, NULL)

  expect_warning(
    result_mixed <- identify_columns_to_remove(dat, c("no match", "id")),
    "No columns matched the pattern 'no match'"
  )
  expect_equal(result_mixed, 2)

})
