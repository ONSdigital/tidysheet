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

  result_one_col <- identify_columns_to_remove(dat, "id", NA, 1)
  result_two_cols <- identify_columns_to_remove(dat, c("name", "id"), NA, 1)

  expect_equal(result_one_col, 2)
  expect_equal(result_two_cols, c(1, 2))

})


test_that("identify_columns_to_remove returns expected result and warnings when more than one col is identified", {

  expect_error(
   identify_columns_to_remove(dat, "(?i)id", NA, 1),
    "More than one.*(?i)id"
  )

  expect_error(
   identify_columns_to_remove(dat, c("(?i)id", "name"), NA, 1),
    "More than one.*(?i)id"
  )

})


test_that("identify_columns_to_remove returns expected result and warnings when no columns are identified", {

  expect_warning(
    result <- identify_columns_to_remove(dat, "no match", NA, 1),
    "No columns matched the pattern 'no match'"
  )
  expect_equal(result, NULL)

  expect_warning(
    result_mixed <- identify_columns_to_remove(dat, c("no match", "id"), NA, 1),
    "No columns matched the pattern 'no match'"
  )
  expect_equal(result_mixed, 2)

})


test_that("identify_columns uses offset correctly", {

  # remove 'name' by finding the column 1 to the left of the 'id' column
  # e.g. could be used if column A is unnamed
  name_col_unnamed <- dat %>%
    mutate(data_type = ifelse(address == "A1", "blank", data_type),
           character = ifelse(address == "A1", NA, character))

  id_col_unnamed <- dat %>%
    mutate(data_type = ifelse(address == "B1", "blank", data_type),
           character = ifelse(address == "B1", NA, character))

  result_moving_left <- identify_columns_to_remove(
    name_col_unnamed, "id", -1, 1
    )
  expect_equal(result_moving_left, 1)

  result_moving_right <- identify_columns_to_remove(
    id_col_unnamed, "name", 1, 1
    )
  expect_equal(result_moving_right, 2)

})
