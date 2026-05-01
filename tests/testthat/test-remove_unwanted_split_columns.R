test_that("remove_unwanted_split_columns removes all expected columns based on what they start with", {

  dat <- data.frame(
    col_to_split = "split this",
    value = 1,
    tmp_use_split_point_1 = TRUE,
    tmp_use_split_point_0 = TRUE,
    tmp_split_1 = 1,
    tmp_pattern_at_start = FALSE,
    tmp_matches_next_pattern_1 = FALSE,
    tmp_whitespace_1 = TRUE,
    tmp_string_1 = "split",
    tmp_action_to_take = "split",
    tmp_string_2 = "this"
  )

  expected <- data.frame(
    col_to_split = "split this",
    value = 1,
    tmp_string_1 = "split",
    tmp_string_2 = "this"
  )

  result <- remove_unwanted_split_columns(dat)

  expect_equal(expected, result)
})

test_that("remove_unwanted_split_columns works when some columns don't exist", {

  dat <- data.frame(
    col_to_split = "split this",
    value = 1,
    tmp_use_split_point_1 = TRUE,
    tmp_whitespace_1 = TRUE,
    tmp_string_1 = "split",
    tmp_action_to_take = "split",
    tmp_string_2 = "this"
  )

  expected <- data.frame(
    col_to_split = "split this",
    value = 1,
    tmp_string_1 = "split",
    tmp_string_2 = "this"
  )

  result <- remove_unwanted_split_columns(dat)

  expect_equal(expected, result)
})

test_that("remove_unwanted_split_columns works when no columns with the prefixes for removal exist", {

  dat <- data.frame(
    col_to_split = "split this",
    value = 1,
    tmp_string_1 = "split",
    tmp_string_2 = "this"
  )

  result <- remove_unwanted_split_columns(dat)

  expect_equal(dat, result)
})
