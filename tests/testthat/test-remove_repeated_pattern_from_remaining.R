test_that("remove_repeated_pattern_from_remaining prevents splits happening twice in the same place", {

  dat <- data.frame(
    col_to_split = c("split \n this \n here",
                     "split \n here Note this",
                     "Note this Note that"),
    tmp_string_1 = c("split", "split", "Note this"),
    tmp_remaining = c("\n this \n here", "\n here Note this", "Note that"),
    tmp_matches_next_pattern_1 = c(TRUE, FALSE, TRUE),
    tmp_matches_next_pattern_3 = c(FALSE, FALSE, TRUE),
    tmp_whitespace_1 = c(TRUE, TRUE, FALSE),
    tmp_whitespace_3 = c(FALSE, FALSE, FALSE)
  )

  # These tests are based on a situation where split_points are
  # c("newline", "newline", "=Note", "=Note") i.e. split_patterns
  # are c('\n', '\n', 'Note', 'Note')

  # dat and expected_1 are based on the situation where we are on the
  # first iteration of calling the function (i.e we are looking at split_patterns
  # 1 and 2 - '\n', '\n')
  # The changes should only occur in tmp_remaining, which is the column from which
  # the repeated pattern is removed/dealt with. The first two will just get the
  # new line removed, the last one will not change.
  expected_1 <- dat %>%
    mutate(dat, tmp_remaining = c("this \n here", "here Note this", "Note that"))

  result_1 <- remove_repeated_pattern_from_remaining(
    dat, "\n", "tmp_matches_next_pattern_1", "tmp_whitespace_1"
  )
  expect_equal(expected_1, result_1)

  # dat_2 is based on the same dataset as dat, but is for when we have got to the
  # final split (i.e we are in iteration 3 of calling this function, and looking at
  # split_patterns 3 and 4 - 'Note' and 'Note')
  # The first 2 elements of tmp_remaining will have the new line removed from the
  # start, and 'Note' will be removed from the start of the last one.
  dat2 <- dat %>%
    mutate(tmp_remaining = c("\n this \n here", "\n here Note this", "Note that"),
           tmp_string_2 = c("this", "here", "Note this"),
           tmp_string_3 = c("here", "here", "Note this"))

  expected_2 <- dat2 %>%
    mutate(tmp_remaining = c("this \n here", "here Note this", " that"))


  result_2 <- remove_repeated_pattern_from_remaining(dat2, "Note", "tmp_matches_next_pattern_3",
                                                     "tmp_whitespace_3")
  expect_equal(expected_2, result_2)

})

test_that("remove_repeated_pattern_from_remaining fails gracefully when required columns are not present", {

  dat <- data.frame(
    col_to_split = c("split \n this \n here",
                     "split \n here Note this",
                     "Note this Note that"),
    tmp_string_1 = c("split", "split", "Note this"),
    tmp_remaining = c("\n this \n here", "\n here Note this", "Note that"),
    tmp_matches_next_pattern_1 = c(TRUE, FALSE, TRUE),
    tmp_matches_next_pattern_3 = c(FALSE, FALSE, TRUE),
    tmp_whitespace_1 = c(TRUE, TRUE, FALSE),
    tmp_whitespace_3 = c(FALSE, FALSE, FALSE)
  )

  matches_next_pattern_missing <- select(dat, -starts_with("tmp_matches"))
  whitespace_missing <- select(dat, -starts_with("tmp_whitespace"))
  remaining_missing <- select(dat, -tmp_remaining)

  expect_error(
    remove_repeated_pattern_from_remaining(
      matches_next_pattern_missing, "\n", "tmp_matches_next_pattern_1", "tmp_whitespace_1"
    ),
    "'tmp_matches_next_pattern_1' should be a column name, but it was not found"
  )
  expect_error(
    remove_repeated_pattern_from_remaining(
      whitespace_missing, "\n", "tmp_matches_next_pattern_1", "tmp_whitespace_1"
    ),
    "'tmp_whitespace_1' should be a column name, but it was not found"
  )
  expect_error(
    remove_repeated_pattern_from_remaining(
      remaining_missing, "\n", "tmp_matches_next_pattern_1", "tmp_whitespace_1"
    ),
    "'tmp_remaining' should be a column name, but it was not found"
  )

})
