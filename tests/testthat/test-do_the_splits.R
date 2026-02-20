test_that("do_the_splits splits strings in the order provided ", {

  # Split strings in the 'from' column and put the resulting strings in new
  # columns with the prefix 'string_'. No 'to' column will be empty even if not
  # all 'from' strings contain matches to all the split_patterns patterns.

  split_patterns <- c( "(\n)\\s*1\\.", "(\n)\\s*2\\.", "(\n)\\s*3\\.")
  split_point_descriptions <- c(
    "newline_whitespace_=1", "newline_whitespace_=2", "newline_whitespace_=3"
  )

  dat <- data.frame(
    col_to_split = c("split\n 1.this \n 2.one \n 3.thousand",
                     "split\n 2.one"),
    tmp_use_split_point_0 = c(TRUE, TRUE),
    tmp_use_split_point_1 = c(TRUE, FALSE),
    tmp_use_split_point_2 = c(TRUE, TRUE),
    tmp_use_split_point_3 = c(TRUE, FALSE),
    tmp_split_1 = c(1, 2),
    tmp_split_2 = c(2, 2),
    tmp_split_3 = c(3, NA),
    tmp_matches_next_pattern_1 = c(FALSE, TRUE),
    tmp_matches_next_pattern_2 = c(FALSE, FALSE),
    tmp_matches_next_pattern_3 = c(FALSE, FALSE),
    tmp_whitespace_1 = c(TRUE, TRUE),
    tmp_whitespace_2 = c(TRUE, TRUE),
    tmp_whitespace_3 = c(TRUE, TRUE),
    value = c(100, 600)
  )

  expected <- dat %>%
    mutate(tmp_split_4 = NA,
           tmp_action_to_take = c("split", "remaining"),
           tmp_string_1 = "split",
           tmp_string_2 = c("1.this ", "split"),
           tmp_string_3 = c("2.one ", "2.one"),
           tmp_string_4 = c("3.thousand", "2.one"))

  result <- do_the_splits(dat, split_patterns, 'col_to_split')

  expect_equal(expected, result)

})
