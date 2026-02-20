test_that("flag_consecutive_matching_splits identifies consecutive matching split patterns", {

  split_patterns <- c("(\n)\\s*1\\.", "(\n)\\s*2\\.", "(\n)\\s*3\\.")
  split_point_descriptions <- c(
    "newline_whitespace_=1", "newline_whitespace_=2", "newline_whitespace_=3"
  )

  dat <- data.frame(
    col_to_split = c("split\n 1.this \n 2.one \n 3.thousand", "split\n 2.one"),
    tmp_split_1 = c(1, 2),
    tmp_split_2 = c(2, 2),
    tmp_split_3 = c(3, NA),
    value = c(100, 600)
  )

  expected <- dat %>%
    mutate(tmp_matches_next_pattern_1 = c(FALSE, TRUE),
           tmp_whitespace_1 = TRUE,
           tmp_matches_next_pattern_2 = FALSE,
           tmp_whitespace_2 = TRUE,
           tmp_matches_next_pattern_3 = FALSE,
           tmp_whitespace_3 = FALSE)

  result <- flag_consecutive_matching_splits(dat, split_patterns, split_point_descriptions)

  expect_equal(expected, result)

})
