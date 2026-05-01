test_that("update_start_of_string works as expected", {

  # 1st test = 1st row:
  # - The first split is the 1st pattern which is not found at the start.
  #   The second pattern is found at the start of the string but should not be
  #   flagged because flags should only be for the patterns used in the first
  #   split.
  # 2nd test = 2nd row:
  # - The first split is using the 2nd pattern, and the 2nd pattern is found at
  #   the start so it should be flagged, and an _ added to the start of the string.
  # 3rd test = 3rd row:
  # - The first split is using the 3rd pattern, and the 3rd pattern is found at
  #   the start so it should be flagged. The pattern starts with an _, so a hyphen
  #   should be added to the start of the string instead of another underscore.
  dat <- data.frame(
    col_to_split = c("B A 2 B 3 C 4", "B 3 C 4", "_C 4"),
    tmp_use_split_point_1 = c(TRUE, FALSE, FALSE),
    tmp_use_split_point_2 = c(TRUE, TRUE, FALSE),
    tmp_use_split_point_3 = c(TRUE, TRUE, TRUE),
    tmp_split_1 = c(1, 2, 3),
    tmp_split_2 = c(2, 2, 3),
    tmp_split_3 = c(3, 3, 3)
  )

  split_patterns <- c("A", "B", "_*C")

  expected <- dat %>%
    mutate(col_to_split = c("B A 2 B 3 C 4", "_B 3 C 4", "-_C 4"),
           tmp_pattern_at_start = c(FALSE, TRUE, TRUE))

  result <- suppressWarnings(
    update_start_of_string(dat, split_patterns, "col_to_split")
  )

  expect_equal(expected, result)

  expect_warning(
    update_start_of_string(dat, split_patterns, "col_to_split"),
    "2 strings"
  )

})

