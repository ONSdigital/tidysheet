test_that("split_to_multiple_columns works in standard cases", {

  # test 1. All standard cases (no repeated patterns and no pattern found at the
  # start of the string)

  split_point_descriptions <- c(
    "ALT_newline_whitespace_=1", "ALT_newline_whitespace_=2",
    "ALT_newline_whitespace_=3"
    )

  dat <- data.frame(
    "col_to_split" = c("A:\n 1.B \n 2.C \n 3.D",
                       "A-\n 2.C \n 3.D",
                       "A\n 1.B \n 3.D",
                       "A\n 1.B \n 2.C",
                       "A\n 3.D",
                       "A\n 2.C",
                       "1. B \n 3. D",
                       "other",
                       "wrong\n 2. order \n 1. matches"
    ),
    "value" = c(1:9)
  )

  expected <- dat %>%
    dplyr::mutate(
      description_1 = c(rep("A", 6), "1. B", "other", "wrong\n 2. order"),
      description_2 = c("1.B", "A", "1.B", "1.B", "A", "A", "1. B", "other", "1. matches"),
      description_3 = c("2.C", "2.C", "1.B", "2.C", "A", "2.C", "1. B", "other", "1. matches"),
      units = c("3.D", "3.D", "3.D", "2.C", "3.D", "2.C", "3. D", "other", "1. matches")
    )

  result <- split_to_multiple_columns(
    dat,
    from = "col_to_split",
    to = c("description_1", "description_2", "description_3", "units"),
    split_point_descriptions = split_point_descriptions
    )


  expect_equal(expected, result)

})


test_that("split_to_multiple_columns works when there are patterns at the start of the string", {

  dat_start_of_string <- data.frame(col_to_split = "pattern at 1. start")
  expected_start_of_string <- data.frame(
    col_to_split = "_pattern at 1. start",
    A = "",
    B = "pattern at",
    C = "1. start"
  )
  result_start_of_string <- suppressWarnings(
    split_to_multiple_columns(
      dat_start_of_string, "col_to_split", LETTERS[1:3], c("ALT_=pat", "ALT_=1")
    )
  )
  expect_equal(expected_start_of_string, result_start_of_string)
})


test_that("split_to_multiple_columns works when there are repeated consecutive patterns", {

  split_point_descriptions <- c(
    "ALT_newline", "ALT_newline", "ALT_=Note", "ALT_=Note"
    )

  dat_repeated_pattern <- data.frame(
    col_to_split = c("split \n this \n here",
                     "split \n here Note this",
                     "to Note this Note that")
  )

  expected_repeated_pattern <- dat_repeated_pattern %>%
    mutate(A = c("split", "split", "to"),
           B = c("this", "here", "to"),
           C = c("here", "here", "to"),
           D = c("here", "Note this", "this"),
           E = c("here", "Note this", "Note that")
    )

  result_repeated_pattern <- suppressWarnings(
    split_to_multiple_columns(
      dat_repeated_pattern, "col_to_split", c("A", "B", "C", "D", "E"),
      split_point_descriptions
    )
  )

  expect_equal(expected_repeated_pattern, result_repeated_pattern)

})


test_that("split_to_multiple_columns raises an error if from is specified but not to", {

  split_point_descriptions <- c(
    "ALT_newline_whitespace_=1", "ALT_newline_whitespace_=2",
    "ALT_newline_whitespace_=3"
  )

  dat <- data.frame(
    "col_to_split" = c("A:\n 1.B \n 2.C \n 3.D"
    ),
    "value" = 1
  )

  expect_error(
    split_to_multiple_columns(
      dat, "col_to_split", to = NA, split_point_descriptions
    ),
    "Either all or none of these must be specified"
  )

})


test_that("split_to_multiple_columns raises an error if to is specified but not from", {

  split_point_descriptions <- c(
    "ALT_newline_whitespace_=1", "ALT_newline_whitespace_=2",
    "ALT_newline_whitespace_=3"
  )

  dat <- data.frame(
    "col_to_split" = c("A:\n 1.B \n 2.C \n 3.D"
    ),
    "value" = 1
  )

  expect_error(
    split_to_multiple_columns(
      dat, NA, to = c("A", "B", "C"), split_point_descriptions
    ),
    "Either all or none of these must be specified"
  )

})


test_that("split_to_multiple_columns raises an error if to is specified but not split_point_descriptions", {

  dat <- data.frame(
    "col_to_split" = c("A:\n 1.B \n 2.C \n 3.D"
    ),
    "value" = 1
  )

  expect_error(
    split_to_multiple_columns(
      dat, "col_to_split", to = c("A", "B", "C"), NA
    ),
    "Either all or none of these must be specified"
  )

})


test_that("split_to_multiple_columns raises an error if more than one column is specified to be split", {

  split_point_descriptions <- c(
    "ALT_newline_whitespace_=1", "ALT_newline_whitespace_=2",
    "ALT_newline_whitespace_=3"
  )

  dat <- data.frame(
    "col_to_split" = c("A:\n 1.B \n 2.C \n 3.D"),
    "other" = c("A:\n 1.B \n 2.C \n 3.D"),
    "value" = 1
  )

  expect_error(
    split_to_multiple_columns(
      dat, c("col_to_split", "other"), to = c("A", "B", "C"), split_point_descriptions
    ),
    "Only one item can be split into multiple columns"
  )

})

test_that("split_to_multiple_columns raises an error if too few 'to' columns are supplied", {

  split_point_descriptions <- c(
    "ALT_newline_whitespace_=1", "ALT_newline_whitespace_=2",
    "ALT_newline_whitespace_=3"
  )

  dat <- data.frame(
    "col_to_split" = c("A:\n 1.B \n 2.C \n 3.D"),
    "value" = 1
  )

  expect_error(
    split_to_multiple_columns(
      dat, "col_to_split", to = c("A", "B", "C"), split_point_descriptions
    ),
    "must contain one fewer elements"
  )

})


test_that("split_to_multiple_columns raises an error if too many 'to' columns are supplied", {

  split_point_descriptions <- c(
    "ALT_newline_whitespace_=1", "ALT_newline_whitespace_=2",
    "ALT_newline_whitespace_=3"
  )

  dat <- data.frame(
    "col_to_split" = c("A:\n 1.B \n 2.C \n 3.D"),
    "value" = 1
  )

  expect_error(
    split_to_multiple_columns(
      dat, "col_to_split", to = c("A", "B", "C", "D", "E"), split_point_descriptions
    ),
    "must contain one fewer elements"
  )

})


test_that("split_to_multiple_columns raises an error if any 'to' columns match the 'from' column", {

  split_point_descriptions <- c(
    "ALT_newline_whitespace_=1", "ALT_newline_whitespace_=2",
    "ALT_newline_whitespace_=3"
  )

  dat <- data.frame(
    "col_to_split" = c("A:\n 1.B \n 2.C \n 3.D"),
    "value" = 1
  )

  expect_error(
    split_to_multiple_columns(
      dat, "col_to_split", to = c("col_to_split", "B", "C", "D"),
      split_point_descriptions
    ),
    "One of the header_split_to strings matches the header_to_split string"
  )

})


test_that("split_to_multiple_columns raises an error if the 'from' column is blank", {

  split_point_descriptions <- c(
    "ALT_newline_whitespace_=1", "ALT_newline_whitespace_=2",
    "ALT_newline_whitespace_=3"
  )

  dat <- data.frame(
    "col_to_split" = rep(NA, 3),
    "value" = 1
  )

  expect_error(
    split_to_multiple_columns(
      dat, "col_to_split", to = c("A", "B", "C", "D"),
      split_point_descriptions
    ),
    "no information  has been found in the data for splitting"
  )

})


test_that("split_to_multiple_columns gives a warning if no split patterns are found", {

  split_point_descriptions <- c(
    "ALT_newline_whitespace_=1", "ALT_newline_whitespace_=2",
    "ALT_newline_whitespace_=3"
  )

  dat <- data.frame(
    "col_to_split" = rep("nowhere to split", 3),
    "value" = 1
  )

  expected <- rename(dat, A = col_to_split)

  expect_warning(
    result <- split_to_multiple_columns(
      dat, "col_to_split", to = c("A", "B", "C", "D"),
      split_point_descriptions
    ),
    "No split pattern has been found in the column to split"
  )

  expect_equal(result, expected)

})

