test_that(
  "get_split_loc identifies the first row number after the desired horizontal split", {

    dat <- data.frame(
      "row" = c(1, 1, 1, 2, 2, 2),
      "col" = c(1, 2, 3, 1, 2, 3),
      "address" = c("A1", "B1", "C1",
                    "A2", "B2", "C2"),
      "character"= c("table 1", "value", NA,
                     "table 2", "value", NA),
      "numeric" = c(NA, NA, 1, NA, NA, 2)
    )

    result <- suppressMessages(get_split_loc(dat, '(?i)table 2', 1, 0, 'row'))
    expect_equal(result, 2)

  })


test_that(
  "get_split_loc identifies the first row number after the desired horizontal split
  and the row to split at is the second instance of the pattern", {

    dat <- data.frame(
      "row" = c(1, 1, 1, 2, 2, 2),
      "col" = c(1, 2, 3, 1, 2, 3),
      "address" = c("A1", "B1", "C1",
                    "A2", "B2", "C2"),
      "character"= c("table 1", "value", NA,
                     "table 2", "value", NA),
      "numeric" = c(NA, NA, 1, NA, NA, 2)
    )

    result <- suppressMessages(get_split_loc(dat, '(?i)table', 2, 0, 'row'))
    expect_equal(result, 2)

  })


test_that(
  "get_split_loc identifies the first row number after the desired horizontal split
  and the row to split at is 1 row after the first instance of the pattern", {

    dat <- data.frame(
      "row" = c(1, 1, 1, 2, 2, 2),
      "col" = c(1, 2, 3, 1, 2, 3),
      "address" = c("A1", "B1", "C1",
                    "A2", "B2", "C2"),
      "character"= c("table 1", "value", NA,
                     "table 2", "value", NA),
      "numeric" = c(NA, NA, 1, NA, NA, 2)
    )

    result <- suppressMessages(get_split_loc(dat, '(?i)table', 1, -1, 'row'))
    expect_equal(result, 2)

  })


test_that(
  "get_split_loc identifies the col number after the desired vertical split", {

    dat <- data.frame(
      "row" = c(1, 1, 2, 2),
      "col" = c(1, 2, 1, 2),
      "address" = c("A1", "B1",
                    "A2", "B2"),
      "character"= c("table 1", "table 2",
                     "value", "value")
    )

    result <- suppressMessages(get_split_loc(dat, '(?i)table 2', 1, 0, 'col'))
    expect_equal(result, 2)

  })


test_that(
  "get_split_loc identifies the col number after the desired vertical split
  and the col to split at is the second instance of the pattern", {

    dat <- data.frame(
      "row" = c(1, 1, 2, 2),
      "col" = c(1, 2, 1, 2),
      "address" = c("A1", "B1",
                    "A2", "B2"),
      "character"= c("table 1", "table 2",
                     "value", "value")
    )

    result <- suppressMessages(get_split_loc(dat, '(?i)table', 2, 0, 'col'))
    expect_equal(result, 2)

  })


test_that(
  "get_split_loc identifies the col number after the desired vertical split
  and the col to split at is 1 col after the first instance of the pattern", {

    dat <- data.frame(
      "row" = c(4, 4, 5, 5),
      "col" = c(3, 4, 3, 4),
      "address" = c("A1", "B1",
                    "A2", "B2"),
      "character"= c("table 1", "table 2",
                     "value", "value")
    )

    result <- suppressMessages(get_split_loc(dat, '(?i)table 2', 1, 1, 'col'))
    expect_equal(result, 3)

  })


test_that(
  "get_split_loc throws an error if direction is neither row nor col", {

    dat <- data.frame(
      "row" = c(1, 1, 2, 2),
      "col" = c(1, 2, 1, 2),
      "address" = c("A1", "B1",
                    "A2", "B2"),
      "character"= c("table 1", "table 2",
                     "value", "value")
    )

    expect_error(
      suppressMessages(get_split_loc(dat, '(?i)table 2', 1, 0, 'column')),
      "Direction must be 'row' or 'col'"
    )

  })
