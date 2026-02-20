test_that("remove_duplicated_header_rows returns original data when no header rows are duplicated", {

  # Duplicated header rows do not exist - original data returned.
  dat <- data.frame(
    address = c("A1", "B1", "A2", "B2"),
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    data_type = c(rep("character", 2), rep("numeric", 2)),
    numeric = c(rep(NA, 2), 2020, 22),
    character = c("Year", "Education", NA, NA))

  result <- remove_duplicated_header_rows(dat, 1, 1)

  expect_equal(result, dat)
  expect_no_message(
    remove_duplicated_header_rows(dat, 1, 1),
    message = "Removing duplicated header rows"
  )

})


test_that("remove_duplicated_header_rows gives expected output when a single row of headers exists and is duplicated", {

  dat <- data.frame(
    address = c("A1", "B1", "A2", "B2", "A3", "B3", "A4", "B4"),
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4),
    data_type = rep(c(rep("character", 2), rep("numeric", 2)), times = 2),
    numeric = rep(c(rep(NA, 2), 2020, 22), times = 2),
    character = rep(c("Year", "Education", NA, NA), times = 2)
  )

  expected <- filter(dat, row != 3)

  expect_message(
    result <- remove_duplicated_header_rows(dat, 1, 1),
    "Removing duplicated header rows"
  )
  expect_equal(result, expected)

})


test_that("remove_duplicated_header_rows gives expected output when all rows of
multiple row headers are duplicated", {

  dat <- data.frame(
    address = c(
      "A1", "B1", "A2", "B2", "A3", "B3", "A4", "B4", "A5", "B5", "A6", "B6"
      ),
    row = rep(1:6, each = 2),
    col = rep(1:2, times = 6),
    data_type = rep(
      c(rep("character", 2), "blank", "character", rep("numeric", 2)),
      times = 2
    ),
    numeric = rep(c(rep(NA, 4), 2020, 22), times = 2),
    character = rep(c("Year", "Education", NA, "primary", NA, NA), times = 2)
  )

  expected <- filter(dat, !row %in% c(4:5))

  expect_message(
    result <- remove_duplicated_header_rows(dat, 1, 2),
    "Removing duplicated header rows"
  )

  expect_equal(result, expected)

})


test_that("remove_duplicated_header_rows returns a warning and the original data
when only one row of multiple row headers is duplicated", {

  dat <- data.frame(
    address = c(
      "A1", "B1", "A2", "B2", "A3", "B3", "A4", "B4", "A5", "B5", "A6", "B6"
      ),
    row = rep(1:6, each = 2),
    col = rep(1:2, times = 6),
    data_type = rep(
      c(rep("character", 2), "blank", "character", rep("numeric", 2)),
      times = 2
    ),
    numeric = c(rep(NA, 4), 2020, 22,
                rep(NA, 4), "Jan 2020", 2),
    character = c("Year", "Education", NA, "primary", NA, NA,
                  "Month", "Education", NA, "primary", NA, NA)
  )

  expect_warning(
    result <- suppressMessages(
      remove_duplicated_header_rows(dat, 1, 2),
      "Some header rows are duplicated but others are not"
    )
  )

  expect_equal(result, dat)

})
