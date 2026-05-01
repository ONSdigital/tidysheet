test_that("get_right_block gives expected output with a single column in the left block", {

  dat <- data.frame(
    address = c("A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2"),
    row = c(1, 1, 1, 1, 2, 2, 2, 2),
    col = c(1, 2, 3, 4, 1, 2, 3, 4),
    data_type = c(rep("character", 5), rep("numeric", 2), "character"),
    numeric = c(rep(NA, 5), 100, 22, NA),
    character = c("Year", "primary", "secondary", "Notes",
                  "2021", NA, NA, "note a"))

  expected <- dat %>%
    filter(! col %in% c(1, 4))

  result <- suppressMessages(get_right_block(dat, 2, 4))

  expect_equal(result, expected)
})


test_that("get_right_block gives expected output with no columns in the left block", {
  dat <- data.frame(
    address = c("B1", "C1", "D1", "B2", "C2", "D2"),
    row = c(1, 1, 1, 2, 2, 2),
    col = c(2, 3, 4, 2, 3, 4),
    data_type = c(rep("character", 3), rep("numeric", 2), "character"),
    numeric = c(rep(NA, 3), 100, 22, NA),
    character = c("primary", "secondary", "Notes",
                  NA, NA, "note a"))

  expected <- filter(dat, col != 4)

  result <- suppressMessages(get_right_block(dat, 2, 4))
  expect_equal(result, expected)

})


test_that("get_right_block gives expected output with no columns to the right of the data", {
  dat <- data.frame(
    address = c("A1", "B1", "C1", "A2", "B2", "C2"),
    row = c(1, 1, 1, 2, 2, 2),
    col = c(1, 2, 3, 1, 2, 3),
    data_type = c(rep("character", 3), rep("numeric", 2), "character"),
    numeric = c(rep(NA, 3), NA, 100, 22),
    character = c("Year", "primary", "secondary",
                  "2021", NA, NA))

  expected <- filter(dat, col != 1)

  result <- suppressMessages(get_right_block(dat, 2, NA))
  expect_equal(result, expected)

})

test_that("get_right_block gives expected output with a multiple columns in the left block", {

  dat <- data.frame(
    address = c("A1", "B1", "C1", "D1", "E1", "A2", "B2", "C2", "D2", "E2"),
    row = c(rep(1, 5), rep(2, 5)),
    col = rep(1:5, 2),
    data_type = c(rep("character", 7), rep("numeric", 2), "character"),
    numeric = c(rep(NA, 7), 100, 22, NA),
    character = c("Year", "Month", "primary", "secondary", "Notes",
                  "2021", "Jan", NA, NA, "note a"))

  expected <- dat %>%
    filter(! col %in% c(1, 2, 5))

  result <- suppressMessages(get_right_block(dat, 3, 5))

  expect_equal(result, expected)

})


test_that("get_right_block gives expected output with a multiple columns in the left block and no outer col", {

  dat <- data.frame(
    address = c("A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2"),
    row = c(rep(1, 4), rep(2, 4)),
    col = rep(1:4, 2),
    data_type = c(rep("character", 6), rep("numeric", 2)),
    numeric = c(rep(NA, 6), 100, 22),
    character = c("Year", "Month", "primary", "secondary",
                  "2021", "Jan", NA, NA))

  expected <- dat %>%
    filter(! col %in% c(1, 2))

  result <- suppressMessages(get_right_block(dat, 3, NA))

  expect_equal(result, expected)

})
