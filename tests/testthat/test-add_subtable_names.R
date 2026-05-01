test_that("add_subtable_names returns dat if subtable_names is NA", {

  dat <- data.frame(row = c(4, 2), col = 2, value = c(2, 1))
  result <- add_subtable_names(dat, NA)
  expect_equal(result, dat)

})


test_that("add_subtable_names adds subtitles to the correct rows of dat even if
row order is messed up", {
  subtable_names <- data.frame(
    row = c(3, 1),
    subtitle = c("table B", "table A")
  )
  dat <- data.frame(row = c(4, 2), col = 2, value = c(2, 1))
  expected <- data.frame(
    row = 1:4,
    col = c(NA, 2, NA, 2),
    value = c(NA, 1, NA, 2),
    subtitle = c("table A", "table A", "table B", "table B")
  )
  result <- suppressMessages(add_subtable_names(dat, subtable_names))
  expect_equal(result, expected)
})


test_that("add_subtable_names works even if the row a title is on is not in dat", {
  subtable_names <- data.frame(
      row = c(1, 3),
      subtitle = c("table A", "table B")
      )
  dat <- data.frame(row = c(2, 4), col = 2, value = 1:2)
  expected <- data.frame(
    row = 1:4,
    col = c(NA, 2, NA, 2),
    value = c(NA, 1, NA, 2),
    subtitle = c("table A", "table A", "table B", "table B")
  )
  result <- suppressMessages(add_subtable_names(dat, subtable_names))
  expect_equal(result, expected)
})


test_that("add_subtable_names works even if the row a title is on is not in dat", {
  subtable_names <- data.frame(
    row = c(1, 3),
    subtitle = c("table A", "table B")
  )
  dat <- data.frame(row = c(2, 4), col = 2, value = 1:2)
  expected <- data.frame(
    row = 1:4,
    col = c(NA, 2, NA, 2),
    value = c(NA, 1, NA, 2),
    subtitle = c("table A", "table A", "table B", "table B")
  )
  result <- suppressMessages(add_subtable_names(dat, subtable_names))
  expect_equal(result, expected)
})
