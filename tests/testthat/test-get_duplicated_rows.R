test_that("get_duplicated_rows returns the numbers of the rows that are duplicates of the target", {

  dat <- data.frame(
    row = rep(1:6, each = 2),
    col = rep(1:2, by = 6),
    character = rep(c("year", "quarter", rep(NA, 2)), 3),
    numeric = c(NA, NA, 1, 2, NA, NA, 3, 4, NA, NA, 5, 6)
  )

  # rows 3 and 5 are repeats of row 1
  expect_equal(get_duplicated_rows(dat, 1), c(3, 5))

})

test_that("get_duplicated_rows returns empty vector when there are no duplicates", {

  dat <- data.frame(
    row = 1:2,
    col = 1,
    character = c("year", NA),
    numeric = c(NA, 200)
  )

  # rows 3 and 5 are repeats of row 1
  expect_equal(get_duplicated_rows(dat, 1), double())

})

test_that("get_duplicated_rows returns empty vector when there are near duplicates", {

  dat <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    character = c("year", "q", "year", "Q")
    )

  # rows 3 and 5 are repeats of row 1
  expect_equal(get_duplicated_rows(dat, 1), double())

})
