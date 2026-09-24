test_that("get_first_data_row_number works with messed up headers after a simple header row", {
  dat <- data.frame(
      row = rep(1:5, each = 3),
      col = rep(1:3, times = 5),
      character = c(NA, "A", "B",
                    NA, "messed", NA,
                    NA, "up", NA,
                    NA, "header", "simple",
                    "count", NA, NA),
      numeric = c(rep(NA, 13), 4, 1),
      is_blank = c(TRUE, FALSE, FALSE,
                  TRUE, FALSE, TRUE,
                  TRUE, FALSE, TRUE,
                  TRUE, FALSE, FALSE,
                  FALSE, FALSE, FALSE)
      )

  result <- suppressMessages(
    get_first_data_row_number(dat, 1, c("type", "name"), "mess", "simp")
  )

  expect_equal(result, 5)

})


test_that("get_first_data_row_number works with messed up headers before a simple header row", {
  dat <- data.frame(
      row = rep(1:5, each = 3),
      col = rep(1:3, times = 5),
      character = c(NA, "messed", NA,
                    NA, "up", NA,
                    NA, "header", "simple",
                    NA, "A", "B",
                    "count", NA, NA),
      numeric = c(rep(NA, 13), 4, 1),
      is_blank = c(TRUE, FALSE, TRUE,
                  TRUE, FALSE, TRUE,
                  TRUE, FALSE, FALSE,
                  TRUE, FALSE, FALSE,
                  FALSE, FALSE, FALSE)
      )
  result <- suppressMessages(
    get_first_data_row_number(dat, 1, c("type", "name"), "mess", "simp")
  )
  expect_equal(result, 5)
})


test_that("get_first_data_row_number works with a blank row between messed up headers and simple headers", {

  dat <- data.frame(
      row = rep(1:6, each = 3),
      col = rep(1:3, times = 6),
      character = c(NA, "messed", NA,
                    NA, NA, NA,
                    NA, "up", NA,
                    NA, "header", "simple",
                    NA, "A", "B",
                    "count", NA, NA),
      numeric = c(rep(NA, 16), 4, 1),
      is_blank = c(TRUE, FALSE, TRUE,
                  TRUE, TRUE, TRUE,
                  TRUE, FALSE, TRUE,
                  TRUE, FALSE, FALSE,
                  TRUE, FALSE, FALSE,
                  FALSE, FALSE, FALSE)
      )
  result <- suppressMessages(
    get_first_data_row_number(dat, 1, c("type", "name"), "mess", "simp")
  )
  expect_equal(result, 6)

})


test_that("get_first_data_row_number works with a blank row between messed up headers and simple headers", {

  dat <- data.frame(
    row = rep(1:6, each = 3),
    col = rep(1:3, times = 6),
    character = c(NA, "messed", NA,
                  NA, "up", NA,
                  NA, "header", "simple",
                  NA, NA, NA,
                  NA, "A", "B",
                  "count", NA, NA),
    numeric = c(rep(NA, 16), 4, 1),
    is_blank = c(TRUE, FALSE, TRUE,
                 TRUE, FALSE, TRUE,
                 TRUE, FALSE, FALSE,
                 TRUE, TRUE, TRUE,
                 TRUE, FALSE, FALSE,
                 FALSE, FALSE, FALSE)
  )
  result <- suppressMessages(
    get_first_data_row_number(dat, 1, c("type", "name"), "mess", "simp")
  )
  expect_equal(result, 6)

})


test_that("get_first_data_row_number works with simple headers", {

  dat <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    character = c(NA, "A", NA,
                  NA, "Aa", "Ab",
                  "count", NA, NA),
    numeric = c(rep(NA, 7), 2, 2),
    is_blank = c(TRUE, FALSE, TRUE,
                 TRUE, FALSE, FALSE,
                 FALSE, FALSE, FALSE)
  )
  result <- suppressMessages(
    get_first_data_row_number(dat, 1, c("type", "name"), NA, NA)
  )
  expect_equal(result, 3)

})


test_that("get_first_data_row_number works with simple headers starting on row 2", {

  dat <- data.frame(
    row = rep(1:4, each = 3),
    col = rep(1:3, times = 4),
    character = c("title", NA, NA,
                  NA, "A", NA,
                  NA, "Aa", "Ab",
                  "count", NA, NA),
    numeric = c(rep(NA, 10), 2, 2),
    is_blank = c(FALSE, TRUE, TRUE,
                 TRUE, FALSE, TRUE,
                 TRUE, FALSE, FALSE,
                 FALSE, FALSE, FALSE)
  )
  result <- suppressMessages(
    get_first_data_row_number(dat, 2, c("type", "name"), NA, NA)
  )
  expect_equal(result, 4)

})
