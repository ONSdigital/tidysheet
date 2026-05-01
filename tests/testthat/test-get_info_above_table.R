test_that("get_info_above_table removes non-character data and rows from row_num onwards", {

  dat <- data.frame(
      row = 1:4,
      col = 1,
      character = c("title", NA, "first_header", NA),
      numeric = c(NA, 1995, NA, 1000)
      )

  expected <- filter(dat, row == 1)
  result <- suppressMessages(get_info_above_table(dat, 3))
  expect_equal(result, expected)

})


test_that("get_info_above_table returns an empty dataframe when there is no string info above the dat", {

  dat <- data.frame(
    row = 1:2,
    col = 1,
    character = c("first_header", NA),
    numeric = c(NA, 1000)
  )
  expected <- data.frame(
    row = as.integer(),
    col = as.numeric(),
    character = as.character(),
    numeric = as.numeric()
  )
  result <- suppressMessages(get_info_above_table(dat, 1))
  expect_equal(result, expected)

})

