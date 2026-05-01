test_that("get_table_data removes data above the first header row ", {

  dat <- data.frame(
    row = as.numeric(1:4),
    col = 1,
    character = c("title", NA, "first_header", NA),
    numeric = c(NA, 1995, NA, 1000)
  )
  expected <- filter(dat, row >= 3)
  result <- suppressMessages(get_table_data(dat, 3))
  expect_equal(result, expected)

})


test_that(
  "get_table_data returns all data and a warning if first header row is NA", {

    dat <- data.frame(
      row = as.numeric(1:4),
      col = 1,
      character = c("title", NA, "first_header", NA),
      numeric = c(NA, 1995, NA, 1000)
    )

    expect_warning(
      result <- suppressMessages(get_table_data(dat, NA)),
      "No data removed from above the first header row"
    )
    expect_equal(result, dat)

  })
