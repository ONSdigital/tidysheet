test_that("get_fy_from_fy_end returns expected outpput when years are all valid", {
  dat <- data.frame("fy_end" = c(2021, 2022, NA), value = 1:3)
  expected <- mutate(dat, year = c("2020-21", "2021-22", NA))

  result <- suppressMessages(get_fy_from_fy_end(dat, "fy_end"))

  expect_equal(result, expected)
})


test_that("get_fy_from_fy_end returns expected outpput when a year has more than 4 characters", {
  dat <- data.frame("fy_end" = c(2021, 2022, NA, 20222), value = 1:4)
  expected <- mutate(dat, year = c("2020-21", "2021-22", NA, NA))

  expect_warning(
    suppressMessages(
      result <- get_fy_from_fy_end(dat, "fy_end")
    ),
    "1 rows in 'fy_end' either contained no numbers or"
  )

  expect_equal(result, expected)
})


test_that("get_fy_from_fy_end returns expected output when a year is not a number", {
  dat <- data.frame("fy_end" = "word")
  expected <- mutate(dat, year = as.character(NA))

  w <- capture_warnings(suppressMessages(get_fy_from_fy_end(dat, "fy_end")))
  expect_match(w, "1 rows in 'fy_end' either contained no numbers", all = FALSE)

  result <- suppressWarnings(
    suppressMessages(
      get_fy_from_fy_end(dat, "fy_end")
      )
    )

  expect_equal(result, expected)
})

