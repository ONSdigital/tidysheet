test_that("get_fy_start_from_fy_end returns expected output", {
  dat <- data.frame(fy_end = c("2000", "this 2020", "blue"))
  expected <- mutate(dat, fy_start = c(1999, 2019, NA))

  expect_warning(
    suppressMessages(
       result <- get_fy_start_from_fy_end(dat, "fy_end")
    ),
    "1 rows in 'fy_end' did not contain.*YYYY"
  )

  expect_equal(result, expected)
})

