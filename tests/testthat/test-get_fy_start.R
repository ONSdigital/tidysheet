test_that("get_fy_start works when only financial year end is available", {
  dat <- data.frame(
    fy_end = c(2021, 2022),
    value = c(1, 2)
  )
  expected <- dat %>%
    mutate(fy_start = c(2020, 2021),
           year = c("2020-21", "2021-22"))

  result <- suppressMessages(
    get_fy_start(
      dat, fy_from_fy_end = TRUE, fy_start_from_fy_end = TRUE,
      fy_end_pattern = "fy"
    )
  )

  expect_equal(result, expected)

})


test_that("get_fy_start works with month", {

  dat <- data.frame(
      Year = c("2021", "2021"),
      month = c("jan-mar", "apr-jun"),
      value = c(1, 2)
      )

  expected <- dat %>%
    mutate(year = "2021",
           fy_start = c(2020, 2021))

  result <- suppressMessages(
    get_fy_start(
      dat, year_from_pattern = "Year", month_pattern = "month",
      calendar_to_fy_start = TRUE,  q1_is_jan_to_mar = FALSE,
    )
  )

  expect_equal(result, expected)
})
