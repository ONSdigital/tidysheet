test_that("get_year_info_from_fy_end returns expected output for fy_start", {
  dat <- data.frame("fy_end" = c(2021, 2022, NA), value = c(1:3))

  expected <- mutate(dat, fy_start = c(2020, 2021, NA))

  result <- suppressMessages(
    get_year_info_from_fy_end(
    dat = dat,
    fy_end_pattern = "end",
    fy_start_from_fy_end=TRUE
    )
  )

  expect_equal(result, expected)
})


test_that("get_year_info_from_fy_end returns expected output for financial year", {

  dat <- data.frame("fy_end" = c(2021, 2022, NA), value = c(1:3))
  expected <- mutate(dat, year = c("2020-21", "2021-22", NA))

  result <- suppressMessages(
    get_year_info_from_fy_end(dat, "end", fy_from_fy_end=TRUE)
  )

  expect_equal(result, expected)

})


test_that("get_year_info_from_fy_end uses FALSE as a default for fy_satrt_from_fy_end when it is given as NA", {

  dat <- data.frame("fy_end" = c(2021, 2022, NA), value = c(1:3))
  expected <- mutate(dat, year = c("2020-21", "2021-22", NA))

  result <- suppressMessages(
    get_year_info_from_fy_end(dat, "end", NA, fy_from_fy_end=TRUE)
  )

  expect_equal(result, expected)

})


test_that("get_year_info_from_fy_end uses FALSE as a default for fy_from_fy_end when it is given as NA", {

  dat <- data.frame("fy_end" = c(1991, 1992, 1993), value = c(1:3),
                    "fy_start" = c(2020, 2021, NA),
                    "year" = c("2020-21", "2021-22", NA))

  result <- suppressWarnings(
    suppressMessages(
      get_year_info_from_fy_end(dat, "end", TRUE, NA)
    )
  )

  expect_equal(result, dat)

})


test_that("get_year_info_from_fy_end raises an error if no pattern is given but fy_start_from_fy_end is ", {

  dat <- data.frame("fy_end" = c(1991, 1992, 1993), value = c(1:3),
                    "fy_start" = c(2020, 2021, NA),
                    "year" = c("2020-21", "2021-22", NA))

  expect_error(
    suppressMessages(
      get_year_info_from_fy_end(dat, NA, TRUE, NA)
    ),
    "A regular expression to identify the column containing fy end must be supplied"
  )

})


test_that("get_year_info_from_fy_end raises an error if no pattern is given but fy_from_fy_end is ", {

  dat <- data.frame("fy_end" = c(1991, 1992, 1993), value = c(1:3),
                    "fy_start" = c(2020, 2021, NA),
                    "year" = c("2020-21", "2021-22", NA))

  expect_error(
    suppressMessages(
      get_year_info_from_fy_end(dat, NA, NA, TRUE)
    ),
    "A regular expression to identify the column containing fy end must be supplied"
  )

})


test_that("get_year_info_from_fy_end returns expected output for both fy_start and financial year", {
  dat <- data.frame("fy_end" = c(2021, 2022, NA), value = c(1:3))

  expected <- dat %>%
    mutate(fy_start = c(2020, 2021, NA),
           year = c("2020-21", "2021-22", NA))

  result <- suppressMessages(
    get_year_info_from_fy_end(
      dat, "end", fy_start_from_fy_end=TRUE, fy_from_fy_end=TRUE
    )
  )

  expect_equal(result, expected)

})


test_that("get_year_info_from_fy_end does not overwrite existing columns for fy_start", {
  dat <- data.frame("fy_end" = c(1991, 1992, 1993), value = c(1:3),
                    "fy_start" = c(2020, 2021, NA),
                    "year" = c("2020-21", "2021-22", NA))

  expect_warning(
    suppressMessages(
      result <- get_year_info_from_fy_end(dat, "end", fy_start_from_fy_end=TRUE)
    ),
    "'fy_start' is already a column in dat so will not be created from 'fy_end'"
  )

  expect_equal(result, dat)


})


test_that("get_year_info_from_fy_end does not overwrite existing columns for financial year", {
  dat <- data.frame("fy_end" = c(1991, 1992, 1993), value = c(1:3),
                    "fy_start" = c(2020, 2021, NA),
                    "year" = c("2020-21", "2021-22", NA))

  expect_warning(
    suppressMessages(
      result <- get_year_info_from_fy_end(dat, "end", fy_from_fy_end=TRUE)
    ),
    "'year' is already a column in dat so will not be created from 'fy_end'"
  )

  expect_equal(result, dat)


})


test_that("get_year_info_from_fy_end does not give the financial year if multiple columns match the fy_end pattern", {
  dat <- data.frame("fy_end" = c(1991, 1992, 1993), value = c(1:3),
                    "fy_start" = c(2020, 2021, NA))

  expect_error(
    suppressMessages(
      get_year_info_from_fy_end(dat, "no matches", fy_from_fy_end=TRUE)
    ),
    "No columns found"
  )


})
