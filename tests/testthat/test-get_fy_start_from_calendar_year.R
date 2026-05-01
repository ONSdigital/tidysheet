test_that("get_fy_start_from_calendar_year uses Q1 as Jan-Mar when q1_is_jan_to_mar is NA", {
  dat <- tibble(
    Year = "2021",
    Quarter = c("Q1", "Q2", "Q3", "Q4", NA),
    Value = 1:5
  )

  expected <- dat %>%
    mutate(fy_start = c(2020, rep(2021, 3), NA)
    )

  quarter_specified <- suppressMessages(
    get_fy_start_from_calendar_year(
      dat = dat,
      q1_is_jan_to_mar = NA,
      calendar_year_to_fy_start = TRUE,
      year_from_pattern = "(?i)year",
      quarter_col_pattern = "(?i)quarter")
  )
  expect_equal(quarter_specified, expected)
})


test_that("get_fy_start_from_calendar_year returns expected output when Q1 is Jan-Mar", {
  dat <- tibble(
    Year = "2021",
    Quarter = c("Q1", "Q2", "Q3", "Q4", NA),
    Month = c("jan-mar", "apr-jun", "jul-sep", "oct-dec", NA),
    Value = 1:5
  )

  expected <- dat %>%
    mutate(fy_start = c(2020, rep(2021, 3), NA)
    )

  quarter_specified <- suppressMessages(
    get_fy_start_from_calendar_year(
      dat = dat,
      q1_is_jan_to_mar = TRUE,
      calendar_year_to_fy_start = TRUE,
      year_from_pattern = "(?i)year",
      quarter_col_pattern = "(?i)quarter")
  )
  expect_equal(quarter_specified, expected)


  month_specified <-  suppressMessages(
    get_fy_start_from_calendar_year(
      dat = dat,
      calendar_year_to_fy_start = TRUE,
      year_from_pattern = "(?i)year",
      month_col_pattern = "(?i)month")
  )
  expect_equal(month_specified, expected)

  none_specified_not_wanted <- get_fy_start_from_calendar_year(
    dat,
    calendar_year_to_fy_start = FALSE,
    year_from_pattern = "(?i)year"
  )
  expect_equal(none_specified_not_wanted, dat)

  patterns_specified_but_not_wanted <-  get_fy_start_from_calendar_year(
    dat = dat,
    calendar_year_to_fy_start = FALSE,
    year_from_pattern = "(?i)year", quarter_col_pattern = "(?i)quarter"
  )
  expect_equal(patterns_specified_but_not_wanted, dat)

})


test_that("get_fy_start_from_calendar_year returns expected output when Q1 is Apr - Jun", {
  dat <- tibble(
    Year = "2021",
    Quarter = c("Q4", "Q1", "Q2", "Q3", NA),
    Month = c("jan-mar", "apr-jun", "jul-sep", "oct-dec", NA),
    Value = 1:5
  )

  expected <- mutate(dat, fy_start = c(2020, rep(2021, 3), NA))

  quarter_specified <- suppressMessages(
    get_fy_start_from_calendar_year(
      dat = dat,
      q1_is_jan_to_mar = FALSE,
      calendar_year_to_fy_start = TRUE,
      year_from_pattern = "(?i)year",
      quarter_col_pattern = "(?i)quarter")
  )
  expect_equal(quarter_specified, expected)

  month_specified <-  suppressMessages(
    get_fy_start_from_calendar_year(
      dat = dat,
      q1_is_jan_to_mar = FALSE,
      calendar_year_to_fy_start = TRUE,
      year_from_pattern = "(?i)year",
      month_col_pattern = "(?i)month")
  )
  expect_equal(month_specified, expected)

})


test_that("get_fy_start_from_calendar_year does nothing if calendar_year_to_fy_start is FALSE", {
  dat <- tibble(
    Year = "2021-22",
    Quarter = c("Q4", "Q1", "Q2", "Q3", NA),
    Month = c("jan-mar", "apr-jun", "jul-sep", "oct-dec", NA),
    Value = 1:5
  )

  result <-  get_fy_start_from_calendar_year(
    dat = dat,
    q1_is_jan_to_mar = FALSE,
    calendar_year_to_fy_start = FALSE,
    year_from_pattern = "(?i)year",
    month_col_pattern = "(?i)month"
    )

  expect_equal(result, dat)
})


test_that("get_fy_start_from_calendar_year does nothing if calendar_year_to_fy_start is NA", {
  dat <- tibble(
    Year = "2021-22",
    Quarter = c("Q4", "Q1", "Q2", "Q3", NA),
    Month = c("jan-mar", "apr-jun", "jul-sep", "oct-dec", NA),
    Value = 1:5
  )

  result <-  get_fy_start_from_calendar_year(
    dat = dat,
    q1_is_jan_to_mar = FALSE,
    calendar_year_to_fy_start = NA,
    year_from_pattern = "(?i)year",
    month_col_pattern = "(?i)month"
  )

  expect_equal(result, dat)
})


test_that("get_fy_start_from_calendar_year gets fy_start right if year is a
financial year, regardless of q1_is_jan_to_mar setting", {

  dat <- tibble(
    Year = "2021-22",
    Quarter = c("Q4", "Q1", "Q2", "Q3", NA),
    Month = c("jan-mar", "apr-jun", "jul-sep", "oct-dec", NA),
    Value = 1:5
  )

  expected <- mutate(dat, fy_start = 2021)

  result_q1_is_jan_to_mar <- suppressMessages(
    get_fy_start_from_calendar_year(
      dat = dat,
      q1_is_jan_to_mar = TRUE,
      calendar_year_to_fy_start = TRUE,
      year_from_pattern = "(?i)year",
      quarter_col_pattern = "(?i)quarter"
    )
  )

  result_q1_is_not_jan_to_mar <- suppressMessages(
    get_fy_start_from_calendar_year(
      dat = dat,
      q1_is_jan_to_mar = FALSE,
      calendar_year_to_fy_start = TRUE,
      year_from_pattern = "(?i)year",
      quarter_col_pattern = "(?i)quarter"
    )
  )

  expect_equal(result_q1_is_jan_to_mar, expected)
  expect_equal(result_q1_is_not_jan_to_mar, expected)
})


test_that("get_fy_start_from_calendar_year gets fy_start right if only some years
are calendar when q1_is_jan_to_mar is FALSE", {
  dat <- tibble(
    Year = c("2020", "2020", "2021-22"),
    Quarter = c("Q4", "Q1",  NA),
    Month = c("jan-mar", "apr-jun", NA)
  )

  expected <- mutate(dat, fy_start = c(2019, 2020, 2021))

  result <- suppressMessages(
    get_fy_start_from_calendar_year(
      dat = dat,
      q1_is_jan_to_mar = FALSE,
      calendar_year_to_fy_start = TRUE,
      year_from_pattern = "(?i)year",
      quarter_col_pattern = "(?i)quarter"
    )
  )

  expect_equal(result, expected)
})


test_that("get_fy_start_from_calendar_year gets fy_start right if only some years
are calendar when q1_is_jan_to_mar is TRUE", {
  dat <- tibble(
    Year = c("2020", "2020", "2021-22"),
    Quarter = c("Q1", "Q2",  NA),
    Month = c("jan-mar", "apr-jun", NA)
  )

  expected <- mutate(dat, fy_start = c(2019, 2020, 2021))

  result <- suppressMessages(
    get_fy_start_from_calendar_year(
      dat = dat,
      q1_is_jan_to_mar = TRUE,
      calendar_year_to_fy_start = TRUE,
      year_from_pattern = "(?i)year",
      quarter_col_pattern = "(?i)quarter"
    )
  )

  expect_equal(result, expected)
})


test_that("get_fy_start_from_calendar_year can handle month and year being specified", {

  dat <- tibble(
    Year = c("2021", "2022", "2022", "2022"),
    Quarter = c("Q3",  NA, "Q1", NA),
    Months = c("oct - dec", "jan - mar", NA, "Jul"),
    Value = 1:4
  )

  expected <- mutate(dat, fy_start = c(2021, 2021, 2022, 2022))

  result <- suppressMessages(
    get_fy_start_from_calendar_year(
      dat, calendar_year_to_fy_start = TRUE,
      q1_is_jan_to_mar = FALSE,
      year_from_pattern = "(?i)year",
      quarter_col_pattern = "(?i)quarter",
      month_col_pattern = "(?i)month"
    )
  )

  expect_equal(result, expected)
})


test_that("get_fy_start_from_calendar_year throws expected errors", {
  dat <- tibble(
    Year = "2021",
    Quarter = c("Q1", "Q2", "Q3", "Q4"),
    Month = c("jan-mar", "apr-jun", "jul-sep", "oct-dec"),
    Value = 1:4
  )

  with_fy_start_already <- tibble(
    Year = "2021",
    Quarter = c("Q1", "Q2", "Q3", "Q4"),
    Month = c("jan-mar", "apr-jun", "jul-sep", "oct-dec"),
    fy_start = c("2020", "2021", "2021", "2021"),
    Value = 1:4
  )

  # none specified, but wanted
  expect_error(suppressMessages(
    get_fy_start_from_calendar_year(
      dat,
      calendar_year_to_fy_start = TRUE,
      year_from_pattern = "(?i)year"
    ),
  ),
  "calendar_year_to_fy_start is TRUE, but neither month_col_pattern, nor quarter_col_pattern has been supplied"
  )

  # fy_start is already a col in dat
  expect_error(suppressMessages(
    get_fy_start_from_calendar_year(
      with_fy_start_already,
      calendar_year_to_fy_start = TRUE,
      year_from_pattern = "(?i)year",
      month_col_pattern = "(?i) month"
    ),
  ),
  "calendar_year_to_fy_start is TRUE, but fy_start is already a column"
  )

})


test_that("get_fy_start_from_calendar_year raises an error if month is specified but not found", {
  dat <- tibble(
    Year = "2021",
    Quarter = c("Q1", "Q2", "Q3", "Q4", NA),
    Value = 1:5
  )

  expect_error(
    suppressMessages(suppressWarnings(
      get_fy_start_from_calendar_year(
        dat = dat,
        q1_is_jan_to_mar = NA,
        calendar_year_to_fy_start = TRUE,
        year_from_pattern = "(?i)year",
        month_col_pattern = "(?i)month")
    )),
    "month_col_pattern and/or calendar_year_to_fy_start may need to change"
  )
})


test_that("get_fy_start_from_calendar_year raises an error if quarter is specified but not found", {
  dat <- tibble(
    Year = "2021",
    Quarter = c("Q1", "Q2", "Q3", "Q4", NA),
    Value = 1:5
  )

  expect_error(
    suppressMessages(suppressWarnings(
      get_fy_start_from_calendar_year(
        dat = dat,
        q1_is_jan_to_mar = NA,
        calendar_year_to_fy_start = TRUE,
        year_from_pattern = "(?i)year",
          quarter_col_pattern = "q")
    )),
    "quarter_col_pattern and/or calendar_year_to_fy_start may need to change."
  )
})


test_that("get_fy_start_from_calendar_year gives a warning if quarter and month
dont give the same result and fy_start is set to NA", {
  dat <- tibble(
    Year = "2021",
    Quarter = "Q1",
    Month = 4,
    Value = 1
  )

  expected <- mutate(dat, fy_start = NA)

  expect_warning(
    suppressMessages(
      get_fy_start_from_calendar_year(
        dat = dat,
        q1_is_jan_to_mar = NA,
        calendar_year_to_fy_start = TRUE,
        year_from_pattern = "(?i)year",
        quarter_col_pattern = "(?i)quarter",
        month_col_pattern = "(?i)month"
        )
    ),
    "fy_start calculated for month did not match fy_start calculated for quarter in 1 row"
  )
})

