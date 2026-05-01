test_that(
  "fill_missing_fy_start fills NA in fy_start when year_type is financial but
  not calendar", {

    dat <- data.frame(
      year = c("2020", "2020-21", "2021-22"),
      year_type = c("calendar", "financial", "financial"),
      fy_start = c(NA, NA, "2021")
    )

    expected <- mutate(dat, fy_start = c(NA, "2020", "2021"))
    result <- suppressMessages(fill_missing_fy_start(dat))
    expect_equal(result, expected)
  })


test_that(
  "fill_missing_fy_start creates fy_start if it is missing", {

    dat <- data.frame(
      year = c("2020", "2020-21", "2021-22"),
      year_type = c("calendar", "financial", "financial")
    )

    expected <- mutate(dat, fy_start = c(NA, "2020", "2021"))
    result <- suppressMessages(fill_missing_fy_start(dat))

    expect_equal(result, expected)
  })


test_that(
  "fill_missing_fy_start throws an error if there is no year column", {

    dat <- data.frame(
      year_type = c("calendar", "financial", "financial")
    )

    expect_error(
      suppressMessages(fill_missing_fy_start(dat)),
      "Column 'year' not found"
    )
  })


test_that(
  "fill_missing_fy_start throws an error if there is no year_type column", {

    dat <- data.frame(
      year = c("2020", "2020-21", "2021-22")
    )

    expect_error(
      suppressMessages(fill_missing_fy_start(dat)),
      "Column 'year_type' not found"
    )
  })
