test_that("replace_blanks fills blanks in one column with values from another", {
  dat <- data.frame(
      Code = c(NA, "E08000001"),
      Region_code = c("E92000001", "E12000002")
      )

  expected <- data.frame(
    Code = c("E92000001", "E08000001"),
    Region_code = c("E92000001", "E12000002")
  )

  result <- suppressMessages(
    replace_blanks(
      dat,
      to_columns = "(?i)^code$",
      from_columns = "(?i)region.code"
    )
  )
  expect_equal(result, expected)
})


test_that("replace_blanks can act on multiple columns", {
  dat <- data.frame(
    Code = c(NA, "E08000001"),
    Region_code = c("E92000001", "E12000002"),
    LA = c(NA, "Bolton"),
    Region = c("England", "North West")
  )
  expected <- data.frame(
    Code = c("E92000001", "E08000001"),
    Region_code = c("E92000001", "E12000002"),
    LA = c("England", "Bolton"),
    Region = c("England", "North West")
  )
  result <- suppressMessages(
    replace_blanks(
      dat,
      to_columns = c("(?i)^code$", "(?i)la"),
      from_columns = c("(?i)region.code", "(?i)region$")
    )
  )
  expect_equal(result, expected)
})


test_that("replace_blanks returns original dat if to and from are not supplied", {
  dat <- data.frame(
    Code = c(NA, "E08000001"),
    Region_code = c("E92000001", "E12000002")
  )

  result <- replace_blanks(dat)
  expect_equal(result, dat)
})


test_that("replace_blanks returns original dat if to and from are NA", {
  dat <- data.frame(
    Code = c(NA, "E08000001"),
    Region_code = c("E92000001", "E12000002")
  )

  result <- replace_blanks(dat, NA, NA)

  expect_equal(result, dat)
})


test_that("replace_blanks throws an error if to is NA but from is supplied", {
  dat <- data.frame(
    Code = c(NA, "E08000001"),
    Region_code = c("E92000001", "E12000002")
  )

  expect_error(suppressMessages(
    replace_blanks(dat, NA, "Region_code")
    ),
    "Only one .* is specified"
  )

})


test_that("replace_blanks throws an error if to is longer than from", {
  dat <- data.frame(
    Code = c(NA, "E08000001"),
    Region_code = c("E92000001", "E12000002")
  )

  expect_error(suppressMessages(
    replace_blanks(dat, c("Code", "Region_code"), "Region_code")
    ),
    "must have the same number of elements"
  )

})


test_that("replace_blanks throws an error if to is shorter than from", {
  dat <- data.frame(
    Code = c(NA, "E08000001"),
    Region_code = c("E92000001", "E12000002")
  )

  expect_error(suppressMessages(
    replace_blanks(dat, "Region_code", c("Code", "Region_code"))
  ),
  "must have the same number of elements"
  )

})


test_that("replace_blanks gives a warning if to pattern is not found, but continues with other columns", {
  dat <- data.frame(
    Code = c(NA, "E08000001"),
    Region_code = c("E92000001", "E12000002"),
    still_replaced = c("ok", NA),
    still_used = "this"
  )
  expected <- mutate(dat, still_replaced = c("ok", "this"))

  expect_warning(
    result <- suppressMessages(
      replace_blanks(dat, c("nope", "replaced"), c("Region_code", "used"))
    ),
    "no matching column has been found"
  )

  expect_equal(result, expected)

})


test_that("replace_blanks gives a warning if from pattern is not found, but continues with other columns", {
  dat <- data.frame(
    Code = c(NA, "E08000001"),
    Region_code = c("E92000001", "E12000002"),
    still_replaced = c("ok", NA),
    still_used = "this"
  )

  expected <- mutate(dat, still_replaced = c("ok", "this"))

  expect_warning(
    result <- suppressMessages(
      replace_blanks(dat, c("Code", "replaced"), c("nope", "still_used"))
    ),
    "no matching column has been found"
  )

  expect_equal(result, expected)

})


test_that("replace_blanks gives a warning if to pattern matches multiple columns, but continues with other columns", {
  dat <- data.frame(
    Code = c(NA, "E08000001"),
    Region_code = c("E92000001", "E12000002"),
    still_replaced = c("ok", NA),
    still_used = "this"
  )
  expected <- mutate(dat, still_replaced = c("ok", "this"))

  expect_warning(
    result <- suppressMessages(
      replace_blanks(dat, c("(?i)code", "replaced"), c("Region_code", "used"))
    ),
    "Multiple matching column names"
  )

  expect_equal(result, expected)

})


test_that("replace_blanks gives a warning if from pattern matches multiple columns, but continues with other columns", {
  dat <- data.frame(
    Code = c(NA, "E08000001"),
    Region_code = c("E92000001", "E12000002"),
    still_replaced = c("ok", NA),
    still_used = "this"
  )
  expected <- mutate(dat, still_replaced = c("ok", "this"))

  expect_warning(
    result <- suppressMessages(
      replace_blanks(dat, c("Code", "replaced"), c("(?i)code", "used"))
    ),
    "Multiple matching column names"
  )

  expect_equal(result, expected)

})
