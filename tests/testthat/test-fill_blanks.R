test_that("fill_blanks fills blanks uses 'down' as the default direction", {

  dat <- data.frame(
    var1 = c("colour", NA, NA, "wavelength"),
    var2 = c("red", "green", "blue", "short")
  )
  expected <- mutate(dat, var1 = c("colour", "colour", "colour", "wavelength"))

  result <- suppressMessages(fill_blanks(dat, "var1", NA))
  expect_equal(result, expected)

})


test_that("fill_blanks raises an error if an invalid fill_dir is specified", {

  dat <- data.frame(
    var1 = c("colour", NA, NA, "wavelength"),
    var2 = c("red", "green", "blue", "short")
  )
  expected <- mutate(dat, var1 = c("colour", "colour", "colour", "wavelength"))

  expect_error(
    suppressMessages(fill_blanks(dat, "var1", "left")),
    "fill_columns_fill_dirs must only contain the following"
    )

})


test_that("fill_blanks fills blanks does nothing if the relevant vars are NA", {

  dat <- data.frame(
    var1 = c("colour", NA, NA, "wavelength"),
    var2 = c("red", "green", "blue", "short")
  )

  result <- suppressMessages(fill_blanks(dat, NA, NA))
  expect_equal(result, dat)

})


test_that("fill_blanks throws an error if fill_dirs is specified but not column_names", {

  dat <- data.frame(
    var1 = c("colour", NA, NA, "wavelength"),
    var2 = c("red", "green", "blue", "short")
  )

  expect_error(
    suppressMessages(fill_blanks(dat, NA, "down")),
    "fill_columns_fill_dirs is specified but not fill_columns_column_names."
  )

})


test_that(
  "fill_blanks throws an error if fill_dirs is not NA and is a different
  length to column_names", {

  dat <- data.frame(
    var1 = c("colour", NA, NA, "wavelength"),
    var2 = c("red", "green", "blue", "short")
  )

  expect_error(
    suppressMessages(fill_blanks(dat, "var1", c("up","down"))),
  "fill_columns_column_names must contain the same number of elements as fill_columns_fill_dirs"
  )

})


test_that("fill_blanks fills blanks with the first non blank value above them", {

  dat <- data.frame(
      var1 = c("colour", NA, NA, "wavelength"),
      var2 = c("red", "green", "blue", "short")
      )
  expected <- mutate(dat, var1 = c("colour", "colour", "colour", "wavelength"))

  result <- suppressMessages(fill_blanks(dat, "var1", "down"))
  expect_equal(result, expected)

})


test_that("fill_blanks works as expected even if the original data are out of order", {

  dat <- data.frame(
    row = c(4, 1, 3, 2),
    var1 = c("wavelength", "colour", NA, NA),
    var2 = c("short", "red", "green", "blue")
  )
  expected <- dat %>%
    arrange(row) %>%
    mutate(var1 = c("colour", "colour", "colour", "wavelength"))

  result <- suppressMessages(fill_blanks(dat, "var1", "down"))
  expect_equal(result, expected)

})


test_that("fill_blanks fills blanks with the first non blank value below them", {

  dat <- data.frame(
    var1 = c("colour", NA, NA, "wavelength"),
    var2 = c("red", "green", "blue", "short")
  )
  expected <- dat %>%
    mutate(var1 = c("colour", "wavelength", "wavelength", "wavelength"))

  result <- suppressMessages(fill_blanks(dat, "var1", "up"))

  expect_equal(result, expected)

})


test_that(
  "fill_dirs fills blanks of multiple columns", {

    dat <- data.frame(
      var1 = c("colour", NA, NA, "wavelength"),
      var2 = c("red", NA, "blue", "short"),
      var3 = c(NA, NA, NA, "paint")
    )

    expected <- dat %>%
      mutate(var1 = c("colour", "colour", "colour", "wavelength"),
             var3 = "paint")

    result <- suppressMessages(
      fill_blanks(dat, c("var1", "var3"), c("down", "up"))
    )

    expect_equal(result, expected)

  })
