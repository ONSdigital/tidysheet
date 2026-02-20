test_that("combine_columns returns dat if no relevant variables are specified", {
  dat <- data.frame(col1 = c("A", "B"))
  expect_equal(combine_columns(dat, NA, NA, NA), dat)

})


test_that("combine_columns throws an error if some but not all relevant variables are specified", {
  dat <- data.frame(col1 = c("A", "B"))
  expect_error(
    combine_columns(dat, "col1", NA, NA),
    "If one is given, the other must also be"
  )
  expect_error(
    combine_columns(dat, NA, "description", NA),
    "If one is given, the other must also be"
  )
})


test_that("combine_columns successfully combines multiple string columns", {
  dat <- data.frame(
    col1 = c("A", "B"),
    col2 = c("a", NA),
    col3 = c("Aa", "Bb")
  )

  expected <- data.frame(newcol = c("A - a - Aa", "B - Bb"))

  expect_message(
    result <- combine_columns(
      dat, c("col1", "col2", "col3"), "newcol", NA
      ),
    "Concatenated strings will be given as 'newcol'"
  )

  expect_equal(result, expected)

})


test_that("combine_columns throws an error when only one value is given for columns_to_combine_patterns", {

  dat <- data.frame(
    col1 = c("A", "B"),
    col2 = c("Aa", "Bb")
  )

  expect_error(
    combine_columns(dat, "col1", "newcol", NA),
    "More than one column name must be given in columns_to_combine_patterns"
    )

})


test_that("combine_columns throws an error when columns_to_combine_patterns are not found in the column names of dat", {
  dat <- data.frame(
    col1 = c("A", "B"),
    col2 = c("Aa", "Bb")
  )

  expect_error(
    suppressMessages(
      combine_columns(dat, c("col1", "col3"), "newcol", NA)
    ),
    "'col3' does not match any columns in the data"
  )

})


test_that("combine_columns gives a warning when it overwrites any column other than those in columns_to_combine_patterns", {
  dat <- data.frame(
    col1 = c("A", "B"),
    col2 = c("Aa", "Bb"),
    year = "2025"
  )

  expect_warning(
    suppressMessages(
      combine_columns(dat, c("col1", "col2"), "year", NA)
    ),
    "'year' will be overwritten by the concatenation of 'col1', 'col2'"
  )

})


test_that("combine_columns can separately concatenate two sets of columns", {
  dat <- data.frame(
    col1 = c("A", "B"),
    col2 = c("Aa", "Bb"),
    col3 = c("1", "2"),
    col4 = c("C", "D"),
    col5 = c("c", "d")
  )

  expected <- data.frame(
    col1 = c("A - Aa - 1", "B - Bb - 2"),
    col4 = c("C - c", "D - d")
  )

  result <- suppressMessages(
      combine_columns(
        dat,
        c("col1", "col2", "col3", "col4", "col5"), c("col1", "col4"),
        c(3, 2))
    )

  expect_equal(result, expected)
})

