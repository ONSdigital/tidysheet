dat <- data.frame(
  a = c("ant", "apple", NA, "anger"),
  b = c("badger", "bang", "boredom", NA)
)

test_that(
  "drop_rows_with_values returns an unchanged dataframe if col_patterns and
  value_patterns are NA", {

    result <- suppressMessages(drop_rows_with_values(dat, NA, NA))
    expect_equal(result, dat)
  })


test_that(
  "drop_rows_with_values throws an error if col_patterns is NA but
  value_patterns is not NA", {

    expect_error(
      suppressMessages(drop_rows_with_values(dat, "b", NA)),
      "One is present but one is missing."
    )
  })


test_that(
  "drop_rows_with_values throws an error if col_patterns is not NA but
  value_patterns is NA", {

    expect_error(
      suppressMessages(drop_rows_with_values(dat, "a", NA)),
      "One is present but one is missing."
    )
  })


test_that(
  "drop_rows_with_values throws an error if col_patterns is not the same length
  as value_patterns", {

    expect_error(
      suppressMessages(
        drop_rows_with_values(dat, c("b", "a"), c("bad", "ang", "ap"))
        ), "must have the same number of elements"
      )
  })


test_that(
  "drop_rows_with_values gives a warning if col_patterns does not match any
   column names, and returns the data untouched.", {

     expect_warning(
       result <- suppressMessages(drop_rows_with_values(dat, "c", "nope")),
       "no matching column has been found"
     )

     expect_equal(result, dat)
   })


test_that(
  "drop_rows_with_values gives a warning if col_patterns matches multiple
   column names", {

     expect_warning(
       result <- suppressMessages(drop_rows_with_values(dat, "a|b", "nope")),
       "multiple matching column names have been found"
     )

     expect_equal(result, dat)

   })


test_that(
  "drop_rows_with_values drops rows with a specified value in a specified
  column. Do this separately across multiple columns", {

    dat <- data.frame(
      a = c("ant", "apple", NA, "anger"),
      b = c("badger", "bang", "boredom", NA)
    )
    expected <- data.frame(
      a = c("apple", NA),
      b = c("bang", "boredom")
    )

    result <- suppressMessages(
      drop_rows_with_values(dat, c("b", "a"), c("bad", "ang"))
    )

    expect_equal(result, expected)

  })
