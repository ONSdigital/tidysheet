test_that("replace_strings returns dat if no replacements are specified", {
  dat <- data.frame(
    vintage = c("Near actuals", "Estimates"),
    numeric = c(1, 2)
  )

  result <- replace_strings(dat, NA, NA, NA)
  expect_equal(result, dat)
})


test_that("replace_strings raises an error if column is specified but not other args", {
  dat <- data.frame(
    vintage = c("Near actuals", "Estimates"),
    numeric = c(1, 2)
  )

  expect_error(
    suppressMessages(replace_strings(dat, "vintage", NA, NA)),
    "Some but not all of the variables used to replace strings are specified"
  )
})


test_that("replace_strings raises an error if from_regex is specified but not all other args", {
  dat <- data.frame(
    vintage = c("Near actuals", "Estimates"),
    numeric = c(1, 2)
  )

  expect_error(
    suppressMessages(replace_strings(dat, "vintage", "vintage", NA)),
    "Some but not all of the variables used to replace strings are specified"
  )
})


test_that("replace_strings raises an error if from_regex is specified but not all other args", {
  dat <- data.frame(
    vintage = c("Near actuals", "Estimates"),
    numeric = c(1, 2)
  )

  expect_error(
    suppressMessages(replace_strings(dat, "vintage", NA, "vintage")),
    "Some but not all of the variables used to replace strings are specified"
  )
})


test_that("replace_strings uses FALSE as default for keep even if specified as NA", {
  dat <- data.frame(
    vintage = c("Near actuals", "Estimates"),
    numeric = c(1, 2)
  )

  expect_equal(
    suppressMessages(replace_strings(dat, "vintage", "vintage", "vintage", NA)),
    dat
  )
})


test_that("replace_strings replaces strings in a simple context", {
  dat <- data.frame(
    vintage = c("Near actuals", "Estimates"),
    numeric = c(1, 2)
  )

  suppressMessages(
    expect_message(
      replace_strings(
        dat, "vintage", c("(?i)actual", "(?i)est"), c("final", "budget")
      ),
      "It will be renamed as"
    )
  )

  expect_equal(
    suppressMessages(
      replace_strings(
        dat, "vintage", c("(?i)actual", "(?i)est"), c("final", "budget")
      )
    ),
    data.frame(
      vintage = c("final", "budget"),
      numeric = c(1, 2),
      vintage_original = c("Near actuals", "Estimates")
    )
  )

})
