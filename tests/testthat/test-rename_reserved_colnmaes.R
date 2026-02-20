test_that("rename_reserved_colnames gives expected output", {
  dat <- data.frame("source" = c("Europe", "America"),
                    "group" = c("Fox", "Bear"),
                    "value" = c("red", "black"),
                    "numeric" = 1:2)

  expected_one_renamed <- rename(dat, value_1 = value)
  expect_warning(
    result_one_renamed <- suppressMessages(
      rename_reserved_colnames(dat, c("value"))
    ),
    "Existing 'value' column renamed as 'value_1'"
  )
  expect_equal(result_one_renamed, expected_one_renamed)

  expected_two_renamed <- rename(dat, source_1 = source, value_1 = value)


  result_two_renamed <- suppressWarnings(suppressMessages(
    rename_reserved_colnames(dat, c("value", "source"))
  ))
  expect_equal(result_two_renamed, expected_two_renamed)

})
