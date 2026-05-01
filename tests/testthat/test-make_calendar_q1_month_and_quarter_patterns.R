test_that("make_calendar_q1_month_and_quarter_patterns returns expected output for quarters", {

  quarter_patterns <- make_calendar_q1_month_and_quarter_patterns("quarter")
  quarter_patterns_q1_apr_to_jun <- make_calendar_q1_month_and_quarter_patterns(
    "quarter", q1_jan_to_mar = FALSE
    )

  expect_equal(quarter_patterns, "((?i)q1(?!-)|1(?!-))|((?i)jan(?s).*\\w)")
  expect_equal(quarter_patterns_q1_apr_to_jun,
               "((?<!-)(?i)q4|4)|((?i)jan(?s).*\\w)"
               )
})

test_that("make_calendar_q1_month_and_quarter_patterns returns expected output for months", {

  month_patterns <- make_calendar_q1_month_and_quarter_patterns("month")
  month_patterns_q1_apr_to_jun <- make_calendar_q1_month_and_quarter_patterns(
    "month", q1_jan_to_mar = FALSE
    )

  expect_equal(month_patterns, "((?i)jan|feb|mar)")
  expect_equal(month_patterns_q1_apr_to_jun, "((?i)jan|feb|mar)")

})


test_that("make_calendar_q1_month_and_quarter_patterns returns expected output for all", {

  pattern <- make_calendar_q1_month_and_quarter_patterns()
  expect_equal(
    pattern, "((?i)q1(?!-)|1(?!-))|((?i)jan(?s).*\\w)|((?i)jan|feb|mar)"
    )
})


test_that("make_calendar_q1_month_and_quarter_patterns creates patterns that match the strings we want them to match", {

  quarter_patterns <- make_calendar_q1_month_and_quarter_patterns("quarter")
  month_patterns <- make_calendar_q1_month_and_quarter_patterns("month")

  quarter_strings <- c("q1", "1",
                       "jan - mar", "jan to mar",
                       "jan - apr",  "jan to apr")
  month_strings <- c("jan", "feb", "mar", "january")

  no_match_to_month_strings <- c("not a match:", "q1")
  no_match_to_quarter_strings <- c("not a match:", "feb", "Q1-Q3")
  mixed_matches_and_mismatches <- c(month_strings, quarter_strings)

  expect_equal(str_detect(quarter_strings, quarter_patterns), rep(TRUE, 6))
  expect_equal(str_detect(month_strings, month_patterns), rep(TRUE, 4))

  expect_equal(str_detect(no_match_to_month_strings, month_patterns), c(FALSE, FALSE))
  expect_equal(str_detect(no_match_to_quarter_strings, quarter_patterns), c(FALSE, FALSE, FALSE))

  expect_equal(
    str_detect(mixed_matches_and_mismatches, quarter_patterns),
    c(rep(FALSE, 3), rep(TRUE, 7))
  )
  expect_equal(
    str_detect(mixed_matches_and_mismatches, month_patterns),
    c(rep(TRUE, 4), FALSE, FALSE, rep(TRUE, 4))
  )

})
