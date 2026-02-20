test_that("make_quarter_patterns returned pattern identifies single quarters", {

  dat <- c("Q1", "1", ".1", "quarter 1", "q1", "Q2", "3", "quarter4")
  expected <- list("1", "1", "1", " 1", "1", "2", "3", "4")

  patterns <- make_quarter_patterns()['single']

  expect_equal(str_extract_all(dat, patterns), expected)

})


test_that("make_quarter_patterns 'single' does not identify quarter ranges", {

  dat <- c(
    "1-3", "1 - 3", "q1-Q3", "q1 - Q3", "Q1to3", "Q1   to Q3", "13", "12-3"
    )
  empty <- as.character()
  expected <- list(empty, empty, empty, empty, empty, empty, empty, empty)

  patterns <- make_quarter_patterns()['single']

  expect_equal(str_extract_all(dat, patterns), expected)

})
