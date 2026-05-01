test_that("add_quarter_column returns expected output when calendar year is present", {
  dat <- tibble(
    Quarter = c("2023Q1", "2023.2", "2023 q3", "Q4 2023", "quarter 1 2023")
    )
  expected <- mutate(dat, Q = c("1", "2", "3", "4", "1"))

  expect_equal(
    suppressMessages(
      add_quarter_column(dat, "(?i)quarter", "Q")
    ),
    expected
  )
})


test_that("add_quarter_column returns expected output when financial year is present", {
  dat <- tibble(
    Quarter = c("2023-24Q1", "2023 to 2024 2", "2023-24q3", "Q4 2023-24"),
    Value = 1:4
  )
  expected <- mutate(dat, Q = c("1", "2", "3", "4"))

  expect_equal(
    suppressMessages(
      add_quarter_column(dat, "(?i)quarter", "Q")
    ),
    expected
  )
})


test_that("add_quarter_column returns expected output when quarter is numeric", {
  dat <- tibble(
    Quarter = c(2023.1, 2023.2, 2023.3),
    Value = c(1, 2, 3)
  )

  expected <- dat %>%
    mutate(Q = c("1", "2", "3"))

  expect_equal(
    suppressMessages(
      add_quarter_column(dat, "(?i)quarter", "Q")
    ),
    expected
  )
})


test_that("add_quarter_column does nothing if quarter_from pattern and quaretre_col_name are NA", {

  dat <- tibble(
    Quarter = c("2023 Q1", "2023 Q2", "2023 Q3")
    )

  expect_equal(
    add_quarter_column(dat, NA, NA),
    dat
  )
})


test_that("add_quarter_column throws an error if only quarter_from_pattern is NA", {

  dat <- tibble(
    Quarter = c("2023 Q1", "2023 Q2", "2023 Q3")
  )

  expect_error(
    add_quarter_column(dat, NA, "Q"),
    "no pattern has been provided"
  )
})


test_that("add_quarter_column throws an error if only quarter_col_name is NA", {

  dat <- tibble(
    Quarter = c("2023 Q1", "2023 Q2", "2023 Q3")
  )

  expect_error(
    add_quarter_column(dat, "Quarter", NA),
    "no new name has been provided for the column"
  )
})


test_that("add_quarter_column raises an error if quarter column is not found", {
  dat <-  tibble(
    Quarter = c("2023 Q1", "2023 Q2", "2023 Q3")
  )

  expect_error(
    suppressMessages(suppressWarnings(
      add_quarter_column(dat, "(?i)qr", "Q")
    )),
    "Quarter column not identified"
  )

})


test_that("add_quarter_column gives a warning if quarter column already exists and it is overwritten", {
  dat <-  tibble(
    Quarter = c("2023 Q1", "2023 Q2", "2023 Q3")
  )

  expected <- tibble( Quarter = c("1", "2", "3"))

  expect_warning(
    result <- suppressMessages(
      add_quarter_column(dat, "(?i)quarter", "Quarter")
    ),
    "'Quarter' is already a column in the data. It will be overwritten"
  )

  expect_equal(result, expected)
})



test_that("add_quarter_column gives a warning if multiple quarters are found in
one entry, and only the first is returned", {
  dat <-  tibble(
    Quarter = c("2023 Q1 and Q2", "2023 Q2", "2023 Q3")
  )
  expected <-  dat %>%
    mutate(Q = c("1", "2", "3"))

  expect_warning(
    result <- suppressMessages(
      add_quarter_column(dat, "(?i)q", "Q")
    ),
    "More than one quarter found in 1 entry"
  )

  expect_equal(result, expected)

})


test_that("add_quarter_column gives a warning if no individaul quarters are found in
an entry, and quarter is left blank fro those entries", {
  dat <-  tibble(
    Quarter = c("2023 Q1", "2023 Q2-Q3", "2023")
  )
  expected <-  dat %>%
    mutate(Q = c("1", NA, NA))

  expect_warning(
    result <- suppressMessages(
      add_quarter_column(dat, "(?i)q", "Q")
    ),
    "No individual quarters found in 2 entry"
  )

  expect_equal(result, expected)

})
