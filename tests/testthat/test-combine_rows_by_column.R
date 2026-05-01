test_that("combine_rows_by_column function works as expected", {

  # Sample data for testing looks like this in standard format:

  #  data in 2020     ||         ||         |
  # |      || this    ||         || and     |
  # |      ||---------||---------||---------|
  # |      || is one  || one     || another |
  # |      ||---------||---------||---------|
  # | year || heading || here    ||         |
  # |======||=========||=========||=========|
  # | 2020 ||   1     ||  2      ||   3     |

  # in xlsx_cells format
  dat <- data.frame(
    row = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5),
    col = c(1, 2, 4, 2, 3, 4, 1, 2, 3, 1, 2, 3, 4),
    character = c(
      "data in 2020",
      "this", "and",
      "is one", "one", "another",
      "year", "heading", "here",
      rep(NA,4)
      ),
    numeric = c(
      rep(NA, 9),
      2020, 1:3
      ),
    data_type = c(
      rep("character", 9),
      rep("numeric", 4)
      )
  )

  # expected result in standard format
  # data in 2020                ||          ||             |
  #       ||                    ||          ||             |
  #       ||--------------------||----------||-------------|
  #       ||                    ||          ||             |
  #       ||--------------------||----------||-------------|
  #  year || this is one heading|| one here || and another |
  # ======||====================||==========||=============|
  #  2020 ||        1           ||   2      ||      3      |

  # in xlsx_cells format
  expected_result <- data.frame(
    row = c(1, 4, 4, 4, 4, 5, 5, 5, 5),
    col = c(1, 1, 2, 3, 4, 1, 2, 3, 4),
    character = c(
      "data in 2020",
      "year", "this is one heading", "one here", "and another", rep(NA,4)),
    numeric = c(rep(NA, 5), 2020, 1:3),
    data_type = c(rep("character", 5), rep("numeric", 4))
  )
  # use `rectify(result)` if you want to see the result in standard layout

  expect_true(
    all(colSums(
      sapply(
        c(
          "finding the first location that matches pattern 'this'",
          "1 matches found for pattern 'this'. Returning the first location: 2",
          "finding the first location that matches pattern 'here'.",
          "1 matches found for pattern 'here'. Returning the first location: 4",
          "Rows 2 to 4 will be combined. If this is not the desired behaviour, please contact developers."
          ),
        grepl,
        capture_messages(
          combine_rows_by_column(dat, "this", "here")
        )
      )
    ) == 1)
  )
  result <- suppressMessages(
    combine_rows_by_column(dat, "this", "here")
    )
  expect_equal(result, expected_result)

  #----

  # also test case where year is in row 2 (previously this caused a bug)
  dat_year_in_middle_row <- dat %>%
    mutate(row = ifelse(row == 4 & col == 1, 3, row))

  expected_result_year_in_middle_row <- expected_result %>%
    mutate(row = ifelse(row == 4, 3, row))

  result_year_in_middle_row <- suppressMessages(
      combine_rows_by_column(
        dat_year_in_middle_row, "this", "here"
      )
  )
  expect_true(
    all(colSums(
      sapply(
        c(
          "finding the first location that matches pattern 'this'",
          "1 matches found for pattern 'this'. Returning the first location: 2",
          "finding the first location that matches pattern 'here'.",
          "1 matches found for pattern 'here'. Returning the first location: 4",
          "Rows 2 to 4 will be combined. If this is not the desired behaviour, please contact developers."
        ),
        grepl,
        capture_messages(
          combine_rows_by_column(dat_year_in_middle_row, "this", "here")
        )
      )
    ) == 1)
  )

  expect_equal(result_year_in_middle_row, expected_result_year_in_middle_row)

})


test_that("combine_rows_by_column raises expected errors", {

  dat <- data.frame(
    row = 1:3,
    col = 1,
    character = c("A", "B", NA),
    numeric = c(NA, NA, 200),
    data_type = c(rep("character", 2), "numeric")
  )

  # start row is after end row
  expect_error(suppressMessages(combine_rows_by_column(dat, "B", "A")))
  # start row identifier not found
  expect_error(suppressMessages(suppressWarnings(
    combine_rows_by_column(dat, "wrong", "B")
    )))
  # end_row_identifier not found
  expect_error(suppressMessages(suppressWarnings(
    combine_rows_by_column(dat, "A", "wrong")
    )))
})


test_that("combine_rows_by_column function works correctly on disordered data", {

  dat <- data.frame(
    row = c(2, 1, 3),
    col = 1,
    character = c("B", "A", NA),
    numeric = c(NA, NA, 200),
    data_type = c(rep("character", 2), "numeric")
  )

  expected <- data.frame(
    row = c(1, 3),
    col = 1,
    character = c("A B", NA),
    numeric = c(NA, 200),
    data_type = c("character", "numeric")
  )

  expect_true(
    all(colSums(
      sapply(
        c(
          "finding the first location that matches pattern 'A'.",
          "1 matches found for pattern 'A'. Returning the first location: 1",
          "finding the first location that matches pattern 'B'.",
          "1 matches found for pattern 'B'. Returning the first location: 2",
          "Rows 1 to 2 will be combined. If this is not the desired behaviour, please contact developers."
        ),
        grepl,
        capture_messages(
          combine_rows_by_column(dat, "A", "B")
        )
      )
    ) == 1)
  )

  result <- suppressMessages(combine_rows_by_column(dat, "A", "B"))

  expect_equal(result, expected)
})


test_that("combine_rows_by_column returns dat when no relevant variables are specified", {
  dat <- data.frame(address = "A1")
  expect_no_message(result <- combine_rows_by_column(dat, NA, NA))
  expect_equal(result, dat)
})


test_that("combine_rows_by_column throws an error if one but not both of the
relevant variables are specified", {
  dat <- data.frame(address = "A1")
  expect_error(
    combine_rows_by_column(dat, "A", NA),
    "combine_end_row_identifier is missing from the settings"
    )
  expect_error(
    combine_rows_by_column(dat, NA, "A"),
    "combine_start_row_identifier is missing from the settings"
  )
})
