  # |      | Education | Notes     |     |
  # |------|-----------|-----------|-----|
  # | Year |  primary  | see front |     |
  # |------|-----------|-----------|-----|
  # | 2020 |   100     | note 1    | x4  | <- UNLABELLED EXTRA COLUMN WILL BE LOST

  dat <- data.frame(
    address = c("A1", "B1", "C1", "D1",
                "A2", "B2", "C2", "D2",
                "A3", "B3", "C3", "D3"),
    row = rep(1:3, each = 4),
    col = rep(1:4, times = 3),
    character = c(NA, "education", "Notes", NA,
                  "Year", "primary", "(see front)", NA,
                  "2020", NA, "note 1", "x4"),
    numeric = c(NA, NA, NA, NA,
                NA, NA, NA, NA,
                NA, 100, NA, NA)
  )


test_that("get_outer_descriptor_col_loc returns NA if there is no name pattern provided", {

  result <- suppressMessages(
    get_outer_descriptor_col_loc(
      dat, header_row_count = 2, first_right_header_col = 2,
      first_header_row = 1, name_regex = NA)
  )
  expect_equal(result, NA)
})


test_that("get_outer_descriptor_col_loc returns NA and a warning if there is no name matching the pattern provided", {

  expect_warning(
    result <- suppressMessages(
      get_outer_descriptor_col_loc(
        dat,
        header_row_count = 2,
        first_right_header_col = 2,
        first_header_row = 1,
        name_regex = "nomatch"
      )),
    "No columns have been found"
  )

  expect_equal(result, NA)
})


test_that("get_outer_descriptor_col_loc returns NA and a warning if there is no name matching the pattern provided near the right of the table", {

  # extend the number of columns
  dat <- mutate(dat, col = ifelse(col == 4, 7, col))

  expect_warning(
    result <- suppressMessages(
    get_outer_descriptor_col_loc(
      dat,
      header_row_count = 2,
      first_right_header_col = 2,
      first_header_row = 1,
      name_regex = "(?i)note"
    )),
    "not found within 2 columns of the last column in the table"
  )
  expect_equal(result, NA)

})


test_that("get_outer_descriptor_col_loc identifies outer col colname if it is within the last 2 columns", {

  # |      | Education | Notes     |     |
  # |------|-----------|-----------|-----|
  # | Year |  primary  | see front |     |
  # |------|-----------|-----------|-----|
  # | 2020 |   100     | note 1    | x4  | <- UNLABELLED EXTRA COLUMN WILL BE LOST


  dat <- data.frame(
    address = c("A1", "B1", "C1", "D1",
                "A2", "B2", "C2", "D2",
                "A3", "B3", "C3", "D3"),
    row = rep(1:3, each = 4),
    col = rep(1:4, times = 3),
    character = c(NA, "education", "Notes", NA,
                  "Year", "primary", "see front", NA,
                  "2020", NA, "note 1", "x4"),
    numeric = c(NA, NA, NA, NA,
                NA, NA, NA, NA,
                NA, 100, NA, NA)
  )

  expect_warning(
    result <- suppressMessages(
      get_outer_descriptor_col_loc(
        dat,
        header_row_count = 2,
        first_right_header_col = 2,
        first_header_row = 1,
        name_regex = "(?i)note"
      )
    ),
    "close to the last column"
  )

  expect_equal(result, 3)

})


test_that("get_outer_descriptor_col_loc identifies outer col colname if it is the furthest col to the right", {
  dat <- data.frame(
    address = c("A1", "B1", "C1",
                "A2", "B2", "C2",
                "A3", "B3", "C3"),
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    character = c(NA, "education", "Notes",
                  "Year", "primary", "see front",
                  "2020", NA, "note 1"),
    numeric = c(NA, NA, NA,
                NA, NA, NA,
                NA, 100, NA)
  )

  result <- expect_no_warning(
    suppressMessages(
      get_outer_descriptor_col_loc(
        dat,
        header_row_count = 2,
        first_right_header_col = 2,
        first_header_row = 1,
        name_regex = "(?i)note"
      )
    )
  )

  expect_equal(result, 3)
})


test_that("get_outer_descriptor_col_loc only uses the first name_regex and raises a warning if there is more than 1", {
  dat <- data.frame(
    address = c("A1", "B1", "C1",
                "A2", "B2", "C2",
                "A3", "B3", "C3"),
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    character = c(NA, "education", "Notes",
                  "Year", "primary", "see front",
                  "2020", NA, "note 1"),
    numeric = c(NA, NA, NA,
                NA, NA, NA,
                NA, 100, NA)
  )

  expect_warning(
    result <- suppressMessages(
      get_outer_descriptor_col_loc(
        dat,
        header_row_count = 2,
        first_right_header_col = 2,
        first_header_row = 1,
        name_regex = c("(?i)note", "other")
      )
    ),
    "More than one pattern has been given"
  )
  expect_equal(result, 3)

})


test_that("get_outer_descriptor_col_loc gives a warning if the identified column is not the last column", {

  dat <- data.frame(
    address = c("A1", "B1", "C1", "D1",
                "A2", "B2", "C2", "D2"),
    row = rep(1:2, each = 4),
    col = rep(1:4, times = 2),
    character = c(NA, "education", "Notes", "other",
                  "2020", NA, "note 1", "other"),
    numeric = c(NA, NA, NA, NA,
                NA, 100, NA, NA)
  )

  expect_warning(
    result <- suppressMessages(
      get_outer_descriptor_col_loc(
        dat,
        header_row_count = 1,
        first_right_header_col = 2,
        first_header_row = 1,
        name_regex = "(?i)note"
      )
    ),
    "The column that matches this pattern is not the last column"
  )
  expect_equal(result, 3)

})



test_that("get_outer_descriptor_col_loc gives a warning if more than one column
matching the pattern is found to the right of the data, and it returns the location
of the last", {

  dat <- data.frame(
    address = c("A1", "B1", "C1", "D1",
                "A2", "B2", "C2", "D2"),
    row = rep(1:2, each = 4),
    col = rep(1:4, times = 2),
    character = c(NA, "education", "Notes", "notes 2",
                  "2020", NA, "note 1", "other"),
    numeric = c(NA, NA, NA, NA,
                NA, 100, NA, NA)
  )

  expect_warning(
    result <- suppressMessages(
      get_outer_descriptor_col_loc(
        dat,
        header_row_count = 1,
        first_right_header_col = 2,
        first_header_row = 1,
        name_regex = "(?i)note"
      )
    ),
    "Multiple columns have been found to the right of the numeric data"
  )
  expect_equal(result, 4)

})

