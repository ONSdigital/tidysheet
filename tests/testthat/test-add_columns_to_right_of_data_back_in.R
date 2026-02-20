test_that("add_columns_to_right_of_data_back_in works when there is a single header row", {

  whole <- data.frame(
    sheet = "one header test data",
    row = c(15, 15, 15, 16, 16, 16),
    col = c(1, 2, 3, 1, 2, 3),
    character = c("date", "education", "Notes", "2020 Q1", NA, "note 1"),
    numeric = c(NA, NA, NA, NA, 100, NA)
  )
  left_and_right <- data.frame(
    sheet = "one header test data",
    row = 16,
    col = c(1, 3),
    date = c("2020 Q1", NA),
    description = c("education", NA),
    numeric = c(100, NA),
    character = c(NA, "note 1")
  )

  expected <- data.frame(sheet = "one header test data",
                         row = 16,
                         Notes = "note 1",
                         col = 1,
                         date = "2020 Q1",
                         description = "education",
                         numeric = 100,
                         character = as.character(NA))
  result <- suppressMessages(
    add_columns_to_right_of_data_back_in(whole, left_and_right, 3, 15, 1)
  )
  expect_equal(result, expected)

})

test_that("add_columns_to_right_of_data_back_in works when notes is not fully populated", {

  #  date    | education | notes  |
  # ---------|-----------|--------|
  #  2020 Q1 | 100       | note 1 |
  #  2020 Q1 | 200       |        |

  whole <- data.frame(
    sheet = "notes is not fully populated",
    row = c(rep(15:16, each = 3), 17, 17),
    col = c(rep(1:3, times = 2), 1, 2),
    character = c("date", "education", "Notes",
                  "2020 Q1", NA, "note 1",
                  "2020 Q1", NA),
    numeric = c(NA, NA, NA,
                NA, 100, NA,
                NA, 200)
  )
  left_and_right <- data.frame(
    sheet = "notes is not fully populated",
    row = c(16, 16, 17),
    col = c(1, 3, 1),
    date = c("2020 Q1", NA, "2020 Q1"),
    description = c("education", NA, "education"),
    numeric = c(100, NA, 200),
    character = c(NA, "note 1", NA)
  )

  expected <- data.frame(sheet = "notes is not fully populated",
                         row = c(16, 17),
                         Notes = c("note 1", NA),
                         col = 1,
                         date = "2020 Q1",
                         description = "education",
                         numeric = c(100, 200),
                         character = as.character(NA))

  result <- suppressMessages(
    add_columns_to_right_of_data_back_in(whole, left_and_right, 3, 15, 1)
  )
  expect_equal(result, expected)

})

test_that("add_columns_to_right_of_data_back_in works when there are 2 header rows", {

  whole <- data.frame(
    sheet = "two header test data",
    row = c(14, 14, 14, 15, 15, 15, 16, 16, 16),
    col = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    character = c(NA, "primary", "relevant",
                  "date",  "education",  "Notes",
                  "2020 Q1", NA, "note 1"),
    numeric = c(NA, NA, NA, NA, NA, NA, NA, 100, NA)
  )
  left_and_right <- data.frame(
    sheet = "two header test data",
    row = 16,
    col = c(1, 3),
    date = c("2020 Q1", NA),
    description_1 = c("primary", NA),
    description_2 = c("education", NA),
    numeric = c(100, NA),
    character = c(NA, "note 1")
  )

  expected <- data.frame(sheet = "two header test data",
                         row = 16,
                         relevant_Notes = "note 1",
                         col = 1,
                         date = "2020 Q1",
                         description_1 = "primary",
                         description_2 = "education",
                         numeric = 100,
                         character = as.character(NA))

  expect_warning(
    result <- suppressMessages(
      add_columns_to_right_of_data_back_in(whole, left_and_right,
                                           col_location = 3,
                                           first_header_row = 14,
                                           header_row_count = 2)
    ),
    "'relevant_Notes'"
  )
  expect_equal(result, expected)

})

test_that("add_columns_to_right_of_data_back_in works when the column is not the last", {

  #  date    | education | notes  | other |
  # ---------|-----------|--------|-------|
  #  2020 Q1 | 100       | note 1 |  21   |

  whole <- data.frame(
    sheet = "one header test data",
    row = rep(15:16, each = 4),
    col = rep(1:4, times = 2),
    character = c("date", "education", "Notes", "other",
                  "2020 Q1", NA, "note 1", NA),
    numeric = c(NA, NA, NA, NA,
                NA, 100, NA, 21) #,
    # is_blank = FALSE,
    # data_type = c(rep("character", 5), "numeric", "character", "numeric"),
    # address = c("A15", "B15", "C15", "D15", "A16", "B16", "C16", "D16")
  )
  left_and_right <- data.frame(
    sheet = "one header test data",
    row = 16,
    col = 2:4,
    character = c(NA, "note 1", NA),
    numeric = c(100, NA, 21),
    date = "2020 Q1",
    description = c("education", NA, "other")
  )

  expected <- data.frame(sheet = "one header test data",
                         row = 16,
                         Notes = "note 1",
                         col = c(2, 4),
                         character = as.character(NA),
                         numeric = c(100, 21),
                         date = "2020 Q1",
                         description = c("education", "other")
  )

  result <- suppressMessages(
    add_columns_to_right_of_data_back_in(whole, left_and_right, 3, 15, 1)
  )
  expect_equal(result, expected)

})


test_that("add_columns_to_right_of_data_back_in returns left_and_right if col_location is NA", {
  whole <- data.frame(
    sheet = "one header test data",
    row = rep(15:16, each = 2),
    col = rep(1:2, times = 2),
    character = c("date", "education",
                  "2020 Q1", NA),
    numeric = c(NA, NA,
                NA, 100)
  )
  left_and_right <- data.frame(
    sheet = "one header test data",
    row = 16,
    col = 2,
    character = NA,
    numeric = 100,
    date = "2020 Q1",
    description = "education"
  )
  result <- suppressMessages(
    add_columns_to_right_of_data_back_in(whole, left_and_right, NA, 15, 1)
  )
  expect_equal(result, left_and_right)

})


test_that("add_columns_to_right_of_data_back_in raises an error if there are multiple col_locations provided", {
  whole <- data.frame(
    sheet = "one header test data",
    row = rep(15:16, each = 2),
    col = rep(1:2, times = 2),
    character = c("date", "education",
                  "2020 Q1", NA),
    numeric = c(NA, NA,
                NA, 100)
  )
  left_and_right <- data.frame(
    sheet = "one header test data",
    row = 16,
    col = 2,
    character = NA,
    numeric = 100,
    date = "2020 Q1",
    description = "education"
  )
  expect_error(suppressMessages(
    add_columns_to_right_of_data_back_in(whole, left_and_right, c(1, 2), 15, 1)
  ),
  "More than one column location has been given")

})
