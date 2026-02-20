test_that("unpivot_data uses reformat_tidy_data if tidy_data is TRUE", {

  dat <- data.frame(
    sheet = "test",
    row = rep(1:2, each = 2),
    col = rep (1:2, times = 2),
    is_blank = FALSE,
    address = c("A1", "B1", "A2", "B2"),
    data_type = c(rep("character", 3), "numeric"),
    character = c("item", "value", "A", NA),
    numeric = c(NA, NA, NA, 1)
  )

  expected <- tibble(
    row = 2, is_blank = FALSE, sheet = "test", item = "A", numeric = 1
    )

  result <- suppressMessages(
    dat %>%
      unpivot_data(
        left_columns_to_remove = NA,
        columns_to_create = NA,
        first_header_row = 1,
        tolerance = 0.4,
        left_headers = NA,
        header_to_split = NA,
        header_split_to = NA,
        split_points = NA,
        column_to_right_of_data_name_pattern = NA,
        tidy_data = TRUE,
        tidy_notes_name = NA
        )
  )

  expect_equal(result, expected)

  })


test_that("unpivot_data raises an error if header to split is specified, but not in columns_to_create", {
  dat <- data.frame(
    sheet = "test",
    row = c(rep(1:2, each = 3)),
    col = c(rep(1:3, times = 2)),
    data_type = c(rep("character", 4),
                  rep("numeric", 2)),
    character = c("group", "a1", "a2",
                  "C", NA, NA),
    numeric = c(rep(NA, 4), 1, 2),
    is_blank = FALSE
  ) %>%
    mutate(address = paste0(LETTERS[col], row))

  expect_error(
    dat %>%
      unpivot_data(
        left_columns_to_remove = NA,
        columns_to_create = "description",
        first_header_row = 1,
        tolerance = 0.4,
        left_headers = NA,
        header_to_split = "to_split",
        header_split_to = NA,
        split_points = NA,
        column_to_right_of_data_name_pattern = NA,
        tidy_data = NA,
        tidy_notes_name = NA
      ),
    "header_to_split must be present in the columns_to_create list"
  )
})


test_that("unpivot_data raises an error if no numeric columns are found", {
  dat <- data.frame(
    sheet = "test",
    row = c(rep(1:3, each = 2)),
    col = c(rep(1:2, times = 3)),
    data_type = c(rep("character", 3),
                  "numeric", rep("character", 2)),
    character = c("group", "a1",
                  "C", NA, "D", "s"),
    numeric = c(rep(NA, 3), 1, NA, NA),
    is_blank = FALSE
  ) %>%
    mutate(address = paste0(LETTERS[col], row))

  expect_error(
    suppressMessages(
      dat %>%
        unpivot_data(
          left_columns_to_remove = NA,
          columns_to_create = "description_1",
          first_header_row = 1,
          tolerance = 0.6,
          left_headers = NA,
          header_to_split = NA,
          header_split_to = NA,
          split_points = NA,
          column_to_right_of_data_name_pattern = NA,
          tidy_data = NA,
          tidy_notes_name = NA
        )),
    "No columns that are 60% numeric have been found"

  )
})


test_that("unpivot_data produces expected result with 2 header rows and left headers
in line with the second row of headers", {

  dat <- data.frame(
    sheet = "test",
    row = c(rep(1:3, each = 4)),
    col = c(rep(1:4, times = 3)),
    data_type = c("blank", "character", "blank", "character",
                  rep("character", 4),
                  "character", rep("numeric", 3)),
    character = c(NA, "A", NA, "B",
                  "group", "a1", "a2", "b",
                  "C", NA, NA, NA),
    numeric = c(rep(NA, 9), 1, 2, 3),
    is_blank = c(TRUE, FALSE, TRUE, FALSE,
                 rep(FALSE, 8))
  ) %>%
    mutate(address = paste0(LETTERS[col], row))

  expected <- tibble(
    sheet = "test",
    row = 3,
    col = 2:4,
    data_type = "numeric",
    character = as.character(NA),
    numeric = 1:3,
    is_blank = FALSE) %>%
    mutate(
      address = paste0(LETTERS[col], row),
      group = "C",
      description_1 = c("A", "A", "B"),
      description_2 = c("a1", "a2", "b")
    )

  result <- suppressMessages(
    dat %>%
      unpivot_data(
        left_columns_to_remove = NA,
        columns_to_create = c("description_1", "description_2"),
        first_header_row = 1,
        tolerance = 0.4,
        left_headers = NA,
        header_to_split = NA,
        header_split_to = NA,
        split_points = NA,
        column_to_right_of_data_name_pattern = NA,
        tidy_data = NA,
        tidy_notes_name = NA)
  )

  expect_equal(result, expected)

})


test_that("unpivot_data produces expected result with 2 header rows, one of
which needs splitting, and left headers in line with the second row of headers", {
  dat <- data.frame(
    sheet = "test",
    row = c(rep(1:3, each = 4)),
    col = c(rep(1:4, times = 3)),
    data_type = c("blank", "character", "blank", "character",
                  rep("character", 4),
                  "character", rep("numeric", 3)),
    character = c(NA, "A", NA, "B",
                  "group", "a - sub", "a - total", "b - total",
                  "C", NA, NA, NA),
    numeric = c(rep(NA, 9), 1, 2, 3),
    is_blank = c(TRUE, FALSE, TRUE, FALSE,
                 rep(FALSE, 8))
  ) %>%
    mutate(address = paste0(LETTERS[col], row))

  expected <- tibble(
    sheet = "test",
    row = 3,
    col = 2:4,
    data_type = "numeric",
    character = as.character(NA),
    numeric = 1:3,
    is_blank = FALSE) %>%
    mutate(
      address = paste0(LETTERS[col], row),
      group = "C",
      description_1 = c("A", "A", "B"),
      description_2_and_3 = c("a - sub", "a - total", "b - total"),
      description_2 = c("a", "a", "b"),
      description_3 = c("sub", "total", "total"),
    )

  result <- suppressMessages(
    dat %>%
    unpivot_data(
      left_columns_to_remove = NA,
      columns_to_create = c("description_1", "description_2_and_3"),
      first_header_row = 1,
      tolerance = 0.4,
      left_headers = NA,
      header_to_split = "description_2_and_3",
      header_split_to = c("description_2", "description_3"),
      split_points = "ALT_=-",
      column_to_right_of_data_name_pattern = NA,
      tidy_data = NA,
      tidy_notes_name = NA
      )
  )

  expect_equal(result, expected)
})


test_that("unpivot_data produces expected result with a descriptor column to the
right, 2 header rows and left headers in line with the second row of headers", {

  dat <- data.frame(
    sheet = "test",
    address = c("A1", "B1", "C1", "D1", "E1",
                "A2", "B2", "C2", "D2", "E2",
                "A3", "B3", "C3", "D3", "E3"),
    row = c(rep(1:3, each = 5)),
    col = c(rep(1:5, times = 3)),
    data_type = c("blank", "character", "blank", "character", "blank",
                  rep("character", 5),
                  "character", rep("numeric", 3), "character"),
    character = c(NA, "A", NA, "B", NA,
                  "group", "a1", "a2", "b1", "notes",
                  "C", NA, NA, NA, "note 1"),
    numeric = c(rep(NA, 11), 1, 2, 3, NA),
    is_blank = c(TRUE, FALSE, TRUE, FALSE, TRUE,
                 rep(FALSE, 10))
  ) %>%
    mutate(address = paste0(LETTERS[col], row))

  expected <- data.frame(
    sheet = "test",
    row = 3,
    notes = "note 1",
    address = c("B3", "C3", "D3"),
    col = 2:4,
    data_type = "numeric",
    character = as.character(NA),
    numeric = 1:3,
    is_blank = FALSE) %>%
    mutate(
      address = paste0(LETTERS[col], row),
      group = "C",
      description_1 = c("A", "A", "B"),
      description_2 = c("a1", "a2", "b1")
    )

  result <- suppressMessages(
    dat %>%
      unpivot_data(
        left_columns_to_remove = NA,
        columns_to_create = c("description_1", "description_2"),
        first_header_row = 1,
        tolerance = 0.4,
        left_headers = NA,
        header_to_split = NA,
        header_split_to = NA,
        split_points = NA,
        column_to_right_of_data_name_pattern = "(?i)note",
        tidy_data = NA,
        tidy_notes_name = NA)
  )

  expect_equal(result, expected)
})


test_that("unpivot_data works for data that have a two_row_header layout and
high left headers", {

  dat <- data.frame(
    row = c(rep(1:3, each = 4)),
    col = c(rep(1:4, times = 3)),
    data_type = c("character", "character", "blank", "character",
                  "blank", rep("character", 3),
                  "character", rep("numeric", 3)),
    character = c("group", "A", NA, "B",
                  NA, "a1", "a2", "b",
                  "C", NA, NA, NA),
    numeric = c(rep(NA, 9), 1, 2, 3),
    is_blank = c(FALSE, FALSE, TRUE, FALSE,
                 TRUE, rep(FALSE, 7))
  ) %>%
    mutate(address = paste0(LETTERS[col], row))

  expected <- tibble(
    row = 3,
    col = 2:4,
    data_type = "numeric",
    character = as.character(NA),
    numeric = 1:3,
    is_blank = FALSE) %>%
    mutate(
      address = paste0(LETTERS[col], row),
      group = "C",
      description_1 = c("A", "A", "B"),
      description_2 = c("a1", "a2", "b")
    )

  result <- suppressMessages(
    dat %>%
      unpivot_data(
        left_columns_to_remove = NA,
        columns_to_create = c("description_1", "description_2"),
        first_header_row = 1,
        tolerance = 0.4,
        left_headers = NA,
        header_to_split = NA,
        header_split_to = NA,
        split_points = NA,
        column_to_right_of_data_name_pattern = NA,
        tidy_data = NA,
        tidy_notes_name = NA)
  )

  expect_equal(result, expected)
})


test_that("unpivot_data works for a single header row", {

  dat <- data.frame(
    sheet = "test",
    row = c(rep(1:2, each = 3)),
    col = c(rep(1:3, times = 2)),
    data_type = c(rep("character", 4),
                  rep("numeric", 2)),
    character = c("group", "a1", "a2",
                  "C", NA, NA),
    numeric = c(rep(NA, 4), 1, 2),
    is_blank = FALSE
  ) %>%
    mutate(address = paste0(LETTERS[col], row))

  expected <- tibble(
    sheet = "test",
    row = 2,
    col = 2:3,
    data_type = "numeric",
    character = as.character(NA),
    numeric = 1:2,
    is_blank = FALSE) %>%
    mutate(
      address = paste0(LETTERS[col], row),
      group = "C",
      description_1 = c("a1", "a2")
    )

  result <- suppressMessages(
    dat %>%
      unpivot_data(
        left_columns_to_remove = NA,
        columns_to_create = "description_1",
        first_header_row = 1,
        tolerance = 0.4,
        left_headers = NA,
        header_to_split = NA,
        header_split_to = NA,
        split_points = NA,
        column_to_right_of_data_name_pattern = NA,
        tidy_data = NA,
        tidy_notes_name = NA)
  )

    expect_equal(result, expected)
})


test_that("unpivot_data works for a single row header but with duplicate headers", {

  dat <- data.frame(
    sheet = "test",
    row = c(rep(1:4, each = 3)),
    col = c(rep(1:3, times = 4)),
    data_type = c(rep("character", 4),
                  rep("numeric", 2),
                  rep("character", 4),
                  rep("numeric", 2)),
    character = c("group", "a1", "a2",
                  "C", NA, NA,
                  "group", "a1", "a2",
                  "D", NA, NA),
    numeric = c(rep(NA, 4), 1, 2,
                rep(NA, 4), 3, 4),
    is_blank = FALSE
  ) %>%
    mutate(address = paste0(LETTERS[col], row))

  expected <- tibble(
    sheet = "test",
    row = c(2, 2, 4, 4),
    col = c(2, 3, 2, 3),
    data_type = "numeric",
    character = as.character(NA),
    numeric = 1:4,
    is_blank = FALSE) %>%
    mutate(
      address = paste0(LETTERS[col], row),
      group = c("C", "C", "D", "D"),
      description_1 = rep(c("a1", "a2"), 2)
    )

  result <- suppressMessages(
    dat %>%
      unpivot_data(
        left_columns_to_remove = NA,
        columns_to_create = "description_1",
        first_header_row = 1,
        tolerance = 0.4,
        left_headers = NA,
        header_to_split = NA,
        header_split_to = NA,
        split_points = NA,
        column_to_right_of_data_name_pattern = NA,
        tidy_data = NA,
        tidy_notes_name = NA)
  )

  expect_equal(result, expected)

})


test_that("unpivot_data works when one of the left block columns needs to be removed", {

  dat <- data.frame(
    sheet = "test",
    row = c(rep(1:2, each = 4)),
    col = c(rep(1:4, times = 2)),
    data_type = c(rep("character", 6),
                  rep("numeric", 2)),
    character = c("group", "random", "a1", "a2",
                  "C", "gumpf", NA, NA),
    numeric = c(rep(NA, 6), 1, 2),
    is_blank = FALSE
  ) %>%
    mutate(address = paste0(LETTERS[col], row))

  expected <- tibble(
    sheet = "test",
    row = 2,
    col = c(3, 4),
    data_type = "numeric",
    character = as.character(NA),
    numeric = c(1, 2),
    is_blank = FALSE) %>%
    mutate(
      address = paste0(LETTERS[col], row),
      group = "C",
      description_1 = c("a1", "a2")
    )

  expect_warning(
    result <- suppressMessages(
      dat %>%
        unpivot_data(
          left_columns_to_remove = "random",
          columns_to_create = "description_1",
          first_header_row = 1,
          tolerance = 0.4,
          left_headers = NA,
          header_to_split = NA,
          header_split_to = NA,
          split_points = NA,
          column_to_right_of_data_name_pattern = NA,
          tidy_data = NA,
          tidy_notes_name = NA)
    ),
    "removed because they match the left_column_to_remove pattern.*B"
  )

  expect_equal(result, expected)

})

test_that("unpivot data raises an error if more than one header to split is provided", {

  expect_error(
    unpivot_data(
      data.frame(), NA, "a", 1, 0.4, NA, c("a", "b"), "a", ".", NA, NA, NA
    ),
    "More than one header_to_split"
  )
})
