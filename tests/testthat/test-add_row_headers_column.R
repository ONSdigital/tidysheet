xlsx_cells_names <- c(
  "address", "row", "col", "is_blank", "data_type", "numeric"
)

dat <- data.frame(
  address = c('B3', 'B4', 'B5', 'B6', 'B7'),
  row = c(3:7),
  col = 2,
  is_blank = c(TRUE, FALSE, FALSE, TRUE, FALSE),
  data_type = c('blank', rep('numeric', 2), 'blank', 'numeric'),
  numeric = c(NA, 1, 3, NA, 5),
  service = c('education', 'primary',  'secondary', 'Transport', 'roads')
)

#    service    numeric
# ------------ ---------
# Education
#    primary    1
#    secondary  3
# Transport
#    roads      5

test_that("add_row_headers_column returns dat if all relevant variables are NA", {

  expected <- dat %>%
    rename(description_2 = service) %>%
    mutate(description_1 = c(rep("education", 3), rep("Transport", 2)))

  expect_equal(
    add_row_headers_column(dat, xlsx_cells_names, NA, NA, NA, NA, NA),
    dat
  )
})


test_that("add_row_headers_column works in a downward direction by default", {

  expected <- dat %>%
    rename(description_2 = service) %>%
    mutate(description_1 = c(rep("education", 3), rep("Transport", 2)))

  result <- suppressMessages(
    add_row_headers_column(
      dat = dat, xlsx_cells_names = xlsx_cells_names,
      headers_col = 'description_1', nested_col = 'description_2',
      from_pattern = '(?i)service', fill_dir = NA, NA)
  )

  result_down_specified <- suppressMessages(
    add_row_headers_column(
      dat = dat, xlsx_cells_names = xlsx_cells_names,
      headers_col = 'description_1', nested_col = 'description_2',
      from_pattern = '(?i)service', fill_dir = 'down', NA)
  )

  expect_equal(result, expected)
  expect_equal(result_down_specified, expected)
})


test_that("add_row_headers_column can handle the situation when the from column
is the same as the headers_col and nested_col does not yet exist", {

  expected <- dat %>%
    mutate(description_2 = service,
           service = c(rep("education", 3), rep("Transport", 2)))

  result <- suppressMessages(
    add_row_headers_column(
      dat = dat, xlsx_cells_names = xlsx_cells_names,
      headers_col = 'service', nested_col = 'description_2',
      from_pattern = '(?i)service', NA, NA)
  )

  expect_equal(result, expected)
})


test_that("add_row_headers_column raises an error if group_row_na_identifier is
neither 'all' nor an existing column", {

  expect_error(
    suppressMessages(
      add_row_headers_column(
        dat = dat, xlsx_cells_names = xlsx_cells_names,
        headers_col = 'service', nested_col = 'description_2',
        from_pattern = '(?i)service', NA, "description_3")
    ),
    "group_row_na_identifier must be either 'all' or"
  )
})



test_that("add_row_headers_column works as expected when direction is 'up'", {

  expected <- dat %>%
    rename(description_2 = service) %>%
    mutate(description_1 = c("education", rep("Transport", 3), NA))

  result <- suppressMessages(
    add_row_headers_column(
      dat = dat, xlsx_cells_names = xlsx_cells_names,
      headers_col = 'description_1', nested_col = 'description_2',
      from_pattern = '(?i)service', fill_dir = "up", NA)
  )

  expect_equal(result, expected)
})


test_that("add_row_headers_column throws a warning when from_pattern isn't matched by any column names, and uses the first left header by default", {

  expected <- dat %>%
    rename(description_2 = service) %>%
    mutate(description_1 = c(rep("education", 3), rep("Transport", 2)))

  expect_warning(
    result <- suppressMessages(
      add_row_headers_column(
        dat, xlsx_cells_names, 'description_1', 'description_2', '(?i)subervice',
        NA, NA
      )
    ),
    "The first left header column will be used by default: 'service'"
  )

  expect_equal(result, expected)
})


test_that("add_row_headers_column throws an error if some but not all vars are supplied", {

  expect_error(
    add_row_headers_column(
      dat, xlsx_cells_names, NA, 'description_2', '(?i)subervice',
      NA, NA
    ),
    "all others must also be supplied"
  )

  expect_error(
    add_row_headers_column(
      dat, xlsx_cells_names, 'description_1', NA, '(?i)subervice',
      NA, NA
    ),
    "all others must also be supplied"
  )

  expect_error(
    add_row_headers_column(
      dat, xlsx_cells_names, 'description_1', 'description_2', NA,
      NA, NA
    ),
    "all others must also be supplied"
  )

  expect_error(
    add_row_headers_column(
      dat, xlsx_cells_names, NA, 'description_2', NA,
      NA, NA
    ),
    "all others must also be supplied"
  )

})


test_that("add_row_headers_column can identify row headers using blanks in a specific column", {

  #    service    numeric  note1 note2
  # ------------ --------- ----  ----
  # Education                     b
  #    primary    1         a     b
  #    secondary  3         a     b
  # Transport               a
  #    roads      5         a     b

  newdat <-  dat %>%
    mutate(note1 = c(NA, "a", "a", "a", "a"),
           note2 = c("b", "b", "b", NA, "b"),
           is_blank = FALSE)

  expected_note1 <- newdat %>%
    rename(description_2 = service) %>%
    mutate(description_1 = "education")

  expected_note2 <- newdat %>%
    rename(description_2 = service) %>%
    mutate(description_1 = c(NA, NA, NA, rep("Transport", 2)))

  result_note1 <- suppressMessages(
    add_row_headers_column(
      newdat, xlsx_cells_names, 'description_1', 'description_2', '(?i)service',
      "down", "note1"
    )
  )

  result_note2 <- suppressMessages(
    add_row_headers_column(
      newdat, xlsx_cells_names, 'description_1', 'description_2', '(?i)service',
      "down", "note2"
    )
  )

  expect_equal(result_note1, expected_note1)
  expect_equal(result_note2, expected_note2)
})


test_that("add_row_headers_column uses all columns being blank as the default to id header rows", {

  #    service    numeric  note1 note2
  # ------------ --------- ----  ----
  # Education
  #    primary    1         a     b
  #    secondary  3         a     b
  # Transport               a
  #    roads      5         a     b

  newdat <-  dat %>%
    mutate(note1 = c(NA, "a", "a", "a", "a"),
           note2 = c(NA, "b", "b", NA, "b"),
           is_blank = c(TRUE, rep(FALSE, 4)))

  expected <- newdat %>%
    rename(description_2 = service) %>%
    mutate(description_1 = "education")


  result <- suppressMessages(
    add_row_headers_column(
      newdat, xlsx_cells_names, 'description_1', 'description_2', '(?i)service',
      "down", NA
    )
  )

  expect_equal(result, expected)
})
