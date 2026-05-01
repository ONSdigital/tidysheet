test_that("remove_empty_lines removes single blank columns and rows", {
  dat <- tibble(
      address = c("A1", "B1", "A2", "B2"),
      row = rep(1:2, each = 2),
      col = rep(1:2, times = 2),
      is_blank = c(TRUE, TRUE, FALSE, TRUE),
      character = c(NA, NA, "hello", NA),
      data_type = c("blank", "blank", "character", "blank"),
      formula = NA,
      numeric = NA
      )

  expected_row <- filter(dat, row != 1)
  expect_warning(
    suppressMessages(
      result_row <- remove_empty_lines(dat, "row")
    ),
    "1 empty row.*1"
  )
  expect_equal(result_row, expected_row)

  expected_col <- filter(dat, col != 2)
  expect_warning(
    suppressMessages(
      result_col <- remove_empty_lines(dat, "col")
    ),
    "1 empty col.*B"
  )
  expect_equal(result_col, expected_col)

})


test_that("remove_empty_lines removes multiple blank columns and rows", {
  dat <- tibble(
    address = c("A1", "B1", "C1",
                "A2", "B2", "C2",
                "A3", "B3", "C3"),
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    is_blank = c(TRUE, TRUE, FALSE,
                 TRUE, TRUE, TRUE,
                 TRUE, TRUE, TRUE),
    character = c(NA, NA, "hello",
                  rep(NA, 6)),
    data_type = c("blank", "blank", "character",
                  rep("blank", 6)),
    formula = NA,
    numeric = NA
  )

  expected_row <- filter(dat, !row %in% 2:3)
  expect_warning(
    suppressMessages(
      result_row <- remove_empty_lines(dat, "row")
    ),
    "2 empty row.*2, 3"
  )
  expect_equal(result_row, expected_row)

  expected_col <- filter(dat, !col %in% 1:2)
  expect_warning(
    suppressMessages(
      result_col <- remove_empty_lines(dat, "col")
    ),
    "2 empty col.*A, B"
  )
  expect_equal(result_col, expected_col)

})


test_that("remove_empty_lines sees formula resulting in 0 as empty", {
  dat <- tibble(
    address = c("A1"),
    row = 1,
    col = 1,
    is_blank = FALSE,
    character = NA,
    data_type = "numeric",
    formula = "A2+A3",
    numeric = 0
  )

  expected <- filter(dat, row != 1)

  expect_warning(
    suppressMessages(
      result <- remove_empty_lines(dat, "row")
    ),
    "1 empty row.*1"
  )
  expect_equal(result, expected)

})

test_that("remove_empty_lines uses formula_zero_as_blank to determine whether a 0 derived frmo a formaula is seen as empty, with default as TRUE", {
  dat <- tibble(
    address = c("A1", "B1"),
    row = 1,
    col = 1:2,
    is_blank = c(FALSE, TRUE),
    character = NA,
    data_type = c("numeric", "blank"),
    formula = c("A2+A3", NA),
    numeric = c(0, NA)
  )

  expected_false <- filter(dat, col != 2)

  expect_warning(
    suppressMessages(
      result_false <- remove_empty_lines(dat, "col", formula_zero_as_blank=FALSE)
    ),
    "1 empty col.*: B"
  )
  expect_equal(result_false, expected_false)

  expected_true <- filter(dat, ! col %in% 1:2)
  expect_warning(
    suppressMessages(
      result_true <- remove_empty_lines(dat, "col", formula_zero_as_blank=TRUE)
    ),
    "2 empty col.*: A, B"
  )
  expect_equal(result_true, expected_true)

  expect_warning(
    suppressMessages(
      result_default <- remove_empty_lines(dat, "col")
    ),
    "2 empty col.*: A, B"
  )
  expect_equal(result_default, expected_true)

})
