test_that("refine_blanks does nothing if there are no cells with only spaces.", {

  dat <- data.frame(
      data_type = c("character",  "numeric", "blank"),
      is_blank = c(FALSE, FALSE, TRUE),
      character = c("Final", NA, NA),
      numeric = c(NA, 1, NA),
      address = c("A1", "A3", "A4"),
      row = 1,
      col = c(1, 2, 4)
      )

  result <- suppressMessages(refine_blanks(dat))

  expect_equal(result, dat)

})


test_that("refine_blanks only changes space-only cells to blanks.", {

  dat <- data.frame(
      data_type = c(
        "character", "character", "numeric", "blank", "character"
        ),
      is_blank = c(FALSE, FALSE, FALSE, TRUE, FALSE),
      character = c("Final", "    ", NA, NA, " "),
      numeric = c(NA, NA, 1, NA, NA),
      address = c("A1", "A2", "A3", "A4", "A5"),
      row = 1,
      col = 1:5
      )

  expected <- data.frame(
    data_type = c(
      "character", "blank", "numeric", "blank", "blank"
    ),
    is_blank = c(FALSE, TRUE, FALSE, TRUE, TRUE),
    character = c("Final", NA, NA, NA, NA),
    numeric = c(NA, NA, 1, NA, NA),
    address = c("A1", "A2", "A3", "A4", "A5"),
    row = 1,
    col = 1:5
  )

  result <- suppressMessages(refine_blanks(dat))

  expect_equal(result, expected)

  messages <- capture_messages(refine_blanks(dat))

  expect_true(grepl("2 cell", messages[2]))
  expect_true(grepl("1 row", messages[2]))
  expect_true(grepl("2 column", messages[2]))

})


test_that("refine_blanks raises an error if the required columns are not present", {

  dat <- data.frame(vintage = "final", year = "2023", value = 20)
  expect_error(
    suppressMessages(refine_blanks(dat)),
    "dat must be a tidyxl::xlsx_cells dataframe with the columns.*"
  )
})
