test_that("remove_unwanted_cells does nothing if no variables are supplied", {

  dat <- data.frame(address = "A1")

  expect_equal(remove_unwanted_cells(dat, NA), dat)
  expect_no_message(remove_unwanted_cells(dat, NA))

})


test_that("remove_unwanted_cells raises error for address out of range", {

  dat <- data.frame(address = "A1")

  expect_warning(
    result <- remove_unwanted_cells(dat, "A2"),
    "A2 has not been found in the data, so cannot be removed"
    )

  expect_equal(result, dat)
})


test_that("remove_unwanted_cells gives expected output with multiple cells to remove", {

  dat <- data.frame(
    address = c("A1", "A2", "A3", "A4", "A5", "A6"),
    data_type = c("numeric", "character", "logical", "date", "blank", "logical"),
    numeric = c(1, NA, NA, NA, NA, NA),
    character = c(NA, "word", NA, NA, NA, NA),
    logical = c(NA, NA, TRUE, NA, NA, NA),
    date = c(NA, NA, NA, "21/01/2026", NA, NA),
    blank = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
    )

  warnings <- capture_warnings(
    result <- suppressMessages(
      remove_unwanted_cells(dat, c("A1", "A2", "A3", "A4", "A5"))
    )
  )
  expect_equal(
    warnings[1],
    "Cell A1 containing '1' has been removed."
  )
  expect_equal(
    warnings[2],
    "Cell A2 containing 'word' has been removed."
  )
  expect_equal(
    warnings[3],
    "Cell A3 containing 'TRUE' has been removed."
  )
  expect_equal(
    warnings[4],
    "Cell A4 containing '21/01/2026' has been removed."
  )
  expect_equal(
    warnings[5],
    "Cell A5 containing 'TRUE' has been removed."
  )

  expect_equal(result, filter(dat, address == "A6"))

})
