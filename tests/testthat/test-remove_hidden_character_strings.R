test_that("remove_hidden_character_strings gives expected warnings", {
  filepath <- file.path(test_path("testdata"), "hidden_characters.xlsx")

  dat <- tidyxl::xlsx_cells(filepath)

  # For simplicity, only select only the address column for checking result
  # against expected. Address is a unique identifier.
  expected_address <- c("A1", "B1", "A2", "B2", "A3", "A4", "B4")

  expect_warning(suppressMessages(
    result <- remove_hidden_character_strings(dat, filepath, TRUE)
    ),
  "The following cells have been removed.*B3."
  )

  expect_equal(result$address, expected_address)

})

test_that("remove_hidden_character_strings retunrs original data if remove_cells is missing", {

  filepath <- file.path(test_path("testdata"), "hidden_characters.xlsx")
  dat <- tidyxl::xlsx_cells(filepath)

  result_NA <- remove_hidden_character_strings(dat, filepath, NA)
  expect_equal(result_NA, dat)

  result_FALSE <- remove_hidden_character_strings(dat, filepath, FALSE)
  expect_equal(result_FALSE, dat)

})
