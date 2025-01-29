# Test cases for check_dropdown function
test_that("check_dropdown returns expected message when dropdown_regex is found", {
  info_above_table <- data.frame(
    row = c(1:3),
    col = rep(1, 3),
    character = c("Title", "some other info", "England")
  )
  
  expected_message <- "Eng found in information above table, so it is assumed that the dropdown contains the correct selection"
  
  expect_message(
    result <- capture_output(check_dropdown("Eng", info_above_table)),
    expected_message
  )
})

test_that("check_dropdown throws an error when dropdown_regex is not found", {
  info_above_table <- data.frame(
    row = c(1:3),
    col = rep(1, 3),
    character = c("Title", "some other info", "England")
  )
  
  dropdown_regex <- "Wales"
  
  expected_message <- "Wales is the expected dropdown selection (as set in the data dictionary sheet_structure) and has not been found in the information above the table. Please check the correct dropdown has been selected in the interim data. If not, change the selection, save it and re-run pre-processing. If the correct selection is made already, contact a developer"
  
  expect_error(
    expect_message(check_dropdown(dropdown_regex, info_above_table),
                   expected_message)
  )
  
})

test_that("check_dropdown does nothing when dropdown_regex is NA", {
  info_above_table <- data.frame(
    row = c(1:3),
    col = rep(1, 3),
    character = c("Title", "some other info", "England")
  )
  
  dropdown_regex <- NA
  result3 <- check_dropdown(dropdown_regex, info_above_table)
  expect_equal(object = result3, expected = NULL)
})

