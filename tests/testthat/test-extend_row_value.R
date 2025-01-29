context("extend_row_value")

test_that("Add information to row from the 'above' row and place the information 'forward'", {
  
  data <- data.frame(
    id = 1:14,
    address = c("A9", "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19", "A20", "A21","A22"),
    character = c("Acquisition of land & existing buildings", "...of which HPA", 
                  "Another character", "...of which expenditure", "More characters", "...of which grants", "...of which HRA",
                  "Even more characters", "...of which receipts", "Some text", "...of which loans", "...of which payment", "total", "payment"),
    row = 9:22,
    col = rep(1, 14)
  )
  result <- extend_row_value(source_data = data, 
                             extend_row_regex = "^\\s*\\.\\.\\.\\s*of\\s*which", 
                             extend_row_order = "forward", 
                             extend_row_with = "above")
  
  expected_data <- data
  expected_data$character <- c("Acquisition of land & existing buildings", 
                               "...of which HPA Acquisition of land & existing buildings",
                               "Another character", 
                               "...of which expenditure Another character", 
                               "More characters", 
                               "...of which grants More characters", 
                               "...of which HRA More characters",
                               "Even more characters", 
                               "...of which receipts Even more characters", 
                               "Some text", 
                               "...of which loans Some text", 
                               "...of which payment Some text", 
                               "total", "payment")
  
  expect_equal(result, expected_data)
  
  #---------
  # pattern found in two columns but you only want to do the 'combine' on the 
  # first e.g.
  #
  #| Fruit           | random column | value |
  #|=================|===============|=======|
  #|Apple            | some info     |  3    |
  #|...of which pips |...of which x  |  1    |
  #|...of which skin |               |  2    |
  #==========================================
  pattern_in_two_cols <- data.frame(
    id = 1:6,
    address = c("A2", "A3", "A4", "B2", "B3", "B4"),
    row = c(2, 3, 4, 2, 3, 4),
    col = c(1, 1, 1, 2, 2, 2),
    character = c("apple", "...of which pips", "...of which skin",
                  "some info", "...of which x", NA)
  )   
  result_pattern_in_two_cols <- suppressWarnings(
    extend_row_value(
      source_data = pattern_in_two_cols, 
      extend_row_regex = "^\\s*\\.\\.\\.\\s*of\\s*which", 
      extend_row_with = "above", 
      extend_row_order = "forward")
  )
  
  expected_pattern_in_two_cols <- pattern_in_two_cols
  expected_pattern_in_two_cols$character <- c(
    "apple", "...of which pips apple", "...of which skin apple", 
    "some info", "...of which x", NA)
  expect_equal(result_pattern_in_two_cols, expected_pattern_in_two_cols)
  
  expect_warning(extend_row_value(
    source_data = pattern_in_two_cols, 
    extend_row_regex = "^\\s*\\.\\.\\.\\s*of\\s*which", 
    extend_row_with = "above", 
    extend_row_order = "forward"))
  
  
})

test_that("Add information to row from the 'below' row and place it in 'reverse' to original value", {
  data <- data.frame(
    id = 1:14,
    address = c("A9", "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19", "A20", "A21","A22"),
    character = c("...of which HPA","Acquisition of land & existing buildings",  
                  "...of which expenditure","Another character", "...of which grants", 
                  "...of which HRA", "More characters", 
                  "...of which receipts","Even more characters",  "...of which loans", 
                  "...of which payment","Some text", "total", "payment"),
    row = 9:22,
    col = rep(1, 14)
  )
  result <- extend_row_value(
    source_data = data, 
    extend_row_regex = "^\\s*\\.\\.\\.\\s*of\\s*which", 
    extend_row_order = "reverse", 
    extend_row_with = "below")
  
  expected_data <- data
  expected_data$character <- c("Acquisition of land & existing buildings ...of which HPA",
                               "Acquisition of land & existing buildings",
                               "Another character ...of which expenditure",
                               "Another character", 
                               "More characters ...of which grants", 
                               "More characters ...of which HRA", 
                               "More characters", 
                               "Even more characters ...of which receipts",
                               "Even more characters",  
                               "Some text ...of which loans", 
                               "Some text ...of which payment",
                               "Some text", "total", "payment")
  
  expect_equal(result, expected_data)
})

test_that("extend_row_value throws expected errors", {
  data <- data.frame(
    id = 1:2,
    address = c("A9", "A10"),
    character = c("...of which HPA",
                  "Acquisition of land & existing buildings"),
    row = 9:10,
    col = 1, 1
  )
  
  expect_error(
    extend_row_value(source_data = data, 
                     extend_row_regex = "^\\s*\\.\\.\\.\\s*of\\s*which",
                     extend_row_order = "forward", 
                     extend_row_with = "sideways"), 
    "'extend_row_with' in sheet_structure in the data dict must be either 'above' or 'below'. Please contact a developer to resolve the issue."
  )
  
  expect_error(
    extend_row_value(source_data = data, 
                     extend_row_regex = "^\\s*\\.\\.\\.\\s*of\\s*which",
                     extend_row_order = "upward", 
                     extend_row_with = "below"), 
    "'extend_row_order' in sheet_structure in the data dict must be either 'forward' or 'reverse'. Please contact a developer to resolve the issue."
  )
  
})


