context("match_sheet_to_regex")

test_that("match_sheet_to_regex gives type errors for sheet_name",{
  expect_error(match_sheet_to_regex(list(c("Sheet1", "Sheet2")), "2"),
               "Sheet names should be a vector of character strings", fixed = TRUE)
  expect_error(match_sheet_to_regex(list("Sheet1", "Sheet2"), "2"),
               "Sheet names should be a vector of character strings", fixed = TRUE)
  expect_error(match_sheet_to_regex(data.frame("Sheets" = c("Sheet1", "Sheet2")), "2"),
               "Sheet names should be a vector of character strings", fixed = TRUE)
  expect_error(match_sheet_to_regex(c(1, 2), "2"),
               "Sheet names should be a vector of character strings", fixed = TRUE)
})

test_that("match_sheet_to_regex gives type errors for regex",{
  expect_error(match_sheet_to_regex(c("Sheet1", "Sheet2"), list(c("Sheet", "2"))),
               "pattern should be a character string", fixed = TRUE)
  expect_error(match_sheet_to_regex(c("Sheet1", "Sheet2"), list("Sheet", "2")),
               "pattern should be a character string", fixed = TRUE)
  expect_error(match_sheet_to_regex(c("Sheet1", "Sheet2"), data.frame("pattern" = c("Sheet", "2"))),
               "pattern should be a character string", fixed = TRUE)
  expect_error(match_sheet_to_regex(c("Sheet1", "Sheet2"), 2),
               "pattern should be a character string", fixed = TRUE)
})

test_that("match_sheet_to_regex returns expected output", {
  expect_equal(match_sheet_to_regex("RA LA Data", "RA"), 
               list("RA LA Data"))
  expect_equal(match_sheet_to_regex("This is RA LA Data", "RA"), 
               list("This is RA LA Data"))
  
  expect_equal(match_sheet_to_regex("RA LA Data 2020", ".*(RA.*LA.*Data).*"), 
               list("RA LA Data 2020"))
  expect_equal(match_sheet_to_regex("RA_LA_Data_2020", ".*(RA.*LA.*Data).*"), 
               list("RA_LA_Data_2020"))
  
  # multiple matches
  expect_equal(suppressWarnings(match_sheet_to_regex(c("RA LA Data", "Data"), "RA")), 
               list("RA LA Data"))
  
})

test_that("match_sheet_to_regex gives warning when there is more than one match",{
  expect_warning(match_sheet_to_regex(c("RA LA Data", "Not RA LA Data"), "RA"), 
                 "Multiple Sheets have matched preprocessing regex, only tab: RA LA Data, will be preprocessed. Please alert a developer if this is an incorrect tab.")
})
