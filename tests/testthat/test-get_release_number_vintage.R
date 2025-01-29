context("get_release_number_vintage")

# Test case 1: Basic test with a valid input
test_that("get_release_number_vintage returns correct vintage name", {
  provisional <- data.frame("character" = c("release info", "The First release is out"))
  two_provisionals <- data.frame("character" =  c("1st release", "The First release is out"))
  final_one_match <- data.frame("character" = c("__Second  release is here_", "words"))
  final_two_match <- data.frame("character" = c("second release", "first release"))
  final_numbers <- data.frame("character" = c("2nd release", "words"))
  
  result_provisional <- get_release_number_vintage(provisional)
  result_two_provisional <- suppressWarnings(get_release_number_vintage(two_provisionals))
  result_final_one_match <- get_release_number_vintage(final_one_match)
  result_final_two_match <- suppressWarnings(get_release_number_vintage(final_two_match))
  result_final_numbers <- get_release_number_vintage(final_numbers)
  
  expect_equal(result_provisional, "provisional")
  expect_equal(result_two_provisional, "provisional")
  expect_equal(result_final_one_match, "final")
  expect_equal(result_final_two_match, "final")
  expect_equal(result_final_numbers, "final")
})


test_that("get_release_number_vintage returns expected warnings", {
  pattern_interrupted <- data.frame("character" = "first @release")
  no_pattern <- data.frame("character" = c("Stats release mentioned here", "Another example with no release"))
  
  expect_warning(get_release_number_vintage(pattern_interrupted), 
                 "Release number not found in the front page. Vintage will be assigned using information in the main table")  
  
  expect_warning(get_release_number_vintage(no_pattern), 
                 "Release number not found in the front page. Vintage will be assigned using information in the main table")
  
  expect_equal(suppressWarnings(get_release_number_vintage(pattern_interrupted)), NA)
  expect_equal(suppressWarnings(get_release_number_vintage(no_pattern)), NA)
  
})