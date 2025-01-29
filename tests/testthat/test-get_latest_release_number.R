context("get_latest_release_number")

test_that("get_latest_release_number returns correct vintage name", {
  provisional <- "first release"
  two_provisionals <- c("1st release", "first release")
  final_letter_match <- "second  release"
  final_number_match <- "2nd release"
  final_mix <- c("2nd release", "second  release")
  
  result_provisional <- get_latest_release_number(provisional)
  result_two_provisional <- suppressWarnings(get_latest_release_number(two_provisionals))
  result_final_letter_match <- get_latest_release_number(final_letter_match)
  result_final_number_match <- get_latest_release_number(final_number_match)
  result_final_numbers <- suppressWarnings(get_latest_release_number(final_mix))
  
  expect_equal(result_provisional, "provisional")
  expect_equal(result_two_provisional, "provisional")
  expect_equal(result_final_letter_match, "final")
  expect_equal(result_final_number_match, "final")
  expect_equal(result_final_numbers, "final")
})
