# Unit tests
test_that("check_reserved_words identifies reserved words correctly", {
  nesting_dict_with_reserved <- list(list("date", "random", list("sheet", "other")), list("address", "row"))
  nesting_dict_without_reserved <- list(list("random1", "random2", list("random3", "other")), list("random4", "random5"))
  
  expect_error(check_reserved_words(nesting_dict_with_reserved), "Reserved word used in dict, stopping")
  expect_true(check_reserved_words(nesting_dict_without_reserved))
})

test_that("check_reserved_words handles empty list", {
  empty_nesting_dict <- list()
  
  expect_true(check_reserved_words(empty_nesting_dict))
})