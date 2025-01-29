
test_that("extract_matches accepts numeric and character input", {
  expect_equal(extract_matches(2, "[0-9]")[[1]], "2")
  expect_equal(extract_matches("2", "[0-9]")[[1]], "2")
})

test_that("extract_matches accepts df, list, vector, and matrix input", {
  
  expect_equal(paste(
    extract_matches(data.frame("a" = 2, "b" = 3), "[0-9]"),
    collapse = ", "),
    "2, 3")
  
  expect_equal(paste(
    extract_matches(as.list(c(2, 3)), "[0-9]"),
    collapse = ", "),
    "2, 3")
  
  expect_equal(paste(
    extract_matches(c(2, 3), "[0-9]"), 
    collapse = ", "),
    "2, 3")
  
  expect_equal(paste(
    extract_matches(matrix(c(2, 3)), "[0-9]"),
    collapse = ", "),
    "2, 3")
})

test_that("extract_matches returns a list", {
  expect_equal(class(extract_matches(2, "[0-9]")), "list")
})
