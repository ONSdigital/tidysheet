test_that("make_year_patterns produces the expected output", {
  expect_equal(colnames(make_year_patterns()), c("annual", "financial"))
  
  expect_equal(nrow(make_year_patterns()), 1)
  
  expect_equal(make_year_patterns()$annual, 
               "(?<![0-9A-Za-z])(19|20)\\d{2}(?![0-9A-Za-z])")
  expect_equal(make_year_patterns()$financial, 
               "(?<![0-9A-Za-z])(19|20)\\d{2}(-|_|\\/|(\\b\\sto\\s\\b)|(to))\\d{2}(?![0-9A-Za-z])")
  
  expect_equal(class(make_year_patterns()), "data.frame")
  
  expect_equal(typeof(make_year_patterns()$annual), "character")
  expect_equal(typeof(make_year_patterns()$financial), "character")
}) 
