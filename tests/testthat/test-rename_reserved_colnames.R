context("rename_reserved_colnames")

test_that("rename_reserved_colnames gives expected output", {
  dat <- data.frame("source" = c("Europe", "America"),
                    "group" = c("Fox", "Bear"),
                    "value" = c("red", "black"),
                    "numeric" = 1:2)
  
  two_replacement <- rename_reserved_colnames(dat, c("value", "source"))
  one_replacement <- rename_reserved_colnames(dat, c("value"))
  
  two_expected <- rename(dat, source_1 = source, value_1 = value)
  one_expected <- rename(dat, value_1 = value)
  
  expect_equal(two_replacement, two_expected)
  expect_equal(one_replacement, one_expected)
  
}) 
