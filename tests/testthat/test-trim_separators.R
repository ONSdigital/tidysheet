context("trim_separators")

test_that("trim_separators returns expected output", {
  expect_equal(trim_separators(" trailing spaces ", c(":", "-")), "trailing spaces")
  expect_equal(trim_separators("central   spaces",  c(":", "-")), "central   spaces")
  expect_equal(trim_separators("end hyphen-",  c(":", "-")), "end hyphen")
  expect_equal(trim_separators(":start colon",  c(":", "-")), "start colon")
  expect_equal(trim_separators("- start hyphen then space",  c(":", "-")), "start hyphen then space")
  expect_equal(trim_separators("-both ends hyphen-",  c(":", "-")), "both ends hyphen")
  expect_equal(trim_separators("central - hyphen",  c(":", "-")), "central - hyphen")
  expect_equal(trim_separators(":- hyphens and colons:",  c(":", "-")), "hyphens and colons")
}) 
