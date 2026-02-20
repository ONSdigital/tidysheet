dat <- tibble(
  "financial year end" = "2020",
  Year = "2021",
  other = NA
  )

test_that("get_matching_colnames returns all matching column names", {

  expect_equal(
    get_matching_colnames(dat, "(?i)year"),
    c("financial year end", "Year")
    )

  expect_equal(
    get_matching_colnames(dat, "oth"),
    c("other")
  )

})


test_that("get_matching_colnames returns empty character vector with no matches", {

  expect_equal(
    get_matching_colnames(dat, "none"),
    as.character(c())
  )

})

