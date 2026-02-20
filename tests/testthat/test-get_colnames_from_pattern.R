test_that("get_colnames_from_pattern returns expected output for multiple columns", {

  dat <- tibble(
    Year = "2010",
    Value = 1,
    "Item name" = "foo"
  )

  result <- get_colnames_from_pattern(
    dat, c("year", "item"), c("(?i)year", "(?i)item")
    )
  expect_equal(result, c("year" = "Year", "item" = "Item name"))
})


test_that("get_colnames_from_pattern returns expected output when there is no match", {

  dat <- tibble(
    Year = "2010",
    Value = 1,
    "Item name" = "foo"
  )

  # no match for one column
  expect_warning(
    result <- get_colnames_from_pattern(
      dat, c("year", "item"), c("(?i)year", "description")
      ),
    "No column names match pattern: 'description'"
  )
  expect_equal(result, c("year" = "Year", "item" = NA))
})

test_that("get_colnames_from_pattern returns expected output when there are multiple matches for one column", {

  dat <- tibble(
    Year = "2010",
    Value = 1,
    "Item name" = "foo"
  )

  expect_warning(
    result <- get_colnames_from_pattern(
      dat, c("year", "item"), c("(?i)year", "e")
      ),
    "Multiple column names match pattern"
  )
  expect_equal(result, c("year" = "Year", "item" = NA))

})
