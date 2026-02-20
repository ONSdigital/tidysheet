test_that("remove_unwanted_cols returns expected output when all possible columns exist", {

  dat <- tibble(
    row = 1, col = 1, is_blank = FALSE, data_type = "numeric",
    Year = 2023, Value = 1,
     error = FALSE, logical = NA, date = NA,
    character_formatted = NA, formula = NA, is_array = FALSE,
    formula_ref = NA, formula_group = NA, comment = NA, height = 1, width = 1,
    style_format = 26, local_format_id = 1
  )

  expected <- select(dat, c("Year", "Value"))

  expect_equal(remove_unwanted_cols(dat), expected)

})


test_that("remove_unwanted_cols returns expected output when only some removal columns exist", {

  dat <- tibble(
    Year = c(2023, 2023),
    Value = c(1, 2),
    row = c(1, 1),
    local_format_id = NA
  )

  expected <- select(dat, Year, Value)

  expect_equal(remove_unwanted_cols(dat), expected)

})


test_that("remove_unwanted_cols returns expected output when no columns need to be removed", {

  dat <- tibble(
    Year = c(2023, 2023),
    Value = c(1, 2)
  )

  expect_equal(remove_unwanted_cols(dat), dat)

})
