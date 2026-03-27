# =====================
# Functionality tests
# =====================

test_that("select_data_columns selects present columns and warns on missing", {
  dat <- tibble(a = 1:3, b = 4:6, c = 7:9)
  expect_warning(
    result <- select_data_columns(dat, c("a", "c", "d")),
    regexp = "The following columns were not found and will be dropped: d"
  )
  expect_equal(names(result), c("a", "c"))
})

test_that("select_data_columns works if all columns present", {
  dat <- tibble(a = 1:3, b = 4:6)
  result <- select_data_columns(dat, c("a", "b"))
  expect_equal(names(result), c("a", "b"))
})

# =====================
# Edge case tests
# =====================

test_that("select_data_columns handles duplicated column names in input", {
  dat <- tibble(a = 1:3, b = 4:6)
  dat$dup <- dat$a
  names(dat)[3] <- "a"  # duplicate column name
  expect_warning(
    result <- select_data_columns(dat, c("a", "b")),
    NA
  )
  expect_equal(names(result), c("a", "b"))
})

test_that("select_data_columns errors with empty input data", {
  dat <- tibble()
  expect_error(
    select_data_columns(dat, c("a", "b")),
    regexp = "None of the specified columns are present in the data."
  )
})

test_that("select_data_columns errors with NA column_names", {
  dat <- tibble(a = 1:3, b = 4:6)
  expect_error(
    select_data_columns(dat, NA),
    regexp = "None of the specified columns are present in the data."
  )
})

test_that("select_data_columns errors with NULL column_names", {
  dat <- tibble(a = 1:3, b = 4:6)
  expect_error(
    select_data_columns(dat, NULL),
    regexp = "None of the specified columns are present in the data."
  )
})

test_that("select_data_columns errors with empty column_names vector", {
  dat <- tibble(a = 1:3, b = 4:6)
  expect_error(
    select_data_columns(dat, character(0)),
    regexp = "None of the specified columns are present in the data."
  )
})

test_that("select_data_columns errors if no columns present", {
  dat <- tibble(a = 1:3, b = 4:6)
  expect_error(
    select_data_columns(dat, c("x", "y")),
    regexp = "None of the specified columns are present in the data."
  )
})
