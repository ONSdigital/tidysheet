dat <- data.frame(
  data_type = "character",
  numeric = 1,
  Set = "B",
  Type = "A"
)

test_that("rename_columns returns dat if patterns and new_names are not provided", {

  result <- suppressMessages(
    rename_columns(
      dat, exclude_names = c("data_type", "numeric"),
      patterns = NA,
      new_names = NA
    )
  )
  expect_equal(result, dat)
})


test_that("rename_columns raises an error if one but not both of patterns and new_names are provided", {

  expect_error(
    suppressMessages(
      rename_columns(
        dat, exclude_names = c("data_type", "numeric"),
        patterns = c("(?i)type", "Set"),
        new_names = NA
      )
    ),
    "If one is given, the others must also be supplied"
    )

  expect_error(
    suppressMessages(
      rename_columns(
        dat, exclude_names = c("data_type", "numeric"),
        patterns = NA,
        new_names = c("description_1", "description_2")
      )
    ),
    "If one is given, the others must also be supplied"
  )
})


test_that("rename_columns is able to rename multiple specified columns", {

  expected <- dat %>%
    rename(description_1 = Type, description_2 = Set)

  result <- suppressMessages(
    rename_columns(
    dat, exclude_names = c("data_type", "numeric"),
    patterns = c("(?i)type", "Set"),
    new_names = c("description_1", "description_2")
    )
  )
  expect_equal(result, expected)
})


test_that("rename_columns does not look for pattern matches in excluded columns", {

  expected <- rename(dat, description_1 = Type)

  result <- suppressMessages(
    rename_columns(
      dat, exclude_names = c("data_type", "numeric"),
      patterns = "(?i)type",
      new_names = "description_1"
    )
  )

  expect_equal(result, expected)
})


test_that("rename_columns raises a warning if multiple names match the pattern", {

  expect_warning(
    result <- suppressMessages(
      rename_columns(
        dat, exclude_names = "numeric",
        patterns = "(?i)type",
        new_names = "description_1"
      )
    ),
    "More than one column was matched to the pattern"
  )

  expect_equal(result, dat)
})


test_that("rename_columns raises a warning if no names match the pattern", {

  expect_warning(
    result <- suppressMessages(
      rename_columns(
        dat, exclude_names = "numeric",
        patterns = "nomatch",
        new_names = "description_1"
      )
    ),
    "No column names were matched to the pattern"
  )

  expect_equal(result, dat)
})


test_that("rename_columns includes all columns in possibilities if exlude_names is NA", {

  dat <- data.frame(
    data_type = "character",
    numeric = 1,
    Set = "B"
  )
  # data_type would usually be in excluded names
  expected <- rename(dat, description_1 = data_type)

  result <- suppressMessages(
    rename_columns(
      dat, exclude_names = NA,
      patterns = "(?i)type",
      new_names = "description_1"
    )
  )

  expect_equal(result, expected)
})
