test_that("find_all_instances works as expected with both rows and cols", {

  dat <- data.frame(
    row = rep(1:3, each = 2),
    col = rep(1:2, times  = 3),
    data_type = "character",
    character = c("header 1", "header 2",
                  "info 1a", "info 2a",
                  "info 1b", "info 2b")
  )

  one_row <- find_all_instances(
      dat, "(?i)HEADER", direction = "row"
      )
  expect_equal(one_row, 1)

  multiple_rows <- find_all_instances(
    dat, "(?i)info\\s*1", direction = "row"
  )
  expect_equal(multiple_rows, c(2, 3))

  one_column <- find_all_instances(
    dat, "2", direction = "col"
  )
  expect_equal(one_column, 2)

  multiple_columns <- find_all_instances(
    dat, "info", direction = "col"
  )
  expect_equal(multiple_columns, c(1, 2))

})

test_that("find_all_instances returns an empty vector if the pattern is not found", {
  dat <- data.frame(
    row = 1,
    col = 1,
    data_type = "character",
    character = "header"
  )


  expect_warning(
    result <- find_all_instances(dat, "missing"),
    "Failed to find the pattern 'missing'"
  )

  expect_equal(result, double())

})


test_that("find_all_instances uses row as default", {
  dat <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    data_type = "character",
    character = c("header 1", "header 2", "info 1", "info 2")
  )

  # if col was being used as default, this would return 'c(1, 2)'
  expect_equal(find_all_instances(dat, "header"), 1)

})


test_that("find_all_instances doesn't look at numeric data cells", {

  dat <- data.frame(
    row = 1:2,
    col = 1,
    data_type = c("character", "numeric"),
    character = c("header 1", NA),
    numeric = c(NA, 1)
  )

  expect_equal(find_all_instances(dat, "1", "row"), 1)
  expect_equal(find_all_instances(dat, 1, "row"), 1)
})


test_that("find_all_instances doesn't look at numeric data cells", {

  dat <- data.frame(
    row = 1:2,
    col = 1,
    data_type = c("character", "numeric"),
    character = c("header 1", NA),
    numeric = c(NA, 1)
  )

  expect_equal(find_all_instances(dat, "1", "row"), 1)
  expect_equal(find_all_instances(dat, 1, "row"), 1)
})

