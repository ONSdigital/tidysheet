test_that("check_dropdown returns expected message when pattern is found", {
  dat <- data.frame(
    row = c(1:3),
    col = rep(1, 3),
    character = c("Title", "some other info", "England")
  )

  expect_message(
    result <- check_dropdown("Eng", dat),
    "'Eng' found"
  )
})


test_that("check_dropdown throws an error when pattern is not found", {
  dat <- data.frame(
    row = c(1:3),
    col = rep(1, 3),
    character = c("Title", "some other info", "England")
  )

  expect_error(
    check_dropdown("Wales", dat),
    "'Wales' is expected to be in the dropdown selection"
  )

})


test_that("check_dropdown does nothing when pattern is NA", {
  dat <- data.frame(
    row = c(1:3),
    col = rep(1, 3),
    character = c("Title", "some other info", "England")
  )

  result <- check_dropdown(NA, dat)

  expect_equal(result, NULL)
})


test_that("check_dropdown throws error when dropdown does not exist (i.e. is NA)
but is expected", {
  dat <- data.frame(
    row = 1,
    col = 1,
    character = c(NA)
  )

  expect_error(check_dropdown("England", dat), "England")
})


test_that("check_dropdown does nothing when dropdown does not exist (i.e. is NA)
and is not expected", {
  dat <- data.frame(
    row = 1,
    col = 1,
    character = c(NA)
  )

  result <- check_dropdown(NA, dat)

  expect_equal(result, NULL)
})

