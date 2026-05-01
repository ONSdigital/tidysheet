test_that("get_release_number returns NA if no valid release numbers are found", {
  dat <- c(
    "title - release", "1 release", "title - 1st", "first", "eighth release"
  )

  expect_warning(
    result <- suppressMessages(get_release_number(dat)),
    "No release number found."
    )
})


test_that("get_release_number doesn't return numbers higher than 7", {
  dat <- c("eighth release", "release 8")
  expect_warning(
    result <- suppressMessages(get_release_number(dat)),
    "No release number found."
  )
})


test_that("get_release_number returns release numbers lower than 8", {
  dat <- c("title - release 7", "eighth release")

  result <- suppressMessages(get_release_number(dat))

  expect_equal(result, '7')

})


test_that("get_release_number returns the highest number under 8 with a warning if multiple found", {
  dat <- c("title - release 1", "sixth release" )

  expect_warning(
    result <- suppressMessages(get_release_number(dat)),
    "Multiple potential release numbers found"
  )

  expect_equal(result, '6')

})

test_that("get_release_number sees words and numbers as equivalent - e.g. no warning for first release and release 1", {
  dat <- c("release 6", "sixth release" )

  expect_no_warning(
    result <- suppressMessages(get_release_number(dat))
  )

  expect_equal(result, '6')

})
