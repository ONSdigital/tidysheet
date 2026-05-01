test_that("get_first_of_consecutives gives expected output", {

  beginning <- suppressMessages(get_first_of_consecutives(c(1, 2, 3, 4)))
  middle <- suppressMessages(get_first_of_consecutives(c(1, 3, 4, 6)))
  end <- suppressMessages(get_first_of_consecutives(c(1, 3, 5, 6)))
  multiple <- suppressMessages(get_first_of_consecutives(c(1, 3, 4, 6, 7)))
  repeated_values <- suppressMessages(
    get_first_of_consecutives(c(1, 3, 3, 5, 5, 6))
  )
  none <- suppressWarnings(suppressMessages(
    get_first_of_consecutives(c(1, 3, 5))
  ))
  none_starting_later <- suppressWarnings(suppressMessages(
    get_first_of_consecutives(c(3, 5))
    ))

  expect_equal(beginning, 1)
  expect_equal(middle, 3)
  expect_equal(end, 5)
  expect_equal(multiple, 3)
  expect_equal(repeated_values, 5)
  expect_equal(none, 1)
  expect_equal(none_starting_later, 3)

  expect_warning(suppressMessages(get_first_of_consecutives(c(1, 3, 5))))
})


test_that("get_first_of_consecutives returns number with at least x consecutives", {

  dat <- c(1, 3:4, 6:7, 9:11, 13:16)
  expect_equal(
    suppressMessages(get_first_of_consecutives(dat, min_consecutives = 2)),
    3)
  expect_equal(
    suppressMessages(get_first_of_consecutives(dat, min_consecutives = 3)),
    9)
  expect_equal(
    suppressMessages(get_first_of_consecutives(dat, min_consecutives = 4)),
    13)

})

test_that("get_first_of_consecutives has default offset value of 0", {

  dat <- c(1, 3:4, 6:9)
  expect_equal(suppressMessages(get_first_of_consecutives(dat, offset = NA)), 3)
  expect_equal(suppressMessages(get_first_of_consecutives(dat)), 3)

})


test_that("get_first_of_consecutives has default min_consecutives value of 2", {

  dat <- c(1, 3:4, 6:9)
  expect_equal(
    suppressMessages(get_first_of_consecutives(dat, min_consecutives = NA)),
    3)
  expect_equal(suppressMessages(get_first_of_consecutives(dat)), 3)

})


test_that("get_first_of_consecutives returns number plus offset value", {

  dat <- c(1, 3:4, 6:9)
  expect_equal(suppressMessages(get_first_of_consecutives(dat, offset = 1)), 4)
  expect_equal(suppressMessages(get_first_of_consecutives(dat, offset = 2)), 5)
  expect_equal(suppressMessages(get_first_of_consecutives(dat, offset = -1)), 2)

})


test_that("get_first_of_consecutives returns number with at least x consecutives plus offset value", {
  dat <- c(1, 3:4, 6:7, 9:11, 13:16)
  expect_equal(suppressMessages(get_first_of_consecutives(dat, 2, 1)), 4)
  expect_equal(suppressMessages(get_first_of_consecutives(dat, 3, 2)), 11)
  expect_equal(suppressMessages(get_first_of_consecutives(dat, 4, -3)), 10)
})


test_that("get_first_of_consecutives throws an error if the eturn value is less than 1", {

  dat <- c(1, 3:4, 6:9)
  expect_error(
    suppressMessages(
      get_first_of_consecutives(dat, offset = -3)
    ),
    "The number cannot be less than 1"
  )

})
