test_that("remove_underscore_lines removes consecutive underscores", {
  dat <- data.frame(description_1 = c("__A___", "_______"))
  expected <- data.frame(description_1 = c("A", ""))
  result <- suppressMessages(
    remove_underscore_lines(dat, "description_1")
  )
  expect_equal(result, expected)
})


test_that("remove_underscore_lines does not remove single underscores", {
  dat <- data.frame(description_1 = c("_B_"))
  result <- suppressMessages(
    remove_underscore_lines(dat, "description_1")
  )
  expect_equal(result, dat)
})


test_that("remove_underscore_lines does not remove underscores from unmentioned columns", {
  dat <- data.frame(description_1 = "__C__", description_2 = "__C__")
  expected <- data.frame(description_1 = "__C__", description_2 = "C")
  result <- suppressMessages(
    remove_underscore_lines(dat, "description_2")
  )
  expect_equal(result, expected)
})
