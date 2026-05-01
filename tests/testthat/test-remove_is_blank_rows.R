test_that("remove_is_blank_rows removes only is_blank == TRUE", {
  dat <- data.frame(is_blank = c(TRUE, FALSE), numeric = c(NA, 1))
  expected <- data.frame(is_blank = FALSE, numeric = 1)
  result <- suppressMessages(remove_is_blank_rows(dat))
  expect_equal(result, expected)
})


test_that("remove_is_blank_rows changes nothing if is_blank does not exist", {
  dat <- data.frame(numeric = c(NA, 1))
  result <- suppressMessages(remove_is_blank_rows(dat))
  expect_equal(result, dat)
})
