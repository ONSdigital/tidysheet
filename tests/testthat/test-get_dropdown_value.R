test_that("get_dropdown_value returns NA if pattern is NA", {

  dat <- data.frame(
    address = c("A1", "A2", "A3"),
    row = 1:3,
    col = 1,
    character = c("Total England", "Wales is better than England", "Total UK")
    )

  result <- suppressMessages(get_dropdown_value(dat, NA))
  expect_equal(result, NA)


})


test_that("get_dropdown_value gets value based on last match to regex pattern", {

  dat <- data.frame(
    address = c("A1", "A2", "A3"),
    row = 1:3,
    col = 1,
    character = c("Data for England and Wales", "£million", "Total England"))


  result <- suppressMessages(get_dropdown_value(dat, "England"))

  expect_equal(result, "Total England")

})


test_that("get_dropdown_value gets value based on LAST match to regex pattern regardless of row order", {


  dat <- data.frame(
    address = c("A3", "A2", "A1"),
    row = rev(1:3), # mix up the row order
    col = 1,
    character = c("Total England", "£million", "Data for England and Wales"))

  result <- suppressMessages(get_dropdown_value(dat, "England"))

  expect_equal(result, "Total England")

})


test_that("get_dropdown_value gets value based on LAST match to regex pattern regardless of col order", {


  dat <- data.frame(
    address = c("A3", "A2", "B1"),
    row = rev(1:3),
    col = c(1, 1, 2),
    character = c("Total England", "£million", "Data for England and Wales"))


  result <- suppressMessages(get_dropdown_value(dat, "England"))

  expect_equal(result, "Total England")

})
