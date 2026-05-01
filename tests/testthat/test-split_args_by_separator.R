test_that("split_args_by_separator returns unaltered dict if separator is not specified", {
  dict <- tibble(
    key = c("var1", "var2"),
    item = c(list(c("a", "b", "next", "c", "next", "d")),
             list("1"))
  )
  result <- suppressMessages(split_args_by_separator(dict, NA))

  expect_equal(result, dict)
})


test_that("split_args_by_separator raises an error if the separator is nowhere in items", {
  dict <- tibble(
    key = c("var1", "var2"),
    item = c(list(c("a", "b", "next", "c", "next", "d")),
             list("1"))
  )
  expect_error(
    suppressMessages(split_args_by_separator(dict, "-next_table-")),
    "separator is specified but is not present for any variable"
    )
})


test_that("split_args_by_separator splits by the given separator when length is 1 or more", {
  dict <- tibble(
      key = c("var1", "var2"),
      item = c(list(c("a", "b", "next", "c", "next", "d")),
               list("1"))
      )

  expected <- dict %>%
    mutate(
      item_1 = c(list(c("a", "b")), list("1")),
      item_2 = c(list("c"), list("1")),
      item_3 = c(list("d"), list("1"))
    )

  result <- suppressMessages(split_args_by_separator(dict, separator="next"))

  expect_equal(data.frame(result), data.frame(expected))
})


test_that("split_args_by_separator errors if key and item are not present", {
  dict <- tibble(
    name = c("var1", "var2"),
    thing = c(list(c("a", "b", "next", "c")), list("1"))
  )

  expect_error(
    suppressMessages(split_args_by_separator(dict, separator="next")),
    "dict must contain the columns 'key' and 'item'."
  )
})


test_that("split_args_by_separator errors if either key or item are not present", {
  dict_1 <- tibble(
    key = c("var1", "var2"),
    thing = c(list(c("a", "b", "next", "c")), list("1"))
  )
  dict_2 <- tibble(
    name = c("var1", "var2"),
    item = list(c("a", "b", "next", "c"), ("1"))
  )

  expect_error(
    suppressMessages(split_args_by_separator(dict_1, separator="next")),
    "dict must contain the columns 'key' and 'item'."
  )
  expect_error(
    suppressMessages(split_args_by_separator(dict_2, separator="next")),
    "dict must contain the columns 'key' and 'item'."
  )
})


test_that("split_args_by_separator errors if an extra column exists", {
  dict <- tibble(
    key = c("var1", "var2"),
    item = c(list(c("a", "b", "next", "c")), list("1")),
    item_1 = c(list(c("a", "b")), list("1"))
  )

  expect_error(
    suppressMessages(split_args_by_separator(dict, separator="next")),
    "dict must contain only 'key' and 'item' columns."
  )

})


test_that("split_args_by_separator throws an error if different numbers of tables exist", {
  dict <- tibble(
    key = c("var1", "var2"),
    item = c(list(c("a", "b", "next", "c", "next", "d")),
             list(c("e", "f", "next", "g")))
  )
  expect_error(
    suppressMessages(split_args_by_separator(dict, separator="next")),
    "Not all settings that include the separator string are of the same length"
    )
})
