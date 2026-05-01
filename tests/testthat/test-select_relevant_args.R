test_that("select_relevant_args works when there are only args for one table", {
  dict <- tibble(
      key = c("var1", "var2"),
      item = list(NA),
      item_1 = c(list(c("a", "b")), list("1"))
  )
  expected <- dict %>%
    mutate(item = item_1) %>%
    select(-item_1)

  result <- suppressMessages(select_relevant_args(dict, 1))

  expect_equal(result, expected)
})


test_that("select_relevant_args errors when item part does not exist", {
  dict <- tibble(
    key = c("var1", "var2"),
    item = list(NA),
    item_1 = c(list(c("a", "b")), list("1"))
  )

  expect_error(
    suppressMessages(select_relevant_args(dict, 2)),
    "'item_2' not found."
  )

})


test_that("select_relevant_args works when there are args for multiple tables", {
  dict <- tibble(
      key = c("var1", "var2"),
      item = list(NA),
      item_1 = c(list(c("a", "b")), list("1")),
      item_2 = c(list("c"), list(NA))
      )
  expected_1 <- dict %>%
    mutate(item = item_1) %>%
    select(key, item)

  expected_2 <- dict %>%
    mutate(item = item_2) %>%
    select(key, item)

  result_1 <- suppressMessages(select_relevant_args(dict, 1))
  result_2 <- suppressMessages(select_relevant_args(dict, 2))

  expect_equal(result_1, expected_1)
  expect_equal(result_2, expected_2)
})


test_that("select_relevant_args does nothing if dict is just key and item", {
  dict <- tibble(
    key = c("var1", "var2"),
    item = list(c("a", "b"))
  )

  result <- suppressMessages(select_relevant_args(dict, 1))

  expect_equal(result, dict)

})


test_that("select_relevant_args errors if key and item are not columns", {
  dict_1 <- tibble(key = "var1")
  dict_2 <- tibble(item_1 = list("a"))
  dict_3 <- tibble(key = list("var1"), item_1 = list("a"))

  expect_error(
    suppressMessages(select_relevant_args(dict_1, 1)),
    "'item_1' not found"
  )

  expect_error(
    suppressMessages(select_relevant_args(dict_2, 1)),
    "dict must contain the column 'key'"
  )

  expect_error(
    suppressMessages(select_relevant_args(dict_3, 1)),
    "dict must contain 'key' and 'item' columns."
  )
})
