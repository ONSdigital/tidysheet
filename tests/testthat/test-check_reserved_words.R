test_that("check_reserved_words identifies when no reserved words have been used", {

  dict <- tibble(
    "key" = c("header_identifier", "group_col"),
    "item" = list("safe", c("safe", "safe"))
  )

  expect_true(check_reserved_words(dict, NA))

})


test_that("check_reserved_words raises an error if only one reserved word is used", {

  dict <- tibble(
    "key" = c("header_identifier", "group_col"),
    "item" = c("row", "safe")
  )
  expect_error(
    check_reserved_words(dict, NA),
    "'row'"
  )

})


test_that("check_reserved_words does not raise an error if only a reserved word is used for an excluded variable", {

  dict <- tibble(
    "key" = c("header_identifier", "group_col"),
    "item" = c("row", "safe")
  )
  expect_true(check_reserved_words(dict, "header_identifier"))

})


test_that("check_reserved_words works when an excluded variable is not in the dict", {

  dict <- tibble(
    "key" = c("header_identifier", "group_col"),
    "item" = c("row", "safe")
  )
  expect_error(check_reserved_words(dict, "nope"), "'row'")

})


test_that("check_reserved_words raises an error if multiple reserved words are used", {

  dict <- tibble(
    "key" = c("header_identifier", "group_col"),
    "item" = list("row", c("col", "safe"))
  )

  expect_error(
    check_reserved_words(dict, NA),
    "'row', 'col'"
    )

})


test_that("check_reserved_words raises an error if multiple reserved words are used, even if one is an excluded variable", {

  dict <- tibble(
    "key" = c("header_identifier", "group_col"),
    "item" = list("row", c("col", "safe"))
  )

  expect_error(
    check_reserved_words(dict, "header_identifier"),
    "'col'"
  )

})


test_that("check_reserved_words does not raise an error if multiple reserved words are used, if they are all in excluded variables", {

  dict <- tibble(
    "key" = c("header_identifier", "group_col"),
    "item" = list("row", c("col", "safe"))
  )

  expect_true(check_reserved_words(dict, c("header_identifier", "group_col")))
})


test_that("check reserved words works when exclude contains NA alongside a reserved word", {

  dict_to_error <- tibble(
    "key" = c("header_identifier", "group_col"),
    "item" = list("row", c("col", "safe"))
  )

  expect_error(check_reserved_words(dict_to_error, c(NA, "group_col")), "'row'")

  dict_to_pass <- tibble(
    "key" = c("header_identifier", "group_col"),
    "item" = list("fine", c("col", "safe"))
  )

  expect_true(check_reserved_words(dict_to_pass, c(NA, "group_col")))


  })


test_that("check_reserved_words handles an empty dict", {

  dict <- list()
  expect_true(check_reserved_words(dict, NA))

})


test_that("check_reserved_words handles an empty dict even when excluded is specified", {

  dict <- list()
  expect_true(check_reserved_words(dict, "header"))

})


