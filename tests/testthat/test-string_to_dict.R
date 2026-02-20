test_that("string_to_dict works for basic cases", {
  input <- "key1: value1, key2: value2, key3: value3"

  expected <- tibble(
    key = c("key1", "key2", "key3"),
    item = c(list("value1"), list("value2"), list("value3"))
  )

  result <- string_to_dict(input)
  expect_equal(result, expected)

})

test_that("string_to_dict works for basic cases with spaces", {
  expected <- tibble(
    key = c("key1", "key2", "key3"),
    item = c(list("value1"), list("value2"), list("value3"))
  )
  input <- "  key1 : value1  , key2:  value2,  key3: value3  "

  result <- string_to_dict(input)
  expect_equal(result, expected)
})

test_that("string_to_dict works for basic cases with curly braces included", {
  expected <- tibble(
    key = c("key1", "key2", "key3"),
    item = c(list("value1"), list("value2"), list("value3"))
  )
  input <- "{key1: value1, key2: value2, key3: value3}"
  result <- string_to_dict(input)
  expect_equal(result, expected)
})

test_that("string_to_dict works for item lists", {
  expected <- tibble(
    key = c("name", "age", "key3")) %>%
    dplyr::mutate(item = case_when(
      key == "name" ~ stringr::str_split("John, Janet", ","),
      key == "age" ~ stringr::str_split("25, 26", ","),
      key == "key3" ~ stringr::str_split("value3", ",")))

  input <- "{name: John, Janet age: 25, 26, key3: value3}"
  result <- string_to_dict(input)
  expect_equal(result, expected)

})

test_that("string_to_dict works for nested dict with square braces", {
  expected <- tibble(
    key = c("name", "age", "key3")) %>%
    dplyr::mutate(item = case_when(
      key == "name" ~ stringr::str_split("John, Janet", ","),
      key == "age" ~ stringr::str_split("25, 26", ","),
      key == "key3" ~ stringr::str_split("value3", ",")))

  input <- "{name: [John, Janet] age: [25, 26]  key3: value3}"
  result <- string_to_dict(input)
  expect_equal(result, expected)
})

test_that("string_to_dict returns output for blank string", {
  input <- "{}"
  expected <- data.frame(key = character(0), item = character(0))
  expect_warning(result <- string_to_dict(input), "Empty input string provided")
  expect_equal(result, expected)
})
