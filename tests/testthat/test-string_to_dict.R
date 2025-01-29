context("string_to_dict")

test_that("string_to_dict works for basic cases", {
  expected_output <- data.frame(
    key = c("key1", "key2", "key3"),
    item = c("value1", "value2", "value3")
  )
  
  # Test case 1: Basic key-value pairs
  basic_input <- "key1: value1, key2: value2, key3: value3"
  basic_result <- string_to_dict(basic_input) %>% 
    mutate(item = unlist(item))
  
  expect_equal(basic_result, tibble(expected_output))
  
  # Test case 2: Key-value pairs with spaces
  spaces_input <- "  key1 : value1  , key2:  value2,  key3: value3  "
  spaces_result <- string_to_dict(spaces_input) %>% 
    mutate(item = unlist(item))
  expect_equal(spaces_result, tibble(expected_output))
  
  # Test case 3: string with curly braces
  braces_input <- "{key1: value1, key2: value2, key3: value3}"
  braces_result <- string_to_dict(braces_input) %>% 
    mutate(item = unlist(item))
  expect_equal(braces_result, tibble(expected_output))
})

test_that("string_to_dict works for item lists", {
  expected_output <- data.frame(key = c("name", "age", "key3")) %>% 
    dplyr::mutate(item = case_when(
      key == "name" ~ stringr::str_split("John, Janet", ","),
      key == "age" ~ stringr::str_split("25, 26", ","),
      key == "key3" ~ stringr::str_split("value3", ","))) 
  
  # simple (expected) nested dict
  simple_input <- "{name: John, Janet age: 25, 26, key3: value3}"
  simple_result <- string_to_dict(simple_input)
  expect_equal(simple_result, tibble(expected_output))
  
  # nested dict with square braces
  braced_input <- "{name: [John, Janet] age: [25, 26]  key3: value3}"
  braced_result <- string_to_dict(braced_input)
  expect_equal(braced_result, tibble(expected_output))
})

test_that("string_to_dict returns output for blank string", {
  blank_input <- "{}"
  expected_output <- data.frame(key = character(0), item = character(0))
  result <- suppressWarnings(string_to_dict(blank_input))
  expect_equal(result, expected_output)
})  

test_that("string_to_dict returns appropriate warnings", {
  expect_warning(string_to_dict("{}"), 
                 "Empty input string provided. Returning an empty dictionary.")
}
)
