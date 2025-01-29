context("get_dict_item returns expected output")

test_that("get_dict_item returns correct items", {
  dict <- tibble(key = c("key1", "key2"),
                 item = c("item1", "item2a, item2b")
  ) %>% 
    dplyr::mutate(item = stringr::str_split(item, ", "))
  
  single_item <- get_dict_item("key1", dict) 
  single_item_from_array <- get_dict_item("key2", dict)[1] 
  array_item <- get_dict_item("key2", dict)
  
  expect_equal(single_item, "item1")
  expect_equal(single_item_from_array, "item2a")
  expect_equal(array_item, c("item2a", "item2b"))
}
)