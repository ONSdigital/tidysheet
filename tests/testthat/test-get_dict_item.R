dict <- tibble(
  key = c("key1", "key2", "key3", "key4"),
  item = c(
    list("item1"), list(c("item2a", "item2b")), list(NA), list(c("true", "false"))
  )
)

test_that("get_dict_item returns a single item correctly", {

  item <- get_dict_item("key1", dict)
  expect_equal(item, "item1")

})

test_that("get_dict_item returns a single item from an array correctly", {

  item <- get_dict_item("key2", dict)[1]
  expect_equal(item, "item2a")

})


test_that("get_dict_item returns an array correctly", {

  item <- get_dict_item("key2", dict)
  expect_equal(item, c("item2a", "item2b"))

})


test_that("get_dict_item returns NA if item is NA", {

  expect_equal(get_dict_item("key3", dict), NA)

})


test_that("get_dict_item returns TRUE and FALSE for true and false", {

  expect_equal(get_dict_item("key4", dict), c(TRUE, FALSE))

})


test_that("get_dict_item returns NA if item is not present in dict", {

  expect_equal(get_dict_item("key5", dict), NA)

})
