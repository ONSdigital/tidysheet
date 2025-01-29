context("get_layout_type returns correct layout_type")

test_that("get_layout_type returns correct item", {
  # testing when layout_dict variable has multiple items and a single item
  single_year_single_item <- string_to_dict("{from_2020_to_21: row_headers}") 
  single_year_two_items <- string_to_dict("{from_2020_to_21: [three_row_header, multi_table]}") 
  single_year_three_items <- string_to_dict("{from_2020_to_21: [three_row_header, multi_table, row_headers]}")
  two_years <- string_to_dict("{from_2020_to_21: three_row_header, multi_table, from_2021_to_22: multi_table}")
  
  result_single_year_single_item <- get_layout_type("2021",single_year_single_item)
  result_single_year_two_items_default <- suppressWarnings(get_layout_type("2021", single_year_two_items))
  result_single_year_two_items_second <- suppressWarnings(get_layout_type("2021", single_year_two_items, 2))
  result_single_year_three_items_first <- suppressWarnings(get_layout_type("2021", single_year_three_items, 1))
  result_single_year_three_items_second <- suppressWarnings(get_layout_type("2021", single_year_three_items, 2))
  result_single_year_three_items_third <- suppressWarnings(get_layout_type("2021", single_year_three_items, 3))
  result_two_years_item_not_given <- suppressWarnings(get_layout_type("2021", two_years, 2))
  
  expect_equal(result_single_year_single_item, "row_headers")
  expect_equal(result_single_year_two_items_default, "three_row_header")
  expect_equal(result_single_year_two_items_second, "multi_table")
  expect_equal(result_single_year_three_items_first, "three_row_header")
  expect_equal(result_single_year_three_items_second, "multi_table")
  expect_equal(result_single_year_three_items_third, "row_headers")
  expect_equal(is.na(result_two_years_item_not_given), TRUE)
  
  expect_warning(get_layout_type("2021", single_year_two_items), 
                 "Layout type has more than one element for 2021, now using element 1: three_row_header")
  expect_warning(get_layout_type("2021", single_year_three_items), 
                 "Layout type has more than one element for 2021, now using element 1: three_row_header")
  expect_warning(get_layout_type("2021", two_years, 2), 
                 "Expecting 2 layouts for 2021 but only 1 given in data dict. Please speak to a developer.")
  
  expect_error(get_layout_type("2018", single_year_single_item))
})