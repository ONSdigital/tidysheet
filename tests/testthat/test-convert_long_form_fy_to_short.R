test_that("convert_long_form_fy_to_short works as expected", {
  result <- convert_long_form_fy_to_short(
    c("2021/2022", "2021-2022", "2022 to 2023")
    )
  expected <-  c("2021/22", "2021-22", "2022 to 23")
  expect_equal(result, expected)
})
