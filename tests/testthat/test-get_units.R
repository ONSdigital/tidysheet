test_that("get_units returns table_units before sheet_units", {
  expect_equal(
    suppressMessages(get_units("millions", "thousands")), "thousands"
    )
})


test_that("get_units returns table_units when sheet_units is NA", {
  expect_equal(suppressMessages(get_units(NA, "thousands")), "thousands")
})


test_that("get_units returns sheet_units before NA", {
  expect_equal(suppressMessages(get_units("millions", NA)), "millions")

})


test_that("get_units returns NA if both inputs are NA", {
  expect_equal(suppressMessages(get_units(NA, NA)), NA)
})
