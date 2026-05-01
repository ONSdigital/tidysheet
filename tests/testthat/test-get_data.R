test_that("get_data raises an error if the file type is wrong" , {
  expect_error(get_data("file.ods", "sheet.*1"))
  expect_error(get_data("file.csv", "sheet.*1"))
  expect_error(get_data("file.txt", "sheet.*1"))
})
