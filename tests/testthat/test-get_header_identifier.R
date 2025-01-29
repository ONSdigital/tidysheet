context("get_header_identifier returns expected output")

test_that("get_header_identifier returns correct item", {
  header_identifiers <- c("total", "HRA Reserves") 
  
  result_1 <- get_header_identifier(header_identifiers, "1")
  result_2 <- get_header_identifier(header_identifiers, "2")
  result_3 <- get_header_identifier("total", "1")
  
  expect_equal(result_1, "total")  
  expect_equal(result_2, "HRA Reserves")
  expect_equal(result_3, "total")
}
)

test_that("get_header_identifier returns warning message", {
  header_identifiers <- c("total", "HRA Reserves") 
  
  expect_warning(get_header_identifier(header_identifiers, "3"), "file_part is out of range of vector length, contact developer")
  expect_warning(get_header_identifier(header_identifiers, "0"), "file_part is out of range of vector length, contact developer")
}
)
