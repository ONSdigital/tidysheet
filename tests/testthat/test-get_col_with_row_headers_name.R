test_that("get_col_with_row_headers_name correctly identifies the match", {
  result <- suppressMessages(
    get_col_with_row_headers_name(c("column 1", "column 2"), "2")
  )
  expect_equal(result, "column 2")
})


test_that(
  "get_col_with_row_headers_name throws error is there are multiple matches and
  the first match is returned", {

    expect_warning(
      result <- suppressMessages(
        get_col_with_row_headers_name(c("column 1", "column 2"), "column")
      ),
      "More than one column was identified"
    )
    expect_equal(result, "column 1")

  }
)


test_that("get_col_with_row_headers_name returns NA if pattern is NA", {
  result <- suppressMessages(
    get_col_with_row_headers_name(c("column 1", "column NA 2"), NA)
  )
  expect_equal(result, NA)
})
