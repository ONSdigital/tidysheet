test_that("get_first_instance throws an error if input doesn't contain required columns", {
  expect_error(
    suppressMessages(
      get_first_instance(c("row", "character"), "abc")
    ),
    class = "simpleError"
  )
  expect_error(
    suppressMessages(
      get_first_instance(data.frame("value" = 1, "character" = "abc"), "abc")
      ),
    class = "simpleError"
  )
  expect_error(
    suppresssMessages(
      get_first_instance(data.frame("row" = 1, "value" = "abc"),"abc")
      ),
    class = "simpleError"
    )

})


test_that("get_first_instance gives the location of the first row/column", {
  expect_equal(
    suppressMessages(
      get_first_instance(
        data.frame("row" = c(1:3), "character" = c("aa","bb", "bb")), "bb"
      )),
    2
  )
  expect_equal(
    suppressMessages(
      get_first_instance(
        data.frame("row" = c(1:3), "character" = c("aa","bb", "bb")), "bb", "row"
      )),
    2
  )
  expect_equal(
    suppressMessages(
      get_first_instance(

        data.frame("col" = c(1:3), "character" = c("aa","bb", "bb")), "bb", "col"
      )),
    2
  )
})


test_that("get_first_instance does not require the pattern to match the full string", {
  expect_equal(
    suppressMessages(
      get_first_instance(
        data.frame("row" = c(1:2), "character" = c("aa","ab")), "b"
      )),
    2
  )
  expect_equal(
    suppressMessages(
      get_first_instance(
        data.frame("col" = c(1:2), "character" = c("aa","ab")), "b", "col"
      )),
    2
  )
})
