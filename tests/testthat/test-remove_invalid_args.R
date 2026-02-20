test_that("remove_invalid_args removes keys not in the args list", {
  dict <- data.frame(key = c("do_thing", "sing"), item = c("jump", "A"))
  variables <- c("do_thing", "ignore_thing")
  expected <- data.frame(key = c("do_thing"), item = c("jump"))
  expect_warning(
    result <- remove_invalid_args(dict, variables),
    "Invalid variable found in settings: 'sing'."
  )
  expect_equal(result, expected)
})
