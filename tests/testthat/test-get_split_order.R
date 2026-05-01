test_that("get_split_order correctly gives the order in which split_point patterns should be used for each item", {

  dat <- data.frame(
    col_to_split = c("split\n 1.this \n 2.one \n 3.thousand", "split\n 2.one", "no splits"),
    'tmp_use_split_point_1' = c(TRUE, FALSE, FALSE),
    'tmp_use_split_point_2' = c(TRUE, TRUE, FALSE),
    'tmp_use_split_point_3' = c(TRUE, FALSE, FALSE),
    'tmp_use_split_point_4' = c(FALSE, FALSE, FALSE),
    value = c(100, 600, 200)
  )

  # tmp_use_split_point_0 needs to be added as column (all TRUE) for functions
  # called later to work correctly - not ideal that it is in this function,
  # but not worth the work required to move it.
  expected <- dat %>%
    mutate(tmp_use_split_point_0 = TRUE,
           tmp_split_1 = as.integer(c(1, 2, 4)),
           tmp_split_2 = as.integer(c(2, 2, 4)),
           tmp_split_3 = as.integer(c(3, 4, 4)))

  result <- get_split_order(dat)

  expect_equal(expected, result)

})
