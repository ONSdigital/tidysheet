dat_1 <- data.frame(a = 1)
dat_2 <- data.frame(b = 2)
dat_3 <- data.frame(c = 3)
dat_4 <- data.frame(c = 4)


test_that("remove_used_tables raises an error if used tables are selected to be kept", {

  dat <- list(dat_1, dat_2, dat_3)
  expect_error(
    suppressMessages(remove_used_tables(dat, 1, 1)),
    "correct the settings so that no subtables used to create further subtables are selected"
  )
})


test_that("remove_used_tables removes all tables whose index is listed in
used_tables and keep all remaining by default (keep is NA)", {

  dat <- list(dat_1, dat_2, dat_3)
  expected <- list(dat_2, dat_3)
  result <- suppressMessages(remove_used_tables(dat, 1))
  expect_equal(result, expected)
})


test_that("remove_used_tables removes all tables whose index is listed in
used_tables and not listed in keep.", {

  dat <- list(dat_1, dat_2, dat_3, dat_4)
  used <- c(1, 3)
  keep <- 4
  expected <- list(dat_4)
  result <- suppressMessages(remove_used_tables(dat, used, keep))
  expect_equal(result, expected)
})


