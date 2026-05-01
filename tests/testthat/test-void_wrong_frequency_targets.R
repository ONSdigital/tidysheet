dat <- data.frame(
  "item" = c("Total", "Total", "Other"),
  "numeric" = c(2, 3, 10),
  "matches_regex" = c(rep(TRUE, 2), FALSE),
  "location" = c(1, 2, 1),
  "rename_required" = c(TRUE, FALSE, FALSE)
)

test_that("void_wrong_frequency_targets has no effect when expected frequency matches actual frequency", {

  result <- void_wrong_frequency_targets(dat, "item", 2)
  expect_equal(result, dat)

})


test_that("void_wrong_frequency_targets has no effect when expected frequency is lower than actual frequency", {

  expected <- mutate(dat, rename_required = FALSE)

  expect_warning(
    result <- void_wrong_frequency_targets(dat, "item", 1),
    "More.*found for 'Total'. 2 repeats were found where only 1 were expected"
    )

  expect_equal(result, expected)
})


test_that("void_wrong_frequency_targets has no effect when expected frequency is higher than actual frequency", {

  expected <- mutate(dat, rename_required = FALSE)

  expect_warning(
    result <- void_wrong_frequency_targets(dat, "item", 3),
    "Fewer.*found for 'Total'. 2 repeats were found where 3 were expected")

  expect_equal(result, expected)

})
