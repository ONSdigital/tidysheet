dat <- data.frame(
  "item" = c("Total A", "Total A", "Other"),
  "matches_regex" = c(TRUE, TRUE, FALSE),
  "location" = c(1, 2, 1),
  "rename_required" = c(TRUE, FALSE, FALSE)
)

test_that("refine_target_rows gives expected results when no refining is required", {

  result <- refine_target_rows(dat, colum = "item")
  expect_equal(result, dat)

})


test_that("refine_target_rows gives expected results when there is an extra match but it is not duplicated", {

  extra_match <- dat %>%
    add_row(item = "Total B",
            matches_regex = TRUE,
            location = 1,
            rename_required = TRUE)

  expected <- extra_match %>%
    mutate(rename_required = ifelse(item == "Total B", FALSE, rename_required))

  result <- refine_target_rows(extra_match, "item")

  expect_equal(result, expected)

})


test_that("refine_target_rows gives expected results when there is an extra match and it is also duplicated", {

  extra_match <- dat %>%
    add_row(item = "Total B",
            matches_regex = TRUE,
            location = 2,
            rename_required = FALSE)

  expected <- mutate(extra_match, rename_required = FALSE)

  result <- expect_error(
    refine_target_rows(extra_match, "item"),
    "There are 2 non-unique entries in the 'item' column of the data that match the rename_duplicate_pattern pattern: 'Total A', 'Total B'"
  )

})


test_that("refine_target_rows gives expected results when there are no duplicates", {

  no_duplicates <- dat %>%
    filter(row_number() != 2)

  expected <- mutate(no_duplicates, rename_required = FALSE)

  expect_warning(
  result <- (refine_target_rows(no_duplicates, "item")),
  "rename_duplicate arguments are given but no repeated entries matching.*in the 'item' column"
  )

  expect_equal(result, expected)

})
