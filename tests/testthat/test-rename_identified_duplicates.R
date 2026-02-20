dat <- data.frame(
  "year" = rep(c("2022-23", "2023-24"), each = 3),
  "item" = rep(c("Total", "Total", "Total"), 2),
  "matches_regex" = rep(TRUE, 6),
  "location" = rep(c(1, 2, 3), 2),
  "rename_required" = rep(TRUE, 6)
)

test_that("rename_identified_duplicates gives expected result when all cases need renaming", {

  cols_to_group_by <- c("year", "item_detail")

  expected <- dat %>%
    mutate(item = rep(c("first Total", "second Total", "third Total"), 2),
           rename_note = "item was 'Total' in the raw data. It was given a prefix because there was more than one instance in the raw data with that name.") %>%
    select(-c(location, rename_required))

  result <- rename_identified_duplicates(
    dat,
    cols_to_group_by = c("year", "item"),
    column = "item",
    index = 1:3,
    prefix = c("first", "second", "third"),
    expected_freq = 3
    )

  expect_equal(expected, result)
})


test_that("rename_identified_duplicates gives expected result when 2 of 3 cases need renaming", {

  dat <- dat %>%
    mutate("rename_required" = rep(c(TRUE, FALSE, TRUE), 2))

  expected <- dat %>%
    mutate(item = rep(c("first Total", "Total", "third Total"), 2),
           rename_note = ifelse(rename_required == TRUE,
                                "item was 'Total' in the raw data. It was given a prefix because there was more than one instance in the raw data with that name.",
                                NA)) %>%
    select(-c(location, rename_required))

  result <- rename_identified_duplicates(
    dat,
    cols_to_group_by = c("year", "item"),
    column = "item",
    index = c(1, 3),
    prefix = c("first", "third"),
    expected_freq = 3
  )
  expect_equal(result, expected)
})


test_that("rename_identified_duplicates gives expected result when only 1 case needs renaming", {

  dat <- dat %>%
    mutate("rename_required" = rep(c(FALSE, TRUE, FALSE), 2))

  expected <- dat %>%
    mutate(item = rep(c("Total", "second Total", "Total"), 2),
           rename_note = ifelse(rename_required == TRUE,
                                "item was 'Total' in the raw data. It was given a prefix because there was more than one instance in the raw data with that name.",
                                NA)) %>%
    select(-c(location, rename_required))

  result <- rename_identified_duplicates(
    dat,
    cols_to_group_by = c("year", "item"),
    column = "item",
    index = 2,
    prefix = c("second"),
    expected_freq = 3
  )

  expect_equal(result, expected)

})


test_that("rename_identified_duplicates gives expected result for rows flagged as not needing a rename", {

  expected <- dat %>%
    mutate(item = rep(c("first Total", "Total", "third Total"), 2),
           rename_note = ifelse(rename_required == TRUE,
                                "item was 'Total' in the raw data. It was given a prefix because there was more than one instance in the raw data with that name.",
                                NA)) %>%
    add_row(year = rep(c("2022-23", "2023-24"), 2),
            item = "A",
            matches_regex = FALSE) %>%
    select(-c(location, rename_required))

  dat <- dat %>%
    add_row(year = rep(c("2022-23", "2023-24"), 2),
            item = "A",
            matches_regex = FALSE,
            location = rep(c(1, 2), 2),
            rename_required = FALSE)


  result <- rename_identified_duplicates(
    dat,
    cols_to_group_by = c("year", "item"),
    column = "item",
    index = c(1,3),
    prefix = c("first", "third"),
    expected_freq = 3
  )

  expect_equal(result, expected)

})
