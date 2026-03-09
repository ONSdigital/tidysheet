test_that("behead_left_block associates value cells with a single column of descriptors", {

  dat <- data.frame(
    address = c("A1", "B1", "C1", "A2", "B2", "C2"),
    row = c(1, 1, 1, 2, 2, 2),
    col = c(1, 2, 3, 1, 2, 3),
    data_type = c(rep("character", 4), rep("numeric", 2)),
    numeric = c(rep(NA, 4), 100, 22),
    character = c("Year", "primary", "secondary", "2021", NA, NA))

  expected <- dat %>%
    filter(col != 1) %>%
    mutate(Year = c("Year", "Year", "2021", "2021"),
           set_is_blank_to_true = FALSE)

  result <- suppressMessages(behead_left_block(dat, 'Year', 1, 1))

  expect_equal(result, tibble(expected))
})


test_that("behead_left_block associates value cells with multiple columns of descriptors", {

  dat <- data.frame(
    address = c("A1", "B1", "C1", "A2", "B2", "C2"),
    row = c(1, 1, 1, 2, 2, 2),
    col = c(1, 2, 3, 1, 2, 3),
    data_type = c(rep("character", 5), "numeric"),
    numeric = c(rep(NA, 5), 22),
    character = c("Year", "Region", "secondary", "2021 Q1", "Wales", NA))

  expected <- dat %>%
    filter(col == 3) %>%
    mutate(Year = c("Year", "2021 Q1"),
           Region = c("Region", "Wales"),
           set_is_blank_to_true = FALSE)

  result <- suppressMessages(
    behead_left_block(dat, c("Year", "Region"), 1, 1)
    )

  expect_equal(result, tibble(expected))

})


test_that("behead_left_block does not drop rows with row names but no values and two left header columns", {

  dat <- data.frame(
    address = c("A1", "B1", "C1", "A2", "B2", "C2", "A3", "B3", "C3",
                "A4", "B4", "C4", "A5", "B5", "C5"),
    row = rep(1:5, each = 3),
    col = rep(1:3, times = 5),
    data_type = c(rep("character", 5), "blank", rep("character", 2), "numeric",
                  rep("character", 2), "blank",  rep("character", 2), "numeric"),
    numeric = c(rep(NA, 8), 1, rep(NA, 5), 2),
    character = c("group", "subgroup", "2022", "group 1", "1b", NA,
                  "A", "a", NA, "group 2", "2b", NA, "A", "a", NA)
  )

  expected <- dat %>%
    filter(col == 3) %>%
    mutate(Group = c("group", "group 1", "A", "group 2", "A"),
           subgroup = c("subgroup", "1b", "a", "2b", "a"),
           set_is_blank_to_true = FALSE)

  result <- suppressMessages(
    behead_left_block(dat, c("Group", "subgroup"), 7, 1)
  )

  expect_equal(result, tibble(expected))
})


test_that("behead_left_block does not drop rows with row names but no values and only one left header column", {

  dat <- data.frame(
    address = c("A7", "B7", "A8", "A9", "B9", "A10", "A11", "B11"),
    row = c(7, 7, 8, 9, 9, 10, 11, 11),
    col = c(1, 2, 1, 1, 2, 1, 1, 2),
    data_type = c(rep("character", 4), "numeric",
                  rep("character", 2), "numeric"),
    numeric = c(rep(NA, 4), 10, NA, NA, 5),
    character = c("Group", "2022", "group 1", "a", NA, "group 2", "a", NA)
    )

  expected <- data.frame(
    address = c("B7", "A8",  "B9", "A10",  "B11"),
    row = c( 7, 8,  9, 10, 11),
    col = c(2, 1, 2, 1, 2),
    data_type = c(rep("character", 2), "numeric", "character", "numeric"),
    numeric = c(rep(NA, 2), 10, NA, 5),
    character = c("2022", rep(NA, 4)),
    Group = c("Group", "group 1", "a", "group 2", "a"),
    set_is_blank_to_true = c(FALSE, TRUE, FALSE, TRUE, FALSE)
  )

  result <- suppressMessages(
    behead_left_block(dat, "Group", 7, 1)
  )

  expect_equal(result, tibble(expected))

})
