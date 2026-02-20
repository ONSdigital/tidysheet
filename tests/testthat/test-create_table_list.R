test_that(
  "create_table_list correctly partitions data when there is only one split to
  do by row", {

    dat <- data.frame(
      "row" = c(1, 1, 1, 2, 2, 2),
      "col" = c(1, 2, 3, 1, 2, 3),
      "address" = c("A1", "B1", "C1",
                    "A2", "B2", "C2"),
      "character"= c("table 1", "value", NA,
                     "table 2", "value", NA),
      "numeric" = c(NA, NA, 1, NA, NA, 2)
    )


    expected <- list(
      dat,
      filter(dat, row == 1),
      filter(dat, row == 2)
    )

    result <- suppressMessages(create_table_list(list(dat), 1, 2, "row"))

    expect_equal(result, expected)

  })


test_that(
  "create_table_list correctly partitions data when there is only one split to
  do by column", {

    dat <- data.frame(
      "row" = c(1, 1, 2, 2),
      "col" = c(1, 2, 1, 2),
      "address" = c("A1", "B1",
                    "A2", "B2"),
      "character"= c("table 1", "table 2",
                     "value", "value")
    )


    expected <- list(
      dat,
      filter(dat, col == 1),
      filter(dat, col == 2)
    )

    result <- suppressMessages(create_table_list(list(dat), 1, 2, "col"))

    expect_equal(result, expected)

  })



test_that(
  "create_table_list correctly partitions data when there are multiple tables", {

    dat <- data.frame(
      "row" = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
      "col" = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
      "address" = c("A1", "B1", "C1",
                    "A2", "B2", "C2",
                    "A3", "B3", "C3"),
      "character"= c("table 1", "value", NA,
                     "table 2", "value", NA,
                     "table 3", "value", NA),
      "numeric" = c(NA, NA, 1, NA, NA, 2, NA, NA, 3)
    )

    # one split has already been done to give:
    dat_list <- list(
      dat,
      filter(dat, row == 1),
      filter(dat, row != 1)
    )

    expected <- list(
      dat_list[[1]],
      dat_list[[2]],
      dat_list[[3]],
      filter(dat, row == 2),
      filter(dat, row == 3)
    )

    result <- suppressMessages(create_table_list(dat_list, 3, 3, "row"))

    expect_equal(result, expected)

  })


test_that(
  "create_table_list fails gracefully if the identified row is not in the
  data", {

    dat <- data.frame(
      "row" = c(1, 1, 1, 2, 2, 2),
      "col" = c(1, 2, 3, 1, 2, 3),
      "address" = c("A1", "B1", "C1",
                    "A2", "B2", "C2"),
      "character"= c("table 1", "value", NA,
                     "table 2", "value", NA),
      "numeric" = c(NA, NA, 1, NA, NA, 2)
    )

    expect_error(
      suppressMessages(create_table_list(list(dat), 1, 3, "row")),
      "The split_loc is higher than any available row. Please contact a developer."
    )


})
