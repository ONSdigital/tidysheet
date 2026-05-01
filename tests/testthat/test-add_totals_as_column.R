dat <- data.frame(
  item = c("1st", "2nd", "total A", "1st", "total B"),
  numeric = c(1, 2, 3, 5, 5)
)


test_that("add_totals_as_column returns dat if all require variables are NA", {

    expect_equal(
      add_totals_as_column(dat, c("character", "numeric"), NA, NA, NA),
      dat
      )
  })


test_that(
  "add_totals_as_column creates a new column to hold the words following
  'total' for all rows above and including the row stating the total.
  direction default is 'up'", {

    expected <- dat %>% mutate(
      description_1 = c("A", "A", "A", "B", "B")
      )

    result <- suppressMessages( add_totals_as_column(
      dat, c("character", "numeric"), "description_1", "(?i)ITEM", NA
    ))

    expect_equal(result, expected)

  })



test_that(
  "add_totals_as_column works when direction is specified as 'up'", {

    expected <- dat %>% mutate(
      description_1 = c("A", "A", "A", "B", "B")
    )

    result <- suppressMessages( add_totals_as_column(
      dat, c("character", "numeric"), "description_1", "(?i)ITEM", "up"
    ))

    expect_equal(result, expected)

  })


test_that(
  "add_totals_as_column works when direction is specified as 'down' and blanks
  are filled with item wording", {

    expected <- dat %>% mutate(
      description_1 = c("1st", "2nd", "A", "A", "B")
    )

    result <- suppressMessages( add_totals_as_column(
      dat, c("character", "numeric"), "description_1", "(?i)ITEM", "down"
    ))

    expect_equal(result, expected)

  })


test_that(
  "add_totals_as_column fails gracefully if one of from_pattern or totals_col
  variables are NA", {

    expect_error(
      suppressMessages(add_totals_as_column(
        dat, c("character", "numeric"), NA, "(?i)ITEM", "down"
      )),
      "has not been supplied"
    )

    expect_error(
      suppressMessages(add_totals_as_column(
        dat, c("character", "numeric"), "description_1", NA, "down"
      )),
      "has not been supplied"
    )

  })


test_that(
  "add_totals_as_column uses the first header column if the from column is not
  found but produces a warning", {

    expected <- dat %>% mutate(
      description_1 = c("A", "A", "A", "B", "B")
    )

    expect_warning(
      result <- suppressMessages(
        add_totals_as_column(
          dat, c("character", "numeric"), "description_1", "ITEM", "up"
        )
      ),
      "used by default: 'item'"
    )

    expect_equal(result, expected)


  })


test_that(
  "add_totals_as_column does not look in columns with names listed in
  xlsx_cells_names when looking for match to 'from_pattern'", {

    dat_renamed <- rename(dat, num = item)

    expected <- dat_renamed %>% mutate(
      description_1 = c("A", "A", "A", "B", "B")
    )

    expect_no_warning(
      result <- suppressMessages(
        add_totals_as_column(
          dat_renamed, c("character", "numeric"), "description_1", "num", "up"
        )
      )
    )

    expect_equal(result, expected)

  })
