# Complex table layout.
# The first split has to be vertical, so the first complete table is table c
# The second split is horizontal, and tables are returned top to bottom so
# the second table is table a and the thord is table b.

#   # 1 #     | # 2 # | # 3 #  | # 4 #  |
#   #---------|-------|--------|--------|
# 1 # table a |       |
# 2 # x       | value |
# 3 # a1      | 1     |-----------------|
# 4 # a2      |       | table c |       |
# 5 #-----------------|                 |
# 6 #         |       | y       | value |
# 7 # table b |       | c1      | 3     |
# 8 # z       | value |-----------------|
# 9 # b1      | 2     |

dat <- data.frame(
  row = c(1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6, 7, 7, 8, 8),
  col = c(1, 1, 2, 1, 2, 1, 2, 3, 3, 4, 1, 3, 4,  1, 2,  1, 2),
  data_type = c(rep("character", 4), "numeric", "character", "numeric",
                rep("character", 5), "numeric", rep("character", 3), "numeric"),
  character = c(
    "table a",
    "x", "value",
    "a1", NA,
    "a2", NA, "table c",
    "y", "value",
    "table b", "c1", NA,
    "z", "value",
    "b1", NA
  ),
  numeric = c(
    rep(NA, 4), 1, NA, 1, rep(NA, 5), 3, rep(NA, 3), 2
  )
)

table_a <- filter(dat, row < 5 & col < 3)
table_b <- filter(dat, row > 4 & col < 3)
table_c <- filter(dat, col > 2)


test_that(
  "get_tables_as_list returns a list of tables where no row is in more than one
  table. One vertical split, One horizontal split", {

    expected <- list(table_c, table_a, table_b)

    result <- suppressMessages(
      get_tables_as_list(
        dat, c("col", "row"), c(1, 2), c("table c", "table b"), NA, NA, NA
      )
    )

    expect_equal(result, expected)

})


test_that(
  "get_tables_as_list raises an error if one but not all required vars are not NA", {

    expect_error(
      suppressMessages(
        get_tables_as_list(
          dat, NA, c(1, 2), c("table c", "table b"), NA, NA, c(3, 5)
        )
      ),
      "At least one, but not all"
    )

    expect_error(
      suppressMessages(
        get_tables_as_list(
          dat, c("col", "row"), NA, c("table c", "table b"), NA, NA, c(3, 5)
        )
      ),
      "At least one, but not all"
    )

    expect_error(
      suppressMessages(
        get_tables_as_list(
          dat, c("col", "row"), c(1, 2), NA, NA, NA, c(3, 5)
        )
      ),
      "At least one, but not all"
    )

  })


test_that(
  "get_tables_as_list raises an error if directions, table_ids, or patterns differ in length", {

    expect_error(
      suppressMessages(
        get_tables_as_list(
          dat, "row", c(1, 2), c("table c", "table b"), NA, NA, c(3, 5)
        )
      ),
      "must have an equal number of elements"
    )

    expect_error(
      suppressMessages(
        get_tables_as_list(
          dat, c("col", "row"), 1, c("table c", "table b"), NA, NA, c(3, 5)
        )
      ),
      "must have an equal number of elements"
    )

    expect_error(
      suppressMessages(
        get_tables_as_list(
          dat, c("col", "row"), c(1, 2), "table c", NA, NA, c(3, 5)
        )
      ),
      "must have an equal number of elements"
    )

  })


test_that(
  "get_tables_as_list raises an error if patterns and pattern_instances differ in length", {

    expect_error(
      suppressMessages(
        get_tables_as_list(
          dat, c("col", "row"), c(1, 2), c("table c", "table b"), 2, NA, c(3, 5)
        )
      ),
      "The same number of integers must be given for"
    )
  })


test_that(
  "get_tables_as_list raises an error if patterns and pattern_offsets differ in length", {

    expect_error(
      suppressMessages(
        get_tables_as_list(
          dat, c("col", "row"), c(1, 2), c("table c", "table b"), c(2, 2), 1, c(3, 5)
        )
      ),
      "The same number of integers must be given for .* table_split_pattern_offsets"
    )
  })


test_that(
  "get_tables_as_list only returns the selected tables", {

    expected <- list(table_c, table_b)

    result <- suppressMessages(
      get_tables_as_list(
        dat, c("col", "row"), c(1, 2), c("table c", "table b"), NA, NA, c(3, 5)
      )
    )

    expect_equal(result, expected)

  })


test_that(
  "get_tables_as_list returns the original table as a list of one table if
  settings dont ask for a split", {

    result <- suppressMessages(
      get_tables_as_list(dat, NA, NA, NA, NA, NA, NA)
    )

    expect_equal(result, list(dat))

  })
