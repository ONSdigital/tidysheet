test_that(
  "get_left_col_names works when left and right headers are the same row and
  there is only one header row and one left header. left_headers not supplied
  in settings", {

  dat <- data.frame(
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2),
    data_type = c(rep("character", 3), "numeric"),
    character = c("left", "right", "a", NA),
    address = c("A1", "B1", "A2", "B2")
  )

  result <- suppressMessages(get_left_col_names(dat, 2, NA, 2))
  expect_equal(result, "left")

})


test_that(
  "get_left_col_names works when left and right headers are the same row and
  there are two left headers. left_headers not supplied in settings", {

    dat <- data.frame(
      row = c(1, 1, 1, 2, 2, 2),
      col = c(1, 2, 3, 1, 2, 3),
      data_type = c(rep("character", 5), "numeric"),
      character = c("left 1", "left 2", "right", "a", "b", NA),
      address = c("A1", "B1", "C1", "A2", "B2", "C2")
    )

    result <- suppressMessages(get_left_col_names(dat, 3, NA, 2))
    expect_equal(result, c("left 1", "left 2"))

  })


test_that(
  "get_left_col_names works when left headers are in the same row as the first
  row as the first row of right headers. left_headers not supplied in settings", {

    dat <- data.frame(
      row = c(1, 1, 2, 2, 3, 3),
      col = c(1, 2, 1, 2, 1, 2),
      data_type = c(
        rep("character", 2), "blank", rep("character", 2), "numeric"
        ),
      character = c("left", "right 1", NA, "right 2", "a", NA),
      address = c("A1", "B1", "A2", "B2", "A3", "B3")
    )

    result <- suppressMessages(get_left_col_names(dat, 2, NA, 3))
    expect_equal(result, "left")

  })


test_that(
  "get_left_col_names works when left headers are in the same row as the first
  row as the last row of right headers. left_headers not supplied in settings", {

    dat <- data.frame(
      row = c(1, 1, 2, 2, 3, 3),
      col = c(1, 2, 1, 2, 1, 2),
      data_type = c("blank", rep("character", 4), "numeric"),
      character = c(NA, "right 1", "left", "right 2", "a", NA),
      address = c("A1", "B1", "A2", "B2", "A3", "B3")
    )

    result <- suppressMessages(get_left_col_names(dat, 2, NA, 3))
    expect_equal(result, "left")

  })


test_that(
  "get_left_col_names works when left headers are the row after the last row of
  right headers. left_headers not supplied in settings", {

    dat <- data.frame(
      row = c(1, 1, 2, 2, 3, 3),
      col = c(1, 2, 1, 2, 1, 2),
      data_type = c(
        "blank", rep("character", 2), "bank", "character", "numeric"
        ),
      character = c(NA, "right", "left", NA, "a", NA),
      address = c("A1", "B1", "A2", "B2", "A3", "B3")
    )

    result <- suppressMessages(get_left_col_names(dat, 2, NA, 3))
    expect_equal(result, "left")
  })


test_that(
  "get_left_col_names uses the names from the data if they are all present,
   even if the right number of left_headers are supplied in the settings", {

    dat <- data.frame(
      row = c(1, 1, 2, 2),
      col = c(1, 2, 1, 2),
      data_type = c(rep("character", 3), "numeric"),
      character = c("left", "right", "a", NA),
      address = c("A1", "B1", "A2", "B2")
    )

    result <- suppressMessages(
      get_left_col_names(dat, 2, "alt 1", 2)
      )
    expect_equal(result, "left")

  })


test_that(
  "get_left_col_names  uses the names from the data if ANY of them are present
   even if the right number of left_headers are supplied in the settings.
   Missing left headers are filled with column_(number)", {

     # 1st left header missing
     dat1 <- data.frame(
       row = c(1, 1, 1, 2, 2, 2),
       col = c(1, 2, 3, 1, 2, 3),
       data_type = c("blank", rep("character", 4), "numeric"),
       character = c(NA, "left 2", "right", "a", "b", NA),
       address = c("A1", "B1", "C1", "A2", "B2", "C2")
     )
     # 2nd left header missing
     dat2 <- data.frame(
       row = c(1, 1, 1, 2, 2, 2),
       col = c(1, 2, 3, 1, 2, 3),
       data_type = c("character", "blank", rep("character", 3), "numeric"),
       character = c("left 1", NA, "right", "a", "b", NA),
       address = c("A1", "B1", "C1", "A2", "B2", "C2")
     )

     result1 <- suppressMessages(
       get_left_col_names(dat1, 3, c("alt 1", "alt 2"), 2)
       )
     expect_equal(result1, c("column_1", "left 2"))

     result2 <- suppressMessages(
       get_left_col_names(dat2, 3, c("alt 1", "alt 2"), 2)
     )
     expect_equal(result2, c("left 1", "column_2"))

   })


test_that(
  "get_left_col_names  uses the names from the data if ANY of them are present
   even if the right number of left_headers are supplied in the settings.
   Missing left headers are filled with column_(number)", {

     dat <- data.frame(
       row = c(1, 1, 1, 2, 2, 2),
       col = c(1, 2, 3, 1, 2, 3),
       data_type = c(rep("blank", 2), rep("character", 3), "numeric"),
       character = c(NA, NA, "right", "a", "b", NA),
       address = c("A1", "B1", "C1", "A2", "B2", "C2")
     )

     result <- suppressMessages(
       get_left_col_names(dat, 3, c("alt 1", "alt 2"), 2)
     )
     expect_equal(result, c("alt 1", "alt 2"))

   })


test_that(
  "get_left_col_names gives a warning if all names from the data are blank but
   the wrong number of left_headers are supplied in the settings.
   Missing left headers are filled with column_(number)", {

     dat <- data.frame(
       row = c(1, 1, 1, 2, 2, 2),
       col = c(1, 2, 3, 1, 2, 3),
       data_type = c(rep("blank", 2), rep("character", 3), "numeric"),
       character = c(NA, NA, "right", "a", "b", NA),
       address = c("A1", "B1", "C1", "A2", "B2", "C2")
     )

     expect_warning(
       result_less <- suppressMessages(
         get_left_col_names(dat, 3, "alt 1", 2)
         ),
       "Fewer left_headers were supplied in the data dict than exist"
     )
     expect_equal(result_less, c("column_1", "column_2"))

     expect_warning(
       result_more <- suppressMessages(
         get_left_col_names(dat, 3, c("alt 1", "alt 2", "alt 3"), 2)
         ),
       "More left_headers were supplied in the data dict than have been found"
     )
     expect_equal(result_more, c("column_1", "column_2"))

   })


