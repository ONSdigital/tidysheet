test_that("reformat_tidy_data returns expeccted output", {
  dat <- data.frame(
    sheet = "sheet 1",
    address = c("A1", "B1", "C1",
                "A2", "B2", "C2",
                "A3", "B3", "C3"),
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    is_blank = FALSE,
    data_type = c(rep("character", 5),
                  "numeric",
                  rep("character", 2),
                  "numeric"),
    numeric = c(rep(NA, 5), 1, NA, NA, 2),
    character = c("name", "sound", "value",
                  "cat", "meow", NA,
                  "dog", "woof", NA)
  )

  expected <- tibble(
    row = 2:3,
    is_blank = FALSE,
    sheet = "sheet 1",
    name = c("cat", "dog"),
    sound = c("meow", "woof"),
    numeric = 1:2,
    description = "value"
  )

  result <- suppressMessages(
    reformat_tidy_data(dat, columns_to_create = "description")
  )

  expect_equal(result, expected)
})

