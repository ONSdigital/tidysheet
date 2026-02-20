test_that("remove_line_breaks removes line breaks regardless of placement, and has no impact on numeric columns", {

  dat <- tibble(
    "Service"  = c(
      "Education\n Services",
      "Education Services",
      "Education\n\n Services",
      "\nEducation\n Services\n"
      ),
    "Transaction" = c(
      "Expenditure\n",
      "\nExpenditure",
      "\nExpenditure\n",
      "Expenditure"
      ),
    "Units" = c("thousands\n","\rthousands","hundreds\r", "millions\r\r"),
    "Numbers" = 1:4
    )

  expected <- tibble(
    "Service"  = "Education Services",
    "Transaction" = "Expenditure",
    "Units" = c("thousands","thousands","hundreds", "millions"),
    "Numbers" = 1:4
    )

  result <- suppressMessages(remove_line_breaks(dat))
  expect_identical(result, expected)

})


test_that("remove_line_breaks returns single space between words", {

  dat <- tibble(
    "Service"  = c("word1\nword2", "word1\rword2", "word1\n\nword2"),
    "Numbers" = 1:3
    )

  expected <- tibble(
    "Service"  = "word1 word2",
    "Numbers" = 1:3
    )

  result <- suppressMessages(remove_line_breaks(dat))

  expect_identical(result, expected)

})
