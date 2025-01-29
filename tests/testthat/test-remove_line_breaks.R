context("remove_line_breaks")

test_that("remove_line_breaks removes line breaks", {
  
  # Testing different cases of line break, carriage return placements and effect on numerical columns
  test_dataset <- tibble::tibble(
    "Service"  <- c("Education\n Services","Education Services", "Education\n\n Services","\nEducation\n Services\n"),
    "Transaction" <- c("Expenditure\n", "\nExpenditure", "\nExpenditure\n", "Expenditure"),
    "Units" <- c("thousands\n","\rthousands","hundreds\r", "millions\r\r"),
    "Numbers" <-c(1,2,3,4),)
  
  cleaned_dataset <- remove_line_breaks(test_dataset)
  
  result_dataset <- tibble::tibble(
    "Service"  <- c("Education Services","Education Services", "Education Services","Education Services"),
    "Transaction" <- c("Expenditure", "Expenditure", "Expenditure", "Expenditure"),
    "Units" <- c("thousands","thousands","hundreds", "millions"),
    "Numbers" <-c(1,2,3,4),)
  
  expect_identical(result_dataset, cleaned_dataset)
  
})