context("split_table")

sample_data <- read.csv("D:\\coding_repos\\pub_sec\\R_test_data\\multi_table_tidyxl.csv", encoding = "UTF-8")
expected_split_1 <- read.csv("D:\\coding_repos\\pub_sec\\R_test_data\\multi_table_split_1.csv")
expected_2nd_split_1 <- read.csv("D:\\coding_repos\\pub_sec\\R_test_data\\multi_table_2nd_split_1.csv")
expected_2nd_split_2 <- read.csv("D:\\coding_repos\\pub_sec\\R_test_data\\multi_table_2nd_split_2.csv")
expected_split_3 <- read.csv("D:\\coding_repos\\pub_sec\\R_test_data\\multi_table_split_3.csv")

test_that('split_table gives expected output', {
  
  # create patterns to look for to know where to split the table
  split <- split_table(sample_data, "^Part") # starts with "Part"
  second_split <- split_table(sample_data, "^[0-9]\\.\\s") # starts with a number followed by a space
  
  # check the high level table looks as expected 
  # (the split tables are held as tibbles within the column called 'cells')
  expect_equal(nrow(split), 3)
  expect_equal(split$character, c("Part 1: Comprehensive Income and Expenditure",
                                  "Part 2: Service Breakdown",
                                  "Part 3: Memorandum Items"))
  
  expect_equal(nrow(second_split), 2)
  expect_equal(second_split$character, c("1. Education",
                                         "2. Culture and Related Services"))
  
  # check that the split tables look as expected 
  split_1 <- split[["cells"]][[1]]
  split_3 <- split[["cells"]][[3]]  
  second_split_1 <- second_split[["cells"]][[1]]
  second_split_2 <- second_split[["cells"]][[2]]
  
  expect_equal(tibble(expected_split_1), split_1)
  expect_equal(tibble(expected_split_3), split_3)
  expect_equal(tibble(expected_2nd_split_1), second_split_1)
  expect_equal(tibble(expected_2nd_split_2), second_split_2)
  
})
