context("split_original_header")

test_that("split_original_header returns expected output", {
  
  dat <- data.frame(
    "original_header" = c(paste0("Education:\n secondary_education\r\n ", enc2utf8("\u00A3"), " thousand"),
                          paste0("Education\n ", enc2utf8("\u00A3"), " thousand"),
                          "Education\n   (primary)",
                          "Education - no split (negative test)"
    )
  )
  
  
  three_columns <- c("service", "units", "subservice")
  two_split_points <- c( "\\:\\s*(\r\n|\r|\n)", 
                         paste0("(\r\n|\r|\n)\\s*\\", enc2utf8("\u00A3"))
  )
  three_expected <- dat %>%
    mutate(
      original_header = tolower(original_header),
      service = c(
        "education",
        "education",
        "education (primary)",
        "education - no split (negative test)"
      ),
      subservice = c(
        "secondary_education",
        "education",
        "education (primary)",
        "education - no split (negative test)"
      ),
      units = c(
        "thousand",
        "thousand",
        NA,
        NA
      )
    )
  
  two_columns <- c("service", "subservice")
  one_split_point <- c( "(\r\n|\r|\n)\\s*\\(")
  two_expected <- dat %>% 
    mutate(
      original_header = tolower(original_header),
      service = c(
        paste0("education:\n secondary_education\r\n ", enc2utf8("\u00A3"), " thousand"),
        paste0("education\n ", enc2utf8("\u00A3"), " thousand"),
        "education",
        "education - no split (negative test)"
      ),
      subservice = as.character(c(
        NA,
        NA,
        "(primary)",
        NA
      ))
    )
  
  
  three_result <- split_original_header(dat, three_columns, two_split_points, "original_header")
  two_result <- split_original_header(dat, two_columns, one_split_point, "original_header")
  
  expect_equal(three_result, three_expected)
  expect_equal(two_result, two_expected)
  
})
