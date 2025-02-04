install.packages("renv")
renv::init()

usethis::use_test("main.R")
testthat::test_file("tests/testthat/test-main.R")


install.packages("reticulate")
library(reticulate)

virtualenv_create(envname = "D:/my_virtual_envs/tidysheetenv")
use_virtualenv("D:/my_virtual_envs/tidysheetenv")
install.packages("roxygen2")
roxygen2::roxygenise()
install.packages("usethis")
install.packages("testthat")
usethis::use_testthat()
usethis::use_test("hello.R")
usethis::use_test("create_table_list.R")
library(tidysheet)
?tidysheet::create_table_list
testthat::test_file("tests/testthat/test-create_table_list.R")
install.packages("covr")
library(covr)

install.packages("devtools")
library(devtools)
devtools::build_vignettes()
library(tidysheet)
usethis::use_vignette("tidysheet-introduction")
devtools::build_vignettes()
vignette(package = "tidysheet")

devtools::clean_vignettes()
devtools::build_vignettes()

usethis::use_build_ignore(files = ".github/CODEOWNERS")

detach("package:tidysheet", unload=TRUE)
covr::package_coverage(path = ".",
                       type = "all",
                       combine_types = FALSE,
                       relative_path = TRUE,
                       quiet = TRUE,
                       clean = TRUE,
                       line_exclusions = NULL,
                       function_exclusions = NULL,
                       pre_clean = TRUE,
                       vignettes = TRUE)




#tidysheet Tests Coverage: 100.00%
#R/hello.R: 100.00%

#tidysheet Vignettes Coverage: 100.00%
#R/hello.R: 100.00%

#tidysheet Examples Coverage: 100.00%
#R/hello.R: 100.00%

#cmd:
#pip install pre-commit


#git setup cmds:
#git remote add origin https://github.com/ONSdigital/tidysheet
#git remote -v
#git add .


# Adding srcipts from original code:
script_names <- c("setup_utils.r", "preprocessing_utils.r", "formatting.r", "formatting_multi_table.r")

library(tidysheet)
devtools::check()

# Open a connection to a text file
sink("check_output.txt")

# Run devtools::check()
devtools::check()

# Close the connection
sink()




