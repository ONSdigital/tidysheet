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
