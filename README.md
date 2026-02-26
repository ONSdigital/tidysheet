# tidysheet
## Warning: this project is still in development and is not fully functionally tested

tidysheet takes messy Excel data and converts it to tidy data with a single 
value per row, and gives users the ability to control the number and content of 
descriptor columns. It is resilient to minor changes in schema and the location of 
data, and settings can be easily updated to account for larger changes. It can 
handle multiple tables in a sheet, and can add metadata from 
above tables to descriptor columns (e.g. year, units, and vintage).

This package has been designed for non-coders to be able to just specify
settings to wrangle data, without the need for any coding. See the example below
for what these settings look like, and how you can run it from R or Python.

Full documentation on all settings are given in the [Wiki](https://github.com/ONSdigital/tidysheet_public/wiki) (still in progress).

## Installation
To run the code via python you can install the package locally by running the 
following in R:
```r
# Install from local
renv::restore()
install.packages("D:/coding_repos/tidysheet_1.0.tar.gz", type = "source")
```
Alternatively, to run in R simply use `devtools::load_all()` at the start of
your session.
  
## Example
This example dataset has two rows of headings and some non-unique row 
names that refer to the row name above. This example is to demonstrate the simplicity 
of editing settings: It covers only a small proportion of the functionality of tidysheet.
  
### Input data
<img width="837" height="325" alt="image" src="https://github.com/user-attachments/assets/4a596a5c-152d-4c17-8064-0d07e1eaa0ba" />

### Output data
Some irrelevant columns such as supplier, source, dataset, and units have been
removed for demonstration purposes.
<img width="1071" height="431" alt="image" src="https://github.com/user-attachments/assets/abd22a34-6381-4e4b-b20e-acd1d59fb977" />
  
### In R:
To tidy the example input above and get the output shown, you would run the following in R:
```
# users to edit this:
settings <- "{
  header_identifier: (?i)group,
  left_headers: description_1,
  columns_to_create: description_2, year,
  extend_row_pattern: (?i)of\\s*which,
  extend_row_order: reverse,
  extend_row_with: above,
  single_vintage: final
  }"

filepath <- file.path(test_path("testdata"), "examples.xlsx")

# single line run to tidy any dataset regardless of which settings are used:
tidy_sheet(c("--args", filepath, "demo", NA, settings, "1"), to_csv = FALSE)
```

### In Python:
The tidy_sheet function is designed to run via python, which is why settings
are given as a single string in the R example above. There is a little more set 
up required by developers to run it via python, but because settings are in e.g. 
a toml, this can be easily edited by users via a user interface. Note that 
it could also be run via R using a toml for the settings.

For the example above, the following settings would be given in e.g. a toml file:
```
[settings]
  header_identifier = "(?i)group"
  left_headers = "description_1"
  columns_to_create = ["description_2", "year"]
  extend_row_pattern = ["(?i)of\\s*which"]
  extend_row_order = "reverse"
  extend_row_with = "above"
  single_vintage = "final"
```
An R run-file would need to be created and saved in e.g
"D:\\tidy_sheet_run_file.r":
```
# messages and warnings in the order they occur
options(warn = 1)
# load the tidysheet package
library(tidysheet)
# Get the values passed from subprocess.call in Python
arg_values <- commandArgs(trailingOnly = TRUE)
# Process data
tidysheet::tidy_sheet(arg_values)
```

And something along the lines of the following code would be in python 
(note that this is a non-working example):
```
RSCRIPT_PATH = "C:\\My_RStudio\\R-4.4.1\\bin\\Rscript.exe"
settings = mydict.get("settings")
settings_json = json.dumps(settings) 
input_filepath = "D:\\readme_example.xlsx"
output_filepath = "D:\\readme_example_output.csv"
sheet_pattern = "demo"
run_file = "D:\\tidy_sheet_run_file.r"

process = run_subprocess(
  RSCRIPT_PATH, run_file, "--args", 
  input_filepath, sheet_pattern, output_filepath, settings_json, 1
)

# To get the logs from R you can uset he following, and process that text in 
# whatever way best fits with you Python logging:
stdout.splitlines(process)
```
## Contributing
These notes are in progress.

Please read and follow our Code of Conduct to ensure a welcoming environment for
all contributors.

If you find a bug or have a feature request, please open an issue and provide as
much detail as possible.

### creating documentation
Follow the layout of the roxygen documentation in existing functions. When you
have added a new function with it's docstring, run `devtools::document()`. If
your function uses a package that is not already used in tidysheet, run 
`usethis::use_import_from(package, function)`.

### testing and coverage
Write unit tests using testthat.  
  
Run unit tests using `devtools::test()`, or click test in the build panel of
RStudio. All tests must pass before review.  
  
To run test coverage, use `devtools::test_coverage()` you can see which lines
remain untested in the view panel of RStudio.    
Note that if you receive an error stating that the file cannot be found, you may 
need to restart your session. (use `.rs.restartR()`)  
Messages do not have to be tested, but please aim for at least 90% coverage
before opening a merge request.

### package checks
Check changes to the package conforms to best practice by running
`devtools::check()`


