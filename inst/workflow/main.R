print('Hello from R')
my_args <- commandArgs(trailingOnly = TRUE)
print(my_args)

# Execute the command
my_path <- "D:/coding_repos/pub_sec/pub_sec/preprocessing/revenue_expenditure.r"
source(my_path)

print('Goodbye from R')
