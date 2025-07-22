library(readr)
library(tidyverse)

# Load the school dataset
school <- read_csv("C:/Users/User_Pc/Documents/2018-2019_england_ks5final-(1).csv")

# Check the column names
cat("School dataset columns:\n")
print(colnames(school))

cat("\nFirst few rows:\n")
print(head(school, 3))

cat("\nDataset dimensions:\n")
print(dim(school))
