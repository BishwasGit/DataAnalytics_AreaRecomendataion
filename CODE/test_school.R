library(readr)
library(tidyverse)

# Try different approaches to read the school data
cat("Attempting to read school dataset...\n")

# Method 1: Try as regular CSV
tryCatch({
  school <- read_csv("C:/Users/User_Pc/Documents/2018-2019_england_ks5final-(1).csv")
  cat("Method 1 - CSV successful. Columns found:\n")
  print(colnames(school)[1:20])  # Show first 20 columns
  cat("Total columns:", ncol(school), "\n")
  cat("Total rows:", nrow(school), "\n")
  
  # Look for postcode and school name columns
  postcode_cols <- grep("PCODE|postcode|Postcode", colnames(school), ignore.case = TRUE)
  school_cols <- grep("SCHNAME|School|school", colnames(school), ignore.case = TRUE)
  town_cols <- grep("TOWN|Town|town", colnames(school), ignore.case = TRUE)
  progress_cols <- grep("PROGRESS|progress", colnames(school), ignore.case = TRUE)
  
  cat("Postcode columns:", colnames(school)[postcode_cols], "\n")
  cat("School name columns:", colnames(school)[school_cols], "\n") 
  cat("Town columns:", colnames(school)[town_cols], "\n")
  cat("Progress columns:", colnames(school)[progress_cols], "\n")
  
}, error = function(e) {
  cat("Method 1 failed:", e$message, "\n")
  
  # Method 2: Try reading first few lines manually
  cat("Method 2 - Manual parsing...\n")
  lines <- readLines("C:/Users/User_Pc/Documents/2018-2019_england_ks5final-(1).csv", n = 5)
  cat("First line (header):\n", substr(lines[1], 1, 200), "...\n")
  cat("Second line (data):\n", substr(lines[2], 1, 200), "...\n")
})
