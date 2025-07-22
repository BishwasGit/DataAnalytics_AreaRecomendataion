# Quick test to check crime data structure
library(tidyverse)
library(readr)

setwd("C:/Users/Smriti Khanal/Documents/South & West Yorkshire")

# Load crime datasets
tryCatch({
  crime_south <- read_csv("2025-04-south-yorkshire-street.csv")
  cat("South Yorkshire crime data loaded successfully\n")
  cat("Columns:", paste(colnames(crime_south), collapse = ", "), "\n")
  cat("First few rows:\n")
  print(head(crime_south, 3))
}, error = function(e) {
  cat("Error loading South Yorkshire crime data:", e$message, "\n")
})

tryCatch({
  crime_west <- read_csv("2025-04-west-yorkshire-street.csv")
  cat("\nWest Yorkshire crime data loaded successfully\n")
  cat("Columns:", paste(colnames(crime_west), collapse = ", "), "\n")
  cat("First few rows:\n")
  print(head(crime_west, 3))
}, error = function(e) {
  cat("Error loading West Yorkshire crime data:", e$message, "\n")
})
