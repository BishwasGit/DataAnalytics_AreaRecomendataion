library(tidyverse)
library(readr)

cat("Setting working directory...\n")
setwd("C:/Users/Smriti Khanal/Documents/South & West Yorkshire")

cat("Loading datasets...\n")

# Load and examine each dataset
tryCatch({
  broadband_perf <- read_csv("201805_fixed_pc_performance_r03.csv")
  cat("Broadband performance data loaded. Columns:", paste(colnames(broadband_perf)[1:5], collapse = ", "), "...\n")
}, error = function(e) cat("Error loading broadband performance:", e$message, "\n"))

tryCatch({
  house_price <- read_csv("price_paid_data.csv")
  cat("House price data loaded. Columns:", paste(colnames(house_price)[1:5], collapse = ", "), "...\n")
}, error = function(e) cat("Error loading house price:", e$message, "\n"))

tryCatch({
  crime_south <- read_csv("2025-04-south-yorkshire-street.csv")
  cat("South Yorkshire crime data loaded. Columns:", paste(colnames(crime_south), collapse = ", "), "\n")
  cat("Sample data:\n")
  print(head(crime_south, 2))
}, error = function(e) cat("Error loading South Yorkshire crime:", e$message, "\n"))

tryCatch({
  crime_west <- read_csv("2025-04-west-yorkshire-street.csv")
  cat("West Yorkshire crime data loaded. Columns:", paste(colnames(crime_west), collapse = ", "), "\n")
}, error = function(e) cat("Error loading West Yorkshire crime:", e$message, "\n"))

tryCatch({
  population <- read_csv("Population2011_1656567141570.csv")
  cat("Population data loaded. Columns:", paste(colnames(population)[1:5], collapse = ", "), "...\n")
}, error = function(e) cat("Error loading population:", e$message, "\n"))

cat("Data examination complete.\n")
