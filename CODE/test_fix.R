# Test script to verify the R code fixes
library(tidyverse)
library(readr)

# Set working directory to match the main script
setwd("C:/Users/Smriti Khanal/Documents/South & West Yorkshire")

cat("Testing the improved R script...\n")

# Test just the critical parts to verify they work
tryCatch({
  # Test house price data loading
  house_price <- read_csv("price_paid_data.csv", show_col_types = FALSE)
  cat("✓ House price data loaded successfully\n")
  
  # Test column assignment
  colnames(house_price)[c(4, 2, 5, 6, 7, 10, 15)] <- c("postcode", "price", "property_type", "new_build", "tenure", "street", "county")
  cat("✓ Column assignment completed\n")
  
  # Test data filtering
  house_price_clean <- house_price %>%
    mutate(price = as.numeric(price)) %>%
    filter(!is.na(postcode), !is.na(price), 
           str_detect(postcode, "^[A-Z]{1,2}[0-9][A-Z0-9]?\\s?[0-9][A-Z]{2}$")) %>%
    mutate(postcode = str_trim(str_to_upper(postcode)))
  
  cat("✓ House price data cleaning successful\n")
  cat("Valid postcodes found:", nrow(house_price_clean), "\n")
  cat("Sample prices:", paste(head(house_price_clean$price, 5), collapse = ", "), "\n")
  cat("Sample postcodes:", paste(head(house_price_clean$postcode, 5), collapse = ", "), "\n")
  
  # Test LSOA mapping
  postcode_lsoa <- read_csv("LSOA_POST_CODE_MAPPING.csv", show_col_types = FALSE)
  cat("✓ LSOA mapping data loaded successfully\n")
  cat("LSOA columns available:", paste(colnames(postcode_lsoa)[1:10], collapse = ", "), "...\n")
  
  cat("\nAll tests passed! The fixes should work correctly.\n")
  
}, error = function(e) {
  cat("Error in test:", e$message, "\n")
})
