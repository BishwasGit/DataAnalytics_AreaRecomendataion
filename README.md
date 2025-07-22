# Area Recommendation Analysis - Project Organization Guide

Based on your `index.R` analysis, here's what should go in each folder:

## 📂 OBTAINED DATA
**Purpose:** Store all raw, unprocessed datasets as obtained from original sources

### Current Files to Move Here:
- `price_paid_data.csv` ✅ (Already moved)

### Additional Raw Data Files (when available):
- `201805_fixed_pc_performance_r03.csv` (Broadband performance data)
- `201809_fixed_pc_coverage_r01.csv` (Broadband coverage data)
- `2025-04-south-yorkshire-street.csv` (South Yorkshire crime data)
- `2025-04-west-yorkshire-street.csv` (West Yorkshire crime data)
- `Population2011_1656567141570.csv` (Population census data)
- `2018-2019_england_ks5final-(1).csv` (School performance data)
- `Postcode to LSOA 1.csv` (Geographic mapping data)

### Data Source Documentation:
Create a file `data_sources.txt` with:
- UK Government Price Paid Data (property transactions)
- Ofcom Fixed Broadband Performance Data
- Police Crime Data (South & West Yorkshire)
- Department for Education School Performance Data
- ONS Population Data 2011
- ONS Postcode to LSOA mapping

---

## 🧹 CLEANED DATA
**Purpose:** Store processed, cleaned, and transformed datasets ready for analysis

### Files to Create Here:
- `house_price_summary.csv` (Cleaned house prices by postcode)
- `broadband_summary.csv` (Cleaned broadband speeds by postcode)
- `crime_summary.csv` (Crime counts by area)
- `school_summary.csv` (School data by postcode)
- `population_summary.csv` (Population data by postcode)
- `combined_data.csv` (All datasets joined by postcode)
- `final_data.csv` (Complete dataset with all features)
- `modeling_dataset.csv` (Data prepared for statistical analysis)

### Data Quality Reports:
- `data_coverage_summary.csv` (Coverage statistics for each dataset)
- `data_quality_report.txt` (Issues found and resolution steps)

---

## 💻 CODE
**Purpose:** All R scripts and programming files

### Current Files Moved Here:
- `index.R` ✅ (Main analysis script)
- `check_crime.R` ✅ (Crime data validation)
- `check_school.R` ✅ (School data validation)
- `examine_data.R` ✅ (Data exploration)
- `test_fix.R` ✅ (Bug fixes and testing)
- `test_school.R` ✅ (School data testing)
- `test.R` ✅ (General testing)

### Additional Code Files to Consider:
- `data_cleaning.R` (Separate data cleaning functions)
- `database_setup.R` (MySQL database setup scripts)
- `visualization_functions.R` (Custom plotting functions)
- `recommendation_engine.R` (Scoring algorithm)
- `linear_models.R` (Statistical modeling code)
- `utils.R` (Helper functions)

### Database Scripts:
- `setup_database.sql` (MySQL table creation)
- `database_config.R` (Connection settings)

---

## 📊 GRAPHS
**Purpose:** All visualization outputs (PNG, PDF files)

### House Price Visualizations:
- `house_prices_line_2021_2024.png`
- `house_prices_bar_2023.png`
- `house_prices_boxplot_south.png`
- `house_prices_boxplot_west.png`
- `house_price_distribution.png`

### Broadband Speed Visualizations:
- `broadband_boxplot_south.png`
- `broadband_boxplot_west.png`
- `broadband_bar_south.png`
- `broadband_bar_west.png`
- `broadband_speed_distribution.png`

### Crime Rate Visualizations:
- `crime_drug_boxplot_south.png`
- `crime_drug_boxplot_west.png`
- `vehicle_crime_radar_south.png`
- `robbery_pie_west.png`
- `drug_offense_trend.png`

### School Performance Visualizations:
- `school_attainment_south_2022.png`
- `school_attainment_west_2022.png`
- `school_attainment_trend.png`

### Statistical Analysis Graphs:
- `linear_model_price_vs_speed.png`
- `linear_model_attainment_vs_price.png`
- `linear_model_attainment_vs_drugs.png`
- `linear_model_speed_vs_drugs.png`
- `linear_model_speed_vs_attainment.png`
- `comprehensive_correlation_matrix.png`
- `correlation_heatmap.png`

### Summary Visualizations:
- `top_20_recommendations.png`
- `score_breakdown_top10.png`
- `geographic_scores.png` (if coordinates available)
- `population_distribution.png`

---

## 🎯 RECOMMENDATION SYSTEM
**Purpose:** Recommendation algorithm outputs and scoring results

### Core Recommendation Files:
- `area_recommendations.csv` (Complete ranked list with scores)
- `top_10_recommendations.csv` (Top 10 areas with basic info)
- `top_10_recommendations_with_addresses.csv` (Top 10 with location details)
- `top_3_recommendations_with_addresses.csv` (Final report - top 3 only)

### Scoring Components:
- `price_scores.csv` (House price scoring breakdown)
- `speed_scores.csv` (Broadband speed scoring breakdown)
- `crime_scores.csv` (Crime rate scoring breakdown)
- `school_scores.csv` (School quality scoring breakdown)
- `population_scores.csv` (Population density scoring breakdown)

### Algorithm Documentation:
- `scoring_methodology.md` (Explanation of scoring algorithm)
- `weights_justification.md` (Why 30% price, 25% broadband, 25% crime, etc.)
- `recommendation_criteria.md` (Selection criteria documentation)

### Validation Files:
- `model_validation.csv` (Cross-validation results)
- `sensitivity_analysis.csv` (How results change with different weights)

---

## 📋 REPORT
**Purpose:** Final documentation, presentations, and summary reports

### Executive Summary:
- `executive_summary.pdf` (1-2 page overview)
- `key_findings.md` (Main insights and recommendations)

### Technical Reports:
- `technical_methodology.pdf` (Detailed analysis approach)
- `data_sources_methodology.pdf` (How data was collected and processed)
- `statistical_analysis_report.pdf` (Linear models and correlations)

### Final Recommendations:
- `final_top_3_recommendations.pdf` (Detailed report on top 3 areas)
- `area_profiles.pdf` (Detailed profiles of recommended areas)

### Presentations:
- `project_presentation.pptx` (Slide deck for stakeholders)
- `technical_presentation.pptx` (For technical audience)

### Supporting Documentation:
- `limitations_and_assumptions.md` (Project limitations)
- `future_improvements.md` (Suggestions for enhancement)
- `references.md` (Data sources and methodology references)

### Quality Assurance:
- `validation_results.md` (How recommendations were validated)
- `peer_review_feedback.md` (If applicable)

---

## 🚀 Next Steps to Organize Your Project:

1. **Move Raw Data Files:**
   ```powershell
   # Move all CSV files to OBTAINED DATA (except processed ones)
   Move-Item "path\to\raw\*.csv" "c:\Users\User_Pc\OBTAINED DATA\"
   ```

2. **Generate Cleaned Data:**
   ```r
   # Run sections of index.R to generate cleaned datasets
   source("CODE/index.R")
   ```

3. **Create Visualizations:**
   ```r
   # Run visualization sections to generate all graphs
   # All .png files will be created in current directory
   # Then move to GRAPHS folder
   ```

4. **Extract Recommendation Results:**
   ```r
   # Run recommendation system sections
   # Generate CSV files with rankings and scores
   ```

5. **Compile Reports:**
   - Create summary documents
   - Generate final presentation
   - Document methodology

## 📊 Database Organization (MySQL):
Your project includes database integration with these tables:
- `areas` (Geographic information)
- `house_prices` (Property transaction data)
- `broadband_performance` (Internet speed data)
- `crime_incidents` (Crime statistics)
- `schools` (Educational facility data)
- `population_data` (Demographic information)

## 🎯 Project Objectives Achieved:
✅ Multi-criteria area recommendation system  
✅ Data integration from multiple government sources  
✅ Statistical analysis with linear modeling  
✅ Comprehensive visualization suite  
✅ Database-ready normalized data structure  
✅ Weighted scoring algorithm (Price 30%, Broadband 25%, Crime 25%, Schools 10%, Population 10%)

This organization will help you maintain a clean, professional project structure suitable for academic or professional presentation.
