# INF6027 – Death Metal Popularity Analysis

This repository contains R scripts and outputs for a statistical analysis project investigating **what musical features influence the popularity of death metal songs**. The project was developed as part of the INF6027 module in Data Science.

---

## Research Questions

1. **RQ1**: What musical characteristics are significantly associated with the popularity of death metal songs?
2. **RQ2**: Can we build a predictive model based on these characteristics to estimate song popularity?

---

## Methodology

The analysis was carried out in R and included the following steps:

- **Data preprocessing**: Missing values, outliers, standardization
- **Exploratory Data Analysis (EDA)**: Boxplots and correlation matrix
- **Dimensionality Reduction**: Principal Component Analysis (PCA)
- **Statistical Testing**: Chi-squared tests on categorical variables
- **Predictive Modeling**: Multiple linear regression

---

## Key Findings

- **Song duration**, **loudness**, and **speechiness** were found to be statistically significant predictors of popularity.
- Instrumentality showed significance in the chi-square test but not in regression, suggesting possible nonlinear effects.
- The final regression model was statistically significant, but with **limited explanatory power** (Adjusted R² = 0.045).
- Danceability, tempo, and key were not significant predictors in the regression model.

---

## How to Use the Code

1. Clone this repository or download the files.
2. Open `INF6027.R` in RStudio.
3. Ensure the dataset `death_metal_tracks.csv` is in your working directory.
4. Run the script step by step to reproduce the results:
   - Data cleaning
   - EDA (boxplots and correlation heatmap)
   - PCA and scree plot
   - Chi-square test
   - Linear regression

---

## Files in this Repo

| File / Folder | Description |
|---------------|-------------|
| `INF6027.R` | The main R script used for analysis |
| `death_metal_tracks.csv` | Dataset of death metal songs and audio features |
| `report_screenshots/` | Screenshots from analysis results used in the report |
| `README.md` | Project documentation (this file) |

---

## Author

Steven  
INF6027 Coursework – Data Science (2025)
