# DASS-21 Student Mental Health Analysis

Statistical analysis and visualization of student mental health data using R and the Depression Anxiety Stress Scales (DASS-21).

## Project Overview

This project analyzes depression, anxiety, and stress scores among university students.

The analysis investigates whether mental health scores are associated with:

- Age
- Gender
- Academic year

The project was developed as an independent statistical analysis using R.

## Research Questions

1. Is age associated with depression, anxiety, or stress scores?
2. Are depression, anxiety, and stress scores different between female and male students?
3. Do depression, anxiety, and stress scores differ across academic years?

## Dataset

The dataset used in this project was obtained from the UK Data Service ReShare.

**Original dataset:**

Toth, E., Faherty, T., & Raymond, J. (2021).  
*Student Mental Health During Covid-19 Pandemic, 2020.*  
UK Data Service ReShare.

DOI: 10.5255/UKDA-SN-854720

The dataset belongs to the original data creators and is not claimed as the property of the author of this repository.

Users who reuse the dataset should follow the original data source's access, licensing, and citation requirements.

## Analysis

The statistical analysis was conducted in R and includes:

- Pearson correlation
- Independent-samples Welch t-tests
- One-way ANOVA
- Tukey post-hoc tests
- Descriptive statistics
- Data visualization

The analysis focuses on three DASS-21 subscales:

- Depression
- Anxiety
- Stress

## Project Structure

```text
dass21-student-mental-health-analysis/
│
├── README.md
├── LICENSE
│
├── R/
│   ├── 00_main.R
│   ├── 02_analysis.R
│   └── 03_visualization.R
│
├── results/
│   ├── figures/
│   │   ├── age_depression.png
│   │   ├── dass_by_year.png
│   │   ├── dass_correlation.png
│   │   ├── depression_by_age.png
│   │   ├── gender_boxplots.png
│   │   └── mean_dass_scores.png
│   │
│   └── tables/
│       └── results_summary.csv
│
└── report/
    └── Student_Mental_Health_DASS21_Analysis_Report.docx
