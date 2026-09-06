# DASS-21 Student Mental Health Analysis

Statistical analysis and visualization of student mental health data using R and the Depression Anxiety Stress Scales (DASS-21).

## Project Overview

This project analyzes depression, anxiety, and stress scores among university students.

The analysis investigates whether mental health scores are associated with:

- Age
- Gender
- Survey year

The project was developed as an independent statistical analysis using R.

## Research Questions

1. Is age associated with depression, anxiety, or stress scores?
2. Are depression, anxiety, and stress scores different between female and male students?
3. Do depression, anxiety, and stress scores differ across survey years?

## Dataset

The dataset used in this project was obtained from the UK Data Service ReShare.

**Original dataset:**

Toth, E., Faherty, T., & Raymond, J. (2021).  
*Student Mental Health During Covid-19 Pandemic, 2020.*  
UK Data Service ReShare.

DOI: 10.5255/UKDA-SN-854720

The dataset belongs to the original data creators and is not claimed as the property of the author of this repository.

Users who reuse the dataset should follow the original data source's access, licensing, and citation requirements.

## Data Preparation

The data preparation script:

- Validates all 21 DASS-21 questions to ensure responses are within the expected 0–3 range.
- Treats invalid DASS-21 responses as missing values.
- Treats ages below 15 as missing values.
- Calculates Depression, Anxiety, and Stress subscale scores.
- Preserves missing scores when all items belonging to a subscale are missing.

The project uses raw DASS-21 subscale scores ranging from 0 to 21. Scores are not multiplied by two to convert them to the DASS-42 equivalent.

## Analysis

The statistical analysis was conducted in R and includes:

- Pearson correlation
- Independent-samples Welch t-tests
- One-way ANOVA
- Tukey post-hoc tests
- Descriptive statistics
- Effect sizes
- Data visualization

The analysis focuses on three DASS-21 subscales:

- Depression
- Anxiety
- Stress

For gender comparisons, Cohen's d is used as an effect size.

For one-way ANOVA, eta squared is used as an effect size.

## Interpretation

The analyses are observational and should not be interpreted as evidence of causation.

In particular, differences between survey years should not be interpreted as evidence that the COVID-19 pandemic directly caused changes in mental health scores.

A non-significant statistical test is interpreted as insufficient evidence of an association or difference, rather than proof that no relationship exists.

## Project Structure

```text
dass21-student-mental-health-analysis/
│
├── README.md
├── LICENSE
│
├── Data/
│   └── student_mental_health.csv
│
├── R/
│   ├── 00_main.R
│   ├── 01_data_preparation.R
│   ├── 02_analysis.R
│   └── 03_visualization.R
│
├── results/
│   ├── figures/
│   │   ├── age_depression.png
│   │   ├── anxiety_gender.png
│   │   ├── dass_by_year.png
│   │   ├── dass_correlation.png
│   │   ├── depression_by_age.png
│   │   ├── depression_gender.png
│   │   ├── mean_dass_scores.png
│   │   └── stress_gender.png
│   │
│   └── tables/
│       └── results_summary.csv
│
└── report/
    └── Student_Mental_Health_DASS21_Analysis_Report.docx
