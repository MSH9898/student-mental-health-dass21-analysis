# 02_analysis.R
# Analysis of student mental health data


# =========================================================
# 1. Load data
# =========================================================

library(readr)

student_data <- read_csv(
  "Data/student_mental_health.csv",
  show_col_types = FALSE
)


# =========================================================
# 2. Data cleaning
# =========================================================

# Check and remove invalid values from all DASS-21 questions

dass_items <- paste0("Question ", 1:21)

student_data[dass_items] <- lapply(
  student_data[dass_items],
  function(x) {
    x[x < 0 | x > 3] <- NA
    x
  }
)

# Remove invalid ages below 15
student_data$age[
  student_data$age < 15
] <- NA


# =========================================================
# 3. Calculate DASS-21 subscale scores
# =========================================================

student_data$depression_score <- rowSums(
  student_data[, c(
    "Question 3",
    "Question 5",
    "Question 10",
    "Question 13",
    "Question 16",
    "Question 17",
    "Question 21"
  )]
)

student_data$anxiety_score <- rowSums(
  student_data[, c(
    "Question 2",
    "Question 4",
    "Question 7",
    "Question 9",
    "Question 15",
    "Question 19",
    "Question 20"
  )]
)

student_data$stress_score <- rowSums(
  student_data[, c(
    "Question 1",
    "Question 6",
    "Question 8",
    "Question 11",
    "Question 12",
    "Question 14",
    "Question 18"
  )]
)


# =========================================================
# 4. Correlation between age and depression
# =========================================================

cor.test(
  student_data$age,
  student_data$depression_score,
  use = "complete.obs"
)


# Scatter plot: Age and Depression

plot(
  jitter(student_data$age),
  jitter(student_data$depression_score),
  xlab = "Age",
  ylab = "Depression Score",
  main = "Age and Depression Score",
  pch = 19
)

abline(
  lm(depression_score ~ age, data = student_data),
  lwd = 2
)


# =========================================================
# 5. ANOVA: Depression by year
# =========================================================

anova_model <- aov(
  depression_score ~ factor(year),
  data = student_data
)

summary(anova_model)


# Post-hoc Tukey test

TukeyHSD(anova_model)


# =========================================================
# 6. Mean Depression Score by Age
# =========================================================

age_depression <- aggregate(
  depression_score ~ age,
  data = student_data,
  FUN = mean,
  na.rm = TRUE
)

plot(
  age_depression$age,
  age_depression$depression_score,
  type = "b",
  pch = 19,
  xlab = "Age",
  ylab = "Mean Depression Score",
  main = "Mean Depression Score by Age"
)


# =========================================================
# 7. Gender analysis: Depression
# =========================================================

gender_data <- subset(
  student_data,
  gender %in% c("female", "male")
)

gender_means <- aggregate(
  depression_score ~ gender,
  data = gender_data,
  FUN = mean,
  na.rm = TRUE
)

gender_means


t_test_gender <- t.test(
  depression_score ~ gender,
  data = gender_data
)

t_test_gender


# Boxplot: Depression by Gender

boxplot(
  depression_score ~ gender,
  data = gender_data,
  xlab = "Gender",
  ylab = "Depression Score",
  main = "Depression Score by Gender"
)


# =========================================================
# 8. Descriptive statistics for DASS-21
# =========================================================

summary(
  student_data[, c(
    "depression_score",
    "anxiety_score",
    "stress_score"
  )]
)


# Mean scores

mean_scores <- c(
  Depression = mean(
    student_data$depression_score,
    na.rm = TRUE
  ),
  Anxiety = mean(
    student_data$anxiety_score,
    na.rm = TRUE
  ),
  Stress = mean(
    student_data$stress_score,
    na.rm = TRUE
  )
)

mean_scores


# Barplot: Mean DASS-21 scores

barplot(
  mean_scores,
  xlab = "DASS-21 Subscale",
  ylab = "Mean Score",
  main = "Mean Depression, Anxiety and Stress Scores",
  ylim = c(0, 8)
)


# =========================================================
# 9. Correlation between DASS-21 subscales
# =========================================================

cor(
  student_data[, c(
    "depression_score",
    "anxiety_score",
    "stress_score"
  )],
  use = "complete.obs"
)


# Correlation plot

pairs(
  student_data[, c(
    "depression_score",
    "anxiety_score",
    "stress_score"
  )],
  main = "Relationships Between DASS-21 Subscales",
  pch = 19
)


# =========================================================
# 10. ANOVA: Anxiety by year
# =========================================================

anova_anxiety <- aov(
  anxiety_score ~ factor(year),
  data = student_data
)

summary(anova_anxiety)


# Post-hoc Tukey test

TukeyHSD(anova_anxiety)


# =========================================================
# 11. ANOVA: Stress by year
# =========================================================

anova_stress <- aov(
  stress_score ~ factor(year),
  data = student_data
)

summary(anova_stress)


# =========================================================
# 12. Mean DASS-21 scores by year
# =========================================================

year_means <- aggregate(
  cbind(
    depression_score,
    anxiety_score,
    stress_score
  ) ~ year,
  data = student_data,
  FUN = mean,
  na.rm = TRUE
)

year_means


# Line plot: Mean DASS-21 scores by year

plot(
  year_means$year,
  year_means$depression_score,
  type = "b",
  pch = 19,
  xlab = "Year",
  ylab = "Mean Score",
  main = "Mean DASS-21 Scores by Year",
  ylim = c(0, 8)
)

lines(
  year_means$year,
  year_means$anxiety_score,
  type = "b",
  pch = 19
)

lines(
  year_means$year,
  year_means$stress_score,
  type = "b",
  pch = 19
)

legend(
  "topleft",
  legend = c(
    "Depression",
    "Anxiety",
    "Stress"
  ),
  lty = 1,
  pch = 19
)


# =========================================================
# 13. Gender analysis: Anxiety
# =========================================================

t_test_anxiety_gender <- t.test(
  anxiety_score ~ gender,
  data = student_data,
  subset = gender %in% c("female", "male")
)

t_test_anxiety_gender


# Boxplot: Anxiety by Gender

boxplot(
  anxiety_score ~ gender,
  data = student_data,
  subset = gender %in% c("female", "male"),
  xlab = "Gender",
  ylab = "Anxiety Score",
  main = "Anxiety Score by Gender"
)


# =========================================================
# 14. Gender analysis: Stress
# =========================================================

t_test_stress_gender <- t.test(
  stress_score ~ gender,
  data = student_data,
  subset = gender %in% c("female", "male")
)

t_test_stress_gender


# Boxplot: Stress by Gender

boxplot(
  stress_score ~ gender,
  data = student_data,
  subset = gender %in% c("female", "male"),
  xlab = "Gender",
  ylab = "Stress Score",
  main = "Stress Score by Gender"
)


# =========================================================
# 15. Correlation between age and anxiety
# =========================================================

cor.test(
  student_data$age,
  student_data$anxiety_score,
  use = "complete.obs"
)


# =========================================================
# 16. Correlation between age and stress
# =========================================================

cor.test(
  student_data$age,
  student_data$stress_score,
  use = "complete.obs"
)


# =========================================================
# 17. Mean DASS-21 scores by gender
# =========================================================

gender_dass_means <- aggregate(
  cbind(
    depression_score,
    anxiety_score,
    stress_score
  ) ~ gender,
  data = gender_data,
  FUN = mean,
  na.rm = TRUE
)

gender_dass_means


# =========================================================
# 18. Summary table of statistical tests
# =========================================================

results_summary <- data.frame(
  Analysis = c(
    "Age - Depression",
    "Age - Anxiety",
    "Age - Stress",
    "Gender - Depression",
    "Gender - Anxiety",
    "Gender - Stress",
    "Year - Depression",
    "Year - Anxiety",
    "Year - Stress"
  ),

  P_value = c(
    0.622,
    0.6498,
    0.3939,
    0.6979,
    0.4992,
    0.1233,
    0.000103,
    0.00167,
    0.195
  )
)


# Save results summary table

write.csv(
  results_summary,
  "results/tables/results_summary.csv",
  row.names = FALSE
)
