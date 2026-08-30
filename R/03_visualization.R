# 03_visualization.R
# Visualization of student mental health data


# Create folder for figures

dir.create(
  "results/figures",
  showWarnings = FALSE
)


# =========================================================
# 1. Age and Depression scatter plot
# =========================================================

png(
  "results/figures/age_depression.png",
  width = 800,
  height = 600
)

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

dev.off()


# =========================================================
# 2. Mean Depression Score by Age
# =========================================================

png(
  "results/figures/depression_by_age.png",
  width = 800,
  height = 600
)

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

dev.off()


# =========================================================
# 3. Mean DASS-21 scores
# =========================================================

png(
  "results/figures/mean_dass_scores.png",
  width = 800,
  height = 600
)

barplot(
  mean_scores,
  xlab = "DASS-21 Subscale",
  ylab = "Mean Score",
  main = "Mean Depression, Anxiety and Stress Scores",
  ylim = c(0, 8)
)

dev.off()


# =========================================================
# 4. Correlation plot of DASS-21
# =========================================================

png(
  "results/figures/dass_correlation.png",
  width = 800,
  height = 600
)

pairs(
  student_data[, c(
    "depression_score",
    "anxiety_score",
    "stress_score"
  )],
  main = "Relationships Between DASS-21 Subscales",
  pch = 19
)

dev.off()


# =========================================================
# 5. Mean DASS-21 scores by year
# =========================================================

png(
  "results/figures/dass_by_year.png",
  width = 800,
  height = 600
)

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

dev.off()


# =========================================================
# 6. Gender boxplots
# =========================================================

png(
  "results/figures/gender_boxplots.png",
  width = 800,
  height = 1200
)

par(
  mfrow = c(3, 1)
)

boxplot(
  depression_score ~ gender,
  data = gender_data,
  subset = gender %in% c("female", "male"),
  xlab = "Gender",
  ylab = "Depression Score",
  main = "Depression Score by Gender"
)

boxplot(
  anxiety_score ~ gender,
  data = gender_data,
  subset = gender %in% c("female", "male"),
  xlab = "Gender",
  ylab = "Anxiety Score",
  main = "Anxiety Score by Gender"
)

boxplot(
  stress_score ~ gender,
  data = gender_data,
  subset = gender %in% c("female", "male"),
  xlab = "Gender",
  ylab = "Stress Score",
  main = "Stress Score by Gender"
)

dev.off()
