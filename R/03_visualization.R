# 03_visualization.R
# Visualization of student mental health data

library(ggplot2)

# Create folder for figures
dir.create(
  "results/figures",
  showWarnings = FALSE,
  recursive = TRUE
)


# =========================================================
# 1. Age and Depression scatter plot
# =========================================================

age_depression_data <- subset(
  student_data,
  !is.na(age) & !is.na(depression_score)
)

p1 <- ggplot(
  age_depression_data,
  aes(x = age, y = depression_score)
) +
  geom_jitter(
    width = 0.2,
    height = 0.2,
    alpha = 0.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Age and Depression Score",
    x = "Age",
    y = "Depression Score"
  ) +
  theme_minimal()

ggsave(
  "results/figures/age_depression.png",
  p1,
  width = 8,
  height = 6
)


# =========================================================
# 2. Mean Depression Score by Age
# =========================================================

p2 <- ggplot(
  age_depression,
  aes(x = age, y = depression_score)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Depression Score by Age",
    x = "Age",
    y = "Mean Depression Score"
  ) +
  theme_minimal()

ggsave(
  "results/figures/depression_by_age.png",
  p2,
  width = 8,
  height = 6
)


# =========================================================
# 3. Mean DASS-21 scores
# =========================================================

mean_scores_plot <- data.frame(
  Subscale = names(mean_scores),
  Mean = as.numeric(mean_scores)
)

p3 <- ggplot(
  mean_scores_plot,
  aes(x = Subscale, y = Mean)
) +
  geom_col() +
  labs(
    title = "Mean Depression, Anxiety and Stress Scores",
    x = "DASS-21 Subscale",
    y = "Mean Score"
  ) +
  theme_minimal()

ggsave(
  "results/figures/mean_dass_scores.png",
  p3,
  width = 8,
  height = 6
)


# =========================================================
# 4. DASS-21 correlation plot
# =========================================================

dass_complete <- na.omit(
  student_data[, c(
    "depression_score",
    "anxiety_score",
    "stress_score"
  )]
)

png(
  "results/figures/dass_correlation.png",
  width = 800,
  height = 600
)

pairs(
  dass_complete,
  main = "Relationships Between DASS-21 Subscales",
  pch = 19
)

dev.off()


# =========================================================
# 5. Mean DASS-21 scores by survey year
# =========================================================

year_long <- data.frame(
  Year = rep(year_means$year, times = 3),
  Subscale = rep(
    c(
      "Depression",
      "Anxiety",
      "Stress"
    ),
    each = nrow(year_means)
  ),
  Mean = c(
    year_means$depression_score,
    year_means$anxiety_score,
    year_means$stress_score
  )
)

p5 <- ggplot(
  year_long,
  aes(
    x = Year,
    y = Mean,
    group = Subscale
  )
) +
  geom_line(aes(linetype = Subscale)) +
  geom_point() +
  labs(
    title = "Mean DASS-21 Scores by Survey Year",
    x = "Survey Year",
    y = "Mean Score",
    linetype = "Subscale"
  ) +
  theme_minimal()

ggsave(
  "results/figures/dass_by_year.png",
  p5,
  width = 8,
  height = 6
)


# =========================================================
# 6. Depression by gender
# =========================================================

p6 <- ggplot(
  gender_data,
  aes(
    x = gender,
    y = depression_score
  )
) +
  geom_boxplot() +
  labs(
    title = "Depression Score by Gender",
    x = "Gender",
    y = "Depression Score"
  ) +
  theme_minimal()

ggsave(
  "results/figures/depression_gender.png",
  p6,
  width = 8,
  height = 6
)


# =========================================================
# 7. Anxiety by gender
# =========================================================

p7 <- ggplot(
  anxiety_gender_data,
  aes(
    x = gender,
    y = anxiety_score
  )
) +
  geom_boxplot() +
  labs(
    title = "Anxiety Score by Gender",
    x = "Gender",
    y = "Anxiety Score"
  ) +
  theme_minimal()

ggsave(
  "results/figures/anxiety_gender.png",
  p7,
  width = 8,
  height = 6
)


# =========================================================
# 8. Stress by gender
# =========================================================

p8 <- ggplot(
  stress_gender_data,
  aes(
    x = gender,
    y = stress_score
  )
) +
  geom_boxplot() +
  labs(
    title = "Stress Score by Gender",
    x = "Gender",
    y = "Stress Score"
  ) +
  theme_minimal()

ggsave(
  "results/figures/stress_gender.png",
  p8,
  width = 8,
  height = 6
)
