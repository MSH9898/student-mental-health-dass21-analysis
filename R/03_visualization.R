# 03_visualization.R
# Visualization of student mental health data


# =========================================================
# 0. Load required packages and data
# =========================================================

library(readr)
library(ggplot2)

student_data <- read_csv(
  "Data/student_mental_health.csv",
  show_col_types = FALSE
)


# =========================================================
# 1. Data cleaning and DASS-21 scores
# =========================================================

dass_items <- paste0("Question ", 1:21)

student_data[dass_items] <- lapply(
  student_data[dass_items],
  function(x) {
    x[x < 0 | x > 3] <- NA
    x
  }
)

student_data$age[
  student_data$age < 15
] <- NA


depression_items <- c(
  "Question 3",
  "Question 5",
  "Question 10",
  "Question 13",
  "Question 16",
  "Question 17",
  "Question 21"
)

anxiety_items <- c(
  "Question 2",
  "Question 4",
  "Question 7",
  "Question 9",
  "Question 15",
  "Question 19",
  "Question 20"
)

stress_items <- c(
  "Question 1",
  "Question 6",
  "Question 8",
  "Question 11",
  "Question 12",
  "Question 14",
  "Question 18"
)


depression_n <- rowSums(
  !is.na(student_data[depression_items])
)

student_data$depression_score <- rowSums(
  student_data[depression_items],
  na.rm = TRUE
)

student_data$depression_score[
  depression_n == 0
] <- NA


anxiety_n <- rowSums(
  !is.na(student_data[anxiety_items])
)

student_data$anxiety_score <- rowSums(
  student_data[anxiety_items],
  na.rm = TRUE
)

student_data$anxiety_score[
  anxiety_n == 0
] <- NA


stress_n <- rowSums(
  !is.na(student_data[stress_items])
)

student_data$stress_score <- rowSums(
  student_data[stress_items],
  na.rm = TRUE
)

student_data$stress_score[
  stress_n == 0
] <- NA


# =========================================================
# 2. Create output folder
# =========================================================

dir.create(
  "results/figures",
  showWarnings = FALSE,
  recursive = TRUE
)


# =========================================================
# 3. Age and Depression scatter plot
# =========================================================

age_depression_data <- subset(
  student_data,
  !is.na(age) & !is.na(depression_score)
)

p1 <- ggplot(
  age_depression_data,
  aes(x = age, y = depression_score)
) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +
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
# 4. Mean Depression Score by Age
# =========================================================

age_depression <- aggregate(
  depression_score ~ age,
  data = student_data,
  FUN = mean,
  na.rm = TRUE
)

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
# 5. Mean DASS-21 scores
# =========================================================

mean_scores <- data.frame(
  Subscale = c(
    "Depression",
    "Anxiety",
    "Stress"
  ),
  Mean = c(
    mean(
      student_data$depression_score,
      na.rm = TRUE
    ),
    mean(
      student_data$anxiety_score,
      na.rm = TRUE
    ),
    mean(
      student_data$stress_score,
      na.rm = TRUE
    )
  )
)

p3 <- ggplot(
  mean_scores,
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
# 6. Correlation plot of DASS-21
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
# 7. Mean DASS-21 scores by year
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

year_long <- data.frame(
  Year = rep(
    year_means$year,
    times = 3
  ),
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
# 8. Gender boxplots
# =========================================================

gender_data <- subset(
  student_data,
  gender %in% c("female", "male")
)

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


p7 <- ggplot(
  gender_data,
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


p8 <- ggplot(
  gender_data,
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
