# 01_data_preparation.R
# Data preparation and DASS-21 scoring

library(readr)

# Load data
student_data <- read_csv(
  "Data/student_mental_health.csv",
  show_col_types = FALSE
)

# =========================================================
# 1. Validate DASS-21 questions
# =========================================================

dass_items <- paste0("Question ", 1:21)

student_data[dass_items] <- lapply(
  student_data[dass_items],
  function(x) {
    x[x < 0 | x > 3] <- NA
    x
  }
)

# =========================================================
# 2. Clean age
# =========================================================

student_data$age[
  student_data$age < 15
] <- NA

# =========================================================
# 3. Define DASS-21 subscales
# =========================================================

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

# =========================================================
# 4. Calculate Depression score
# =========================================================

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

# =========================================================
# 5. Calculate Anxiety score
# =========================================================

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

# =========================================================
# 6. Calculate Stress score
# =========================================================

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
