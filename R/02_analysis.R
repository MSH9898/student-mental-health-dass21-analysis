# 02_analysis.R

# Statistical analysis of student mental health data

age_depression_data <- subset(
student_data,
!is.na(age) & !is.na(depression_score)
)

age_anxiety_data <- subset(
student_data,
!is.na(age) & !is.na(anxiety_score)
)

age_stress_data <- subset(
student_data,
!is.na(age) & !is.na(stress_score)
)

cor_depression <- cor.test(
age_depression_data$age,
age_depression_data$depression_score,
method = "pearson"
)

cor_anxiety <- cor.test(
age_anxiety_data$age,
age_anxiety_data$anxiety_score,
method = "pearson"
)

cor_stress <- cor.test(
age_stress_data$age,
age_stress_data$stress_score,
method = "pearson"
)

anova_depression <- aov(
depression_score ~ factor(year),
data = student_data
)

anova_depression_table <- summary(anova_depression)[[1]]

tukey_depression <- TukeyHSD(anova_depression)

eta_squared_depression <-
anova_depression_table["factor(year)", "Sum Sq"] /
sum(anova_depression_table[, "Sum Sq"])

age_depression <- aggregate(
depression_score ~ age,
data = student_data,
FUN = mean,
na.rm = TRUE
)

gender_data <- subset(
student_data,
gender %in% c("female", "male")
)

gender_depression_data <- subset(
gender_data,
!is.na(depression_score)
)

t_test_depression_gender <- t.test(
depression_score ~ gender,
data = gender_depression_data
)

female_depression <- gender_depression_data$depression_score[
gender_depression_data$gender == "female"
]

male_depression <- gender_depression_data$depression_score[
gender_depression_data$gender == "male"
]

n_female_dep <- length(female_depression)
n_male_dep <- length(male_depression)

pooled_sd_dep <- sqrt(
(
(n_female_dep - 1) * sd(female_depression)^2 +
(n_male_dep - 1) * sd(male_depression)^2
) /
(n_female_dep + n_male_dep - 2)
)

cohen_d_depression <-
(mean(female_depression) - mean(male_depression)) /
pooled_sd_dep

dass_summary <- data.frame(
Subscale = c(
"Depression",
"Anxiety",
"Stress"
),
Mean = c(
mean(student_data$depression_score, na.rm = TRUE),
mean(student_data$anxiety_score, na.rm = TRUE),
mean(student_data$stress_score, na.rm = TRUE)
),
SD = c(
sd(student_data$depression_score, na.rm = TRUE),
sd(student_data$anxiety_score, na.rm = TRUE),
sd(student_data$stress_score, na.rm = TRUE)
),
N = c(
sum(!is.na(student_data$depression_score)),
sum(!is.na(student_data$anxiety_score)),
sum(!is.na(student_data$stress_score))
)
)

mean_scores <- dass_summary$Mean
names(mean_scores) <- dass_summary$Subscale

dass_complete <- na.omit(
student_data[
,
c(
"depression_score",
"anxiety_score",
"stress_score"
)
]
)

dass_correlation <- cor(
dass_complete,
method = "pearson"
)

anova_anxiety <- aov(
anxiety_score ~ factor(year),
data = student_data
)

anova_anxiety_table <- summary(anova_anxiety)[[1]]

tukey_anxiety <- TukeyHSD(anova_anxiety)

eta_squared_anxiety <-
anova_anxiety_table["factor(year)", "Sum Sq"] /
sum(anova_anxiety_table[, "Sum Sq"])

anova_stress <- aov(
stress_score ~ factor(year),
data = student_data
)

anova_stress_table <- summary(anova_stress)[[1]]

tukey_stress <- TukeyHSD(anova_stress)

eta_squared_stress <-
anova_stress_table["factor(year)", "Sum Sq"] /
sum(anova_stress_table[, "Sum Sq"])

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

anxiety_gender_data <- subset(
gender_data,
!is.na(anxiety_score)
)

t_test_anxiety_gender <- t.test(
anxiety_score ~ gender,
data = anxiety_gender_data
)

female_anxiety <- anxiety_gender_data$anxiety_score[
anxiety_gender_data$gender == "female"
]

male_anxiety <- anxiety_gender_data$anxiety_score[
anxiety_gender_data$gender == "male"
]

n_female_anx <- length(female_anxiety)
n_male_anx <- length(male_anxiety)

pooled_sd_anx <- sqrt(
(
(n_female_anx - 1) * sd(female_anxiety)^2 +
(n_male_anx - 1) * sd(male_anxiety)^2
) /
(n_female_anx + n_male_anx - 2)
)

cohen_d_anxiety <-
(mean(female_anxiety) - mean(male_anxiety)) /
pooled_sd_anx

stress_gender_data <- subset(
gender_data,
!is.na(stress_score)
)

t_test_stress_gender <- t.test(
stress_score ~ gender,
data = stress_gender_data
)

female_stress <- stress_gender_data$stress_score[
stress_gender_data$gender == "female"
]

male_stress <- stress_gender_data$stress_score[
stress_gender_data$gender == "male"
]

n_female_str <- length(female_stress)
n_male_str <- length(male_stress)

pooled_sd_str <- sqrt(
(
(n_female_str - 1) * sd(female_stress)^2 +
(n_male_str - 1) * sd(male_stress)^2
) /
(n_female_str + n_male_str - 2)
)

cohen_d_stress <-
(mean(female_stress) - mean(male_stress)) /
pooled_sd_str

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
Statistic = c(
unname(cor_depression$estimate),
unname(cor_anxiety$estimate),
unname(cor_stress$estimate),
unname(t_test_depression_gender$statistic),
unname(t_test_anxiety_gender$statistic),
unname(t_test_stress_gender$statistic),
anova_depression_table["factor(year)", "F value"],
anova_anxiety_table["factor(year)", "F value"],
anova_stress_table["factor(year)", "F value"]
),
P_value = c(
cor_depression$p.value,
cor_anxiety$p.value,
cor_stress$p.value,
t_test_depression_gender$p.value,
t_test_anxiety_gender$p.value,
t_test_stress_gender$p.value,
anova_depression_table["factor(year)", "Pr(>F)"],
anova_anxiety_table["factor(year)", "Pr(>F)"],
anova_stress_table["factor(year)", "Pr(>F)"]
),
Effect_Size = c(
unname(cor_depression$estimate),
unname(cor_anxiety$estimate),
unname(cor_stress$estimate),
cohen_d_depression,
cohen_d_anxiety,
cohen_d_stress,
eta_squared_depression,
eta_squared_anxiety,
eta_squared_stress
),
Effect_Type = c(
"Pearson r",
"Pearson r",
"Pearson r",
"Cohen's d",
"Cohen's d",
"Cohen's d",
"Eta squared",
"Eta squared",
"Eta squared"
)
)

dir.create(
"results/tables",
showWarnings = FALSE,
recursive = TRUE
)

write.csv(
results_summary,
"results/tables/results_summary.csv",
row.names = FALSE
)
