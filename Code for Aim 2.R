setwd("~/WORKING_DIRECTORY_FOLDER_NAME")

library(ggplot2)
library(dplyr)
library(broom)
library(effectsize)
library(emmeans)
library(car)

#Read data
df_post <- read.csv("CSV_FILE_NAME_FOR_RESULTS")
str(df_post)

#Convert appropriate variables to factors (categorical)
df_post$afs_grade <- as.factor(df_post$afs_grade)
df_post$sex <- as.factor(df_post$sex)
df_post$indication <- as.factor(df_post$indication)
df_post$oesophagitis <- as.factor(df_post$oesophagitis)
df_post$ppi_use <- as.factor(df_post$ppi_use)
df_post$ppi_continued <- as.factor(df_post$ppi_continued)
df_post$ppi_response <- as.factor(df_post$ppi_response)
df_post$F <- factor(df_post$F, levels = c("Present", "Absent"))

# Create composite PPI status variable
df_post$ppi_status <- case_when(
  df_post$ppi_use == "0" ~ "No PPI",
  df_post$ppi_use %in% c("1", "2") & df_post$ppi_continued == "0" ~ "PPI stopped",
  df_post$ppi_use %in% c("1", "2") & df_post$ppi_continued == "1" ~ "PPI continued",
  TRUE ~ NA_character_
)
df_post$ppi_status <- factor(df_post$ppi_status,
                             levels = c("No PPI", "PPI stopped", "PPI continued")) #makes "No PPI" the reference category for MLR

#=============================================================================
# Descriptive Statistics (overall)
#=============================================================================

cat("=== DEMOGRAPHIC AND CLINICAL CHARACTERISTICS (POST INSUFFLATION) ===\n\n")

# Overall sample characteristics
cat("OVERALL SAMPLE CHARACTERISTICS:\n")
cat("Total sample size:", nrow(df_post), "\n")
cat("Median age:", median(df_post$age, na.rm = TRUE), "years\n")
cat("Age IQR:", quantile(df_post$age, 0.25, na.rm = TRUE), "-", 
    quantile(df_post$age, 0.75, na.rm = TRUE), "years\n")
cat("Age range:", min(df_post$age, na.rm = TRUE), "-", max(df_post$age, na.rm = TRUE), "years\n")
cat("Median BMI:", round(median(df_post$bmi, na.rm = TRUE), 1), "\n")
cat("BMI IQR:", round(quantile(df_post$bmi, 0.25, na.rm = TRUE), 1), "-",
    round(quantile(df_post$bmi, 0.75, na.rm = TRUE), 1), "\n")
cat("Mean GERDQ Score:", round(mean(df_post$gerdq_score, na.rm = TRUE), 2), "\n")
cat("Median GERDQ Score:", round(median(df_post$gerdq_score, na.rm = TRUE), 2), "\n")
cat("GERDQ Score IQR:", round(quantile(df_post$gerdq_score, 0.25, na.rm = TRUE), 2), "-",
    round(quantile(df_post$gerdq_score, 0.75, na.rm = TRUE), 2), "\n")
sex_count <- sum(df_post$sex == "F", na.rm = TRUE)
sex_prop <- round(mean(df_post$sex == "F", na.rm = TRUE) * 100, 1)
cat("Percentage of Females:", sex_count, "(", sex_prop, "%)\n\n")

# Indication breakdown
cat("INDICATION FOR PROCEDURE:\n")
indication_table <- table(df_post$indication, useNA = "ifany")
indication_props <- prop.table(indication_table) * 100
for(i in 1:length(indication_table)) {
  cat("Indication", names(indication_table)[i], ":", indication_table[i],
      "(", round(indication_props[i], 1), "%)\n")
}
cat("\n")

# Oesophagitis findings
cat("OESOPHAGITIS FINDINGS:\n")
oesoph_table <- table(df_post$oesophagitis, useNA = "ifany")
oesoph_props <- prop.table(oesoph_table) * 100
for(i in 1:length(oesoph_table)) { #creates a loop
  grade_name <- ifelse(names(oesoph_table)[i] == "0", "No oesophagitis",
                       paste("LA Grade", names(oesoph_table)[i]))
  cat(grade_name, ":", oesoph_table[i], "(", round(oesoph_props[i], 1), "%)\n")
}
cat("\n")

# PPI Usage Analysis
cat("PPI USAGE PATTERNS:\n")
ppi_use_table <- table(df_post$ppi_use, useNA = "ifany")
ppi_use_props <- prop.table(ppi_use_table) * 100
cat("No PPI use:", ppi_use_table["0"], "(", round(ppi_use_props["0"], 1), "%)\n")
cat("Intermittent PPI use:", ppi_use_table["1"], "(", round(ppi_use_props["1"], 1), "%)\n")
cat("Daily PPI use:", ppi_use_table["2"], "(", round(ppi_use_props["2"], 1), "%)\n\n")

# Among PPI users only
ppi_users <- df_post[df_post$ppi_use %in% c("1", "2"), ] #defines ppi_users as intermittent(1) OR daily(2) users
if(nrow(ppi_users) > 0) {
  cat("AMONG PPI USERS (n =", nrow(ppi_users), "):\n")
  
  # Continuation patterns
  continued_table <- table(ppi_users$ppi_continued, useNA = "ifany")
  continued_props <- prop.table(continued_table) * 100
  cat("Stopped PPI before procedure:", continued_table["0"], "(", round(continued_props["0"], 1), "%)\n")
  cat("Continued PPI through procedure:", continued_table["1"], "(", round(continued_props["1"], 1), "%)\n\n")
  
  # Response patterns
  cat("PPI RESPONSE AMONG USERS:\n")
  response_table <- table(ppi_users$ppi_response, useNA = "ifany")
  response_props <- prop.table(response_table) * 100
  cat("No response:", response_table["0"], "(", round(response_props["0"], 1), "%)\n")
  cat("Partial response:", response_table["1"], "(", round(response_props["1"], 1), "%)\n")
  cat("Full response:", response_table["2"], "(", round(response_props["2"], 1), "%)\n\n")
}

# PPI Status (composite variable)
cat("PPI STATUS (COMPOSITE VARIABLE):\n")
ppi_status_table <- table(df_post$ppi_status, useNA = "ifany")
ppi_status_props <- prop.table(ppi_status_table) * 100
for(i in 1:length(ppi_status_table)) {
  cat(names(ppi_status_table)[i], ":", ppi_status_table[i],
      "(", round(ppi_status_props[i], 1), "%)\n")
}
cat("\n")

#=============================================================================
# Descriptive statistics split by AFS grade
#=============================================================================
cat("=== CHARACTERISTICS BY AFS GRADE ===\n")
descriptive_stats <- df_post %>%
  group_by(afs_grade) %>%
  summarise(
    n = n(),
    mean_gerdq = round(mean(gerdq_score, na.rm = TRUE), 2),
    sd_gerdq = round(sd(gerdq_score, na.rm = TRUE), 2),
    median_gerdq = round(median(gerdq_score, na.rm = TRUE), 2),
    q25_gerdq = quantile(gerdq_score, 0.25, na.rm = TRUE),
    q75_gerdq = quantile(gerdq_score, 0.75, na.rm = TRUE),
    min_gerdq = min(gerdq_score, na.rm = TRUE),
    max_gerdq = max(gerdq_score, na.rm = TRUE),
    mean_age = round(mean(age, na.rm = TRUE), 1),
    median_age = round(median(age, na.rm = TRUE), 1),
    q25_age = quantile(age, 0.25, na.rm = TRUE),
    q75_age = quantile(age, 0.75, na.rm = TRUE),
    mean_bmi = round(mean(bmi, na.rm = TRUE), 1),
    median_bmi = round(median(bmi, na.rm = TRUE), 1),
    q25_bmi = round(quantile(bmi, 0.25, na.rm = TRUE), 1),
    q75_bmi = round(quantile(bmi, 0.75, na.rm = TRUE), 1),
    pct_female = round(mean(sex == "F", na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  )
print(descriptive_stats)

# PPI status by AFS grade
cat("\n=== PPI STATUS BY AFS GRADE ===\n")
ppi_by_afs <- table(df_post$afs_grade, df_post$ppi_status)
print(ppi_by_afs)
cat("\nRow percentages (within each AFS grade):\n")
print(round(prop.table(ppi_by_afs, 1) * 100, 1))

#=============================================================================
# Statistical Tests (unadjusted analysis; post-insufflation)
#=============================================================================

#Levene's test for homogeneity of variance
levene_test <- leveneTest(gerdq_score ~ afs_grade, data = df_post)
cat("Levene's Test for Homogeneity of Variance:\n")
print(levene_test)

# One-way ANOVA (unadjusted)
anova_unadj <- aov(gerdq_score ~ afs_grade, data = df_post)
anova_summary_unadj <- summary(anova_unadj)
cat("\n=== ONE-WAY ANOVA (UNADJUSTED) RESULTS ===\n")
print(anova_summary_unadj)

#=============================================================================
# Multiple Linear Regression (adjusted analysis; post-insufflation)
#=============================================================================
model_post <- lm(gerdq_score ~ afs_grade + age + sex + bmi + ppi_status, data = df_post)
cat("\n=== MULTIPLE LINEAR REGRESSION RESULTS (POST-INSUFFLATION) ===\n")
summary(model_post)
cat("\nCONFIDENCE INTERVALS FOR MODEL COEFFICIENTS:\n")
confint(model_post)

cat("\n=== MODEL CHECKS ===\n")
# 1. Check for multicollinearity (VIF)
vif_values <- vif(model_post)
cat("\nVariance Inflation Factors (VIF):\n")
print(vif_values)

# 2. Residual plots
par(mfrow = c(2, 2))
plot(model_post, main = "Model Diagnostic Plots")
par(mfrow = c(1, 1))

#=============================================================================
# ANCOVA, Estimated Marginal Means (EMMs) and Tukey-adjusted Pairwise Comparisons
#=============================================================================
# ANCOVA from the multivariate model (adjusted)
model_anova <- Anova(model_post)
cat("\n=== TYPE 2 ANCOVA FROM MULTIVARIATE MODEL ===\n")
print(model_anova)

# EMMs
cat("\n=== ESTIMATED MARGINAL MEANS ANALYSIS RESULTS ===\n")
emm_afs <- emmeans(model_post, ~ afs_grade)
print(emm_afs)

cat("\nPAIRWISE COMPARISONS (TUKEY ADJUSTMENT):\n")
emm_pairs <- pairs(emm_afs, adjust = "tukey")
print(emm_pairs)
confint(emm_pairs)

cat("\nSUMMARY OF SIGNIFICANT PAIRWISE DIFFERENCES:\n")
sig_pairs <- summary(emm_pairs)
sig_pairs$significant <- sig_pairs$p.value < 0.05
print(sig_pairs[, c("contrast", "estimate", "SE", "p.value", "significant")])