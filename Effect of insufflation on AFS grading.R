setwd("~/WORKING_DIRECTORY_FOLDER_NAME")

library(ggplot2)
library(dplyr)
library(broom)
library(effectsize)
library(emmeans)
library(car)

# Read data
df_initial <- read.csv("CSV_FILE_NAME_FOR_INITIAL_INSUFFLATION_RESULTS")
str(df_initial)

# Convert appropriate variables to factors (categorical)
df_initial$afs_grade       <- as.factor(df_initial$afs_grade)
df_initial$sex             <- as.factor(df_initial$sex)
df_initial$indication      <- as.factor(df_initial$indication)
df_initial$oesophagitis    <- as.factor(df_initial$oesophagitis)
df_initial$ppi_use         <- as.factor(df_initial$ppi_use)
df_initial$ppi_continued   <- as.factor(df_initial$ppi_continued)
df_initial$ppi_response    <- as.factor(df_initial$ppi_response)

# Create composite PPI status variable
df_initial$ppi_status <- case_when(
  df_initial$ppi_use == "0" ~ "No PPI",
  df_initial$ppi_use %in% c("1", "2") & df_initial$ppi_continued == "0" ~ "PPI stopped",
  df_initial$ppi_use %in% c("1", "2") & df_initial$ppi_continued == "1" ~ "PPI continued",
  TRUE ~ NA_character_
)
df_initial$ppi_status <- factor(df_initial$ppi_status,
                                levels = c("No PPI", "PPI stopped", "PPI continued"))#makes "No PPI" the reference category for mlr

# ============================================================
# Descriptive Statistics (overall)
# ============================================================

cat("=== DEMOGRAPHIC AND CLINICAL CHARACTERISTICS (INITIAL INSUFFLATION) ===\n\n")

cat("OVERALL SAMPLE CHARACTERISTICS:\n")
cat("Total sample size:", nrow(df_initial), "\n")
cat("Median age:", median(df_initial$age, na.rm = TRUE), "years\n")
cat("Age IQR:", quantile(df_initial$age, 0.25, na.rm = TRUE), "-", 
    quantile(df_initial$age, 0.75, na.rm = TRUE), "years\n")
cat("Age range:", min(df_initial$age, na.rm = TRUE), "-", max(df_initial$age, na.rm = TRUE), "years\n")
cat("Median BMI:", round(median(df_initial$bmi, na.rm = TRUE), 1), "\n")
cat("BMI IQR:", round(quantile(df_initial$bmi, 0.25, na.rm = TRUE), 1), "-",
    round(quantile(df_initial$bmi, 0.75, na.rm = TRUE), 1), "\n")
cat("Median GERDQ Score:", round(median(df_initial$gerdq_score, na.rm = TRUE), 2), "\n")
cat("GERDQ Score IQR:", round(quantile(df_initial$gerdq_score, 0.25, na.rm = TRUE), 2), "-",
    round(quantile(df_initial$gerdq_score, 0.75, na.rm = TRUE), 2), "\n")
sex_count <- sum(df_initial$sex == "F", na.rm = TRUE)
sex_prop <- round(mean(df_initial$sex == "F", na.rm = TRUE) * 100, 1)
cat("Percentage of Females:", sex_count, "(", sex_prop, "%)\n\n")

# Indication breakdown
cat("INDICATION FOR PROCEDURE:\n")
indication_table  <- table(df_initial$indication, useNA = "ifany")
indication_props  <- prop.table(indication_table) * 100
for (i in 1:length(indication_table)) {
  cat("Indication", names(indication_table)[i], ":", indication_table[i],
      "(", round(indication_props[i], 1), "%)\n")
}
cat("\n")

# Oesophagitis findings
cat("OESOPHAGITIS FINDINGS:\n")
oesoph_table <- table(df_initial$oesophagitis, useNA = "ifany")
oesoph_props <- prop.table(oesoph_table) * 100
for (i in 1:length(oesoph_table)) {
  grade_name <- ifelse(names(oesoph_table)[i] == "0", "No oesophagitis",
                       paste("LA Grade", names(oesoph_table)[i]))
  cat(grade_name, ":", oesoph_table[i], "(", round(oesoph_props[i], 1), "%)\n")
}
cat("\n")

# PPI Usage Analysis
cat("PPI USAGE PATTERNS:\n")
ppi_use_table <- table(df_initial$ppi_use, useNA = "ifany")
ppi_use_props <- prop.table(ppi_use_table) * 100
cat("No PPI use:", ppi_use_table["0"], "(", round(ppi_use_props["0"], 1), "%)\n")
cat("Intermittent PPI use:", ppi_use_table["1"], "(", round(ppi_use_props["1"], 1), "%)\n")
cat("Daily PPI use:", ppi_use_table["2"], "(", round(ppi_use_props["2"], 1), "%)\n\n")

# Among PPI users only
ppi_users <- df_initial[df_initial$ppi_use %in% c("1", "2"), ]#defines ppi_users as intermittent(1) OR daily(2) users
if (nrow(ppi_users) > 0) {
  cat("AMONG PPI USERS (n =", nrow(ppi_users), "):\n")
  
  # Continuation patterns
  continued_table <- table(ppi_users$ppi_continued, useNA = "ifany")
  continued_props <- prop.table(continued_table) * 100
  cat("Stopped PPI before procedure:", continued_table["0"], "(", round(continued_props["0"], 1), "%)\n")
  cat("Continued PPI through procedure:", continued_table["1"], "(", round(continued_props["1"], 1), "%)\n\n")
  
  # Response patterns
  response_table  <- table(ppi_users$ppi_response, useNA = "ifany")
  response_props  <- prop.table(response_table) * 100
  cat("No response:",   response_table["0"], "(", round(response_props["0"], 1), "%)\n")
  cat("Partial response:", response_table["1"], "(", round(response_props["1"], 1), "%)\n")
  cat("Full response:",    response_table["2"], "(", round(response_props["2"], 1), "%)\n\n")
}

# ---- PPI Status (composite variable) ----
cat("PPI STATUS (COMPOSITE VARIABLE):\n")
ppi_status_table <- table(df_initial$ppi_status, useNA = "ifany")
ppi_status_props <- prop.table(ppi_status_table) * 100
for (i in 1:length(ppi_status_table)) {
  cat(names(ppi_status_table)[i], ":", ppi_status_table[i],
      "(", round(ppi_status_props[i], 1), "%)\n")
}
cat("\n")

# ============================================================
# Statistical Tests (unadjusted analysis; initial insufflation)
# ============================================================

# Levene's test for homogeneity of variance
levene_test_initial <- leveneTest(gerdq_score ~ afs_grade, data = df_initial)
cat("\nLevene's Test for Homogeneity of Variance (Initial Insufflation):\n")
print(levene_test_initial)

# One-way ANOVA (unadjusted)
anova_unadj_initial <- aov(gerdq_score ~ afs_grade, data = df_initial)
anova_summary_unadj_initial <- summary(anova_unadj_initial)
cat("\n=== ONE-WAY ANOVA (UNADJUSTED) — Initial Insufflation ===\n")
print(anova_summary_unadj_initial)

# ============================================================
# Multiple Linear Regression (adjusted analysis; initial insufflation)
# ============================================================
model_initial <- lm(gerdq_score ~ afs_grade + age + sex + bmi + ppi_status, data = df_initial)
cat("\n=== MULTIPLE LINEAR REGRESSION RESULTS (Initial Insufflation; with PPI STATUS) ===\n")
summary(model_initial)
cat("\nCONFIDENCE INTERVALS FOR MODEL COEFFICIENTS:\n")
print(confint(model_initial))

cat("\n=== MODEL ASSUMPTION CHECKS (Initial) ===\n")
# 1. Check for multicollinearity (VIF)
vif_values_initial <- vif(model_initial)
cat("Variance Inflation Factors (VIF):\n")
print(vif_values_initial)

# 2. Residual plots
par(mfrow = c(2, 2))
plot(model_initial, main = "Model Diagnostic Plots (Initial)")
par(mfrow = c(1, 1))

# ============================================================
# ANCOVA
# ============================================================
# ANCOVA from the multivariate model (adjusted)
model_anova_initial <- anova(model_initial)
cat("\n=== TYPE 2 ANCOVA FROM MULTIVARIATE MODEL — Initial Insufflation ===\n")
print(model_anova_initial)
