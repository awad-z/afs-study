setwd("~/WORKING_DIRECTORY_FOLDER_NAME")

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(car)
library(scales)

# Read data
df_conclusive <- read.csv("CSV_FILE_FOR_CONCLUSIVE_GERD_STATUS_RESULTS")

# Convert appropriate variables to factors
df_conclusive$afs_grade <- as.factor(df_conclusive$afs_grade)
df_conclusive$sex <- as.factor(df_conclusive$sex)
df_conclusive$indication <- as.factor(df_conclusive$indication)
df_conclusive$oesophagitis <- as.factor(df_conclusive$oesophagitis)
df_conclusive$ppi_use <- as.factor(df_conclusive$ppi_use)
df_conclusive$ppi_continued <- as.factor(df_conclusive$ppi_continued)
df_conclusive$ppi_response <- as.factor(df_conclusive$ppi_response)
df_conclusive$conclusive <- as.factor(df_conclusive$conclusive)

# Create labeled conclusive variable (2 categories only)
df_conclusive$gerd_status <- case_when(
  df_conclusive$conclusive == "0" ~ "Conclusive NO GERD",
  df_conclusive$conclusive == "1" ~ "Conclusive GERD",
  TRUE ~ NA_character_
)
df_conclusive$gerd_status <- factor(df_conclusive$gerd_status,
                                    levels = c("Conclusive NO GERD", "Conclusive GERD"))

# Create PPI status variable
df_conclusive$ppi_status <- case_when(
  df_conclusive$ppi_use == "0" ~ "No PPI",
  df_conclusive$ppi_use %in% c("1", "2") & df_conclusive$ppi_continued == "0" ~ "PPI stopped",
  df_conclusive$ppi_use %in% c("1", "2") & df_conclusive$ppi_continued == "1" ~ "PPI continued",
  TRUE ~ NA_character_
)
df_conclusive$ppi_status <- factor(df_conclusive$ppi_status, 
                                   levels = c("No PPI", "PPI stopped", "PPI continued"))

#=============================================================================
# OVERALL DEMOGRAPHICS
#=============================================================================

cat("=== COMPREHENSIVE DEMOGRAPHICS - CONCLUSIVE GERD ANALYSIS ===\n\n")

cat("OVERALL SAMPLE CHARACTERISTICS:\n")
cat("===============================\n")
cat("Total sample size:", nrow(df_conclusive), "\n")
cat("Age: Mean =", round(mean(df_conclusive$age, na.rm = TRUE), 1), 
    "± SD =", round(sd(df_conclusive$age, na.rm = TRUE), 1), "years\n")
cat("Age: Median =", median(df_conclusive$age, na.rm = TRUE), 
    "(IQR:", quantile(df_conclusive$age, 0.25, na.rm = TRUE), "-", 
    quantile(df_conclusive$age, 0.75, na.rm = TRUE), ")\n")
cat("BMI: Mean =", round(mean(df_conclusive$bmi, na.rm = TRUE), 1), 
    "± SD =", round(sd(df_conclusive$bmi, na.rm = TRUE), 1), "\n")
cat("BMI: Median =", round(median(df_conclusive$bmi, na.rm = TRUE), 1), 
    "(IQR:", round(quantile(df_conclusive$bmi, 0.25, na.rm = TRUE), 1), "-", 
    round(quantile(df_conclusive$bmi, 0.75, na.rm = TRUE), 1), ")\n")

# Sex distribution
sex_table <- table(df_conclusive$sex, useNA = "ifany")
sex_props <- prop.table(sex_table) * 100
cat("Sex: Female =", sex_table["F"], "(", round(sex_props["F"], 1), "%),", 
    "Male =", sex_table["M"], "(", round(sex_props["M"], 1), "%)\n")

# GERDQ scores
cat("GERDQ Score: Mean =", round(mean(df_conclusive$gerdq_score, na.rm = TRUE), 1), 
    "± SD =", round(sd(df_conclusive$gerdq_score, na.rm = TRUE), 1), "\n")
cat("GERDQ Score: Median =", median(df_conclusive$gerdq_score, na.rm = TRUE), 
    "(IQR:", quantile(df_conclusive$gerdq_score, 0.25, na.rm = TRUE), "-", 
    quantile(df_conclusive$gerdq_score, 0.75, na.rm = TRUE), ")\n\n")

#=============================================================================
# GERD STATUS DISTRIBUTION
#=============================================================================

cat("GERD STATUS DISTRIBUTION:\n")
cat("========================\n")
gerd_status_table <- table(df_conclusive$gerd_status, useNA = "ifany")
gerd_status_props <- prop.table(gerd_status_table) * 100

for(i in 1:length(gerd_status_table)) {
  cat(names(gerd_status_table)[i], ":", gerd_status_table[i], 
      "(", round(gerd_status_props[i], 1), "%)\n")
}
cat("\n")

#=============================================================================
# AFS GRADE DISTRIBUTION
#=============================================================================

cat("AFS GRADE DISTRIBUTION:\n")
cat("=======================\n")

# Overall AFS distribution
afs_overall <- table(df_conclusive$afs_grade, useNA = "ifany")
afs_overall_props <- prop.table(afs_overall) * 100
cat("Overall AFS Grade Distribution:\n")
for(i in 1:length(afs_overall)) {
  cat("Grade", names(afs_overall)[i], ":", afs_overall[i], 
      "(", round(afs_overall_props[i], 1), "%)\n")
}

# AFS by GERD status
cat("\nAFS Grade by GERD Status:\n")
afs_by_gerd <- table(df_conclusive$afs_grade, df_conclusive$gerd_status)
print(afs_by_gerd)

cat("\nRow percentages (within each AFS grade):\n")
afs_by_gerd_row <- round(prop.table(afs_by_gerd, 1) * 100, 1)
print(afs_by_gerd_row)

cat("\nColumn percentages (within each GERD status):\n")
afs_by_gerd_col <- round(prop.table(afs_by_gerd, 2) * 100, 1)
print(afs_by_gerd_col)
cat("\n")

#=============================================================================
# DEMOGRAPHICS BY GERD STATUS
#=============================================================================

cat("=== DETAILED DEMOGRAPHICS BY GERD STATUS ===\n\n")

# Create summary table
demo_by_gerd <- df_conclusive %>%
  group_by(gerd_status) %>%
  summarise(
    n = n(),
    age_mean = round(mean(age, na.rm = TRUE), 1),
    age_sd = round(sd(age, na.rm = TRUE), 1),
    age_median = median(age, na.rm = TRUE),
    age_q1 = quantile(age, 0.25, na.rm = TRUE),
    age_q3 = quantile(age, 0.75, na.rm = TRUE),
    bmi_mean = round(mean(bmi, na.rm = TRUE), 1),
    bmi_sd = round(sd(bmi, na.rm = TRUE), 1),
    bmi_median = round(median(bmi, na.rm = TRUE), 1),
    bmi_q1 = round(quantile(bmi, 0.25, na.rm = TRUE), 1),
    bmi_q3 = round(quantile(bmi, 0.75, na.rm = TRUE), 1),
    gerdq_mean = round(mean(gerdq_score, na.rm = TRUE), 1),
    gerdq_sd = round(sd(gerdq_score, na.rm = TRUE), 1),
    gerdq_median = median(gerdq_score, na.rm = TRUE),
    gerdq_q1 = quantile(gerdq_score, 0.25, na.rm = TRUE),
    gerdq_q3 = quantile(gerdq_score, 0.75, na.rm = TRUE),
    pct_female = round(mean(sex == "F", na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  )

print(demo_by_gerd)

# Individual group summaries
for(status in levels(df_conclusive$gerd_status)) {
  if(sum(df_conclusive$gerd_status == status, na.rm = TRUE) > 0) {
    subset_data <- df_conclusive[df_conclusive$gerd_status == status & !is.na(df_conclusive$gerd_status), ]
    
    cat("\n", status, " (n =", nrow(subset_data), "):\n")
    cat(paste(rep("=", nchar(status) + 10), collapse = ""), "\n")
    cat("Age: Mean =", round(mean(subset_data$age, na.rm = TRUE), 1), 
        "± SD =", round(sd(subset_data$age, na.rm = TRUE), 1), 
        ", Median =", median(subset_data$age, na.rm = TRUE), 
        "(IQR:", quantile(subset_data$age, 0.25, na.rm = TRUE), "-", 
        quantile(subset_data$age, 0.75, na.rm = TRUE), ")\n")
    cat("BMI: Mean =", round(mean(subset_data$bmi, na.rm = TRUE), 1), 
        "± SD =", round(sd(subset_data$bmi, na.rm = TRUE), 1), 
        ", Median =", round(median(subset_data$bmi, na.rm = TRUE), 1), 
        "(IQR:", round(quantile(subset_data$bmi, 0.25, na.rm = TRUE), 1), "-", 
        round(quantile(subset_data$bmi, 0.75, na.rm = TRUE), 1), ")\n")
    cat("GERDQ: Mean =", round(mean(subset_data$gerdq_score, na.rm = TRUE), 1), 
        "± SD =", round(sd(subset_data$gerdq_score, na.rm = TRUE), 1), 
        ", Median =", median(subset_data$gerdq_score, na.rm = TRUE), 
        "(IQR:", quantile(subset_data$gerdq_score, 0.25, na.rm = TRUE), "-", 
        quantile(subset_data$gerdq_score, 0.75, na.rm = TRUE), ")\n")
    
    # Sex distribution within group
    sex_in_group <- table(subset_data$sex)
    sex_props_group <- prop.table(sex_in_group) * 100
    cat("Sex: Female =", sex_in_group["F"], "(", round(sex_props_group["F"], 1), "%),", 
        "Male =", sex_in_group["M"], "(", round(sex_props_group["M"], 1), "%)\n")
  }
}

#=============================================================================
# INDICATION ANALYSIS
#=============================================================================

cat("\n\n=== INDICATION ANALYSIS ===\n")

# Overall indication distribution
cat("OVERALL INDICATION DISTRIBUTION:\n")
indication_table <- table(df_conclusive$indication, useNA = "ifany")
indication_props <- prop.table(indication_table) * 100
for(i in 1:length(indication_table)) {
  cat("Indication", names(indication_table)[i], ":", indication_table[i], 
      "(", round(indication_props[i], 1), "%)\n")
}

# Indication by GERD status
cat("\nINDICATION BY GERD STATUS:\n")
indication_by_gerd <- table(df_conclusive$indication, df_conclusive$gerd_status)
print(indication_by_gerd)

cat("\nRow percentages (within each indication):\n")
indication_by_gerd_row <- round(prop.table(indication_by_gerd, 1) * 100, 1)
print(indication_by_gerd_row)
cat("Indications: 1=Reflux, 2=Other oesophageal, 3=Unrelated, 4=Laryngo-pharyngeal\n\n")

#=============================================================================
# OESOPHAGITIS ANALYSIS
#=============================================================================

cat("=== OESOPHAGITIS ANALYSIS ===\n")

# Overall oesophagitis distribution
cat("OVERALL OESOPHAGITIS DISTRIBUTION:\n")
oesoph_table <- table(df_conclusive$oesophagitis, useNA = "ifany")
oesoph_props <- prop.table(oesoph_table) * 100
for(i in 1:length(oesoph_table)) {
  grade_name <- ifelse(names(oesoph_table)[i] == "0", "No oesophagitis", 
                       paste("LA Grade", names(oesoph_table)[i]))
  cat(grade_name, ":", oesoph_table[i], "(", round(oesoph_props[i], 1), "%)\n")
}

# Oesophagitis by GERD status
cat("\nOESOPHAGITIS BY GERD STATUS:\n")
oesoph_by_gerd <- table(df_conclusive$oesophagitis, df_conclusive$gerd_status)
print(oesoph_by_gerd)

cat("\nColumn percentages (within each GERD status):\n")
oesoph_by_gerd_col <- round(prop.table(oesoph_by_gerd, 2) * 100, 1)
print(oesoph_by_gerd_col)
cat("\n")

#=============================================================================
# PPI ANALYSIS
#=============================================================================

cat("=== PPI USAGE ANALYSIS ===\n")

# Overall PPI status
cat("OVERALL PPI STATUS DISTRIBUTION:\n")
ppi_status_table <- table(df_conclusive$ppi_status, useNA = "ifany")
ppi_status_props <- prop.table(ppi_status_table) * 100
for(i in 1:length(ppi_status_table)) {
  cat(names(ppi_status_table)[i], ":", ppi_status_table[i], 
      "(", round(ppi_status_props[i], 1), "%)\n")
}

# PPI status by GERD status
cat("\nPPI STATUS BY GERD STATUS:\n")
ppi_by_gerd <- table(df_conclusive$ppi_status, df_conclusive$gerd_status)
print(ppi_by_gerd)

cat("\nColumn percentages (within each GERD status):\n")
ppi_by_gerd_col <- round(prop.table(ppi_by_gerd, 2) * 100, 1)
print(ppi_by_gerd_col)

# Among PPI users, analyze response patterns
ppi_users_conclusive <- df_conclusive[!is.na(df_conclusive$ppi_response), ]
if(nrow(ppi_users_conclusive) > 0) {
  cat("\nPPI RESPONSE AMONG USERS BY GERD STATUS:\n")
  ppi_response_by_gerd <- table(ppi_users_conclusive$ppi_response, ppi_users_conclusive$gerd_status)
  print(ppi_response_by_gerd)
  
  cat("\nColumn percentages (within each GERD status, among PPI users only):\n")
  ppi_response_by_gerd_col <- round(prop.table(ppi_response_by_gerd, 2) * 100, 1)
  print(ppi_response_by_gerd_col)
  cat("Response levels: 0=No response, 1=Partial response, 2=Full response\n")
}
