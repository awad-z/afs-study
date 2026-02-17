setwd("~/WORKING_DIRECTORY_FOLDER_NAME")

library(readr)
library(irr)
library(boot)

#===================================================================
#Load datasets with verifications
#===================================================================
# F Component (Flap Valve)
cat("\n--- F COMPONENT (FLAP VALVE) ---\n")
flap <- read_csv("CSV_FILE_FOR_FLAP_VALVE_RESULTS")
for (col in colnames(flap)) {
  flap[[col]] <- factor(flap[[col]], levels = c("Absent", "Present"))#converts to factors
}
cat("Dataset dimensions:", nrow(flap), "rows,", ncol(flap), "columns\n")
cat("DATA STRUCTURE:\n")
str(flap)
cat("\nFIRST FEW ROWS:\n")
print(head(flap))
cat("\nMISSING DATA SUMMARY:\n")
print(colSums(is.na(flap)))
cat("\nUnique values per rater (should be 0/1 or similar for absent/present):\n")
lapply(flap, function(x) sort(unique(x)))

# D Component (Diameter)
cat("\n--- D COMPONENT (DIAMETER) ---\n")
diameter <- read_csv("CSV_FILE_NAME_FOR_DIAMETER_RESULTS")
cat("Dataset dimensions:", nrow(diameter), "rows,", ncol(diameter), "columns\n")
cat("DATA STRUCTURE:\n")
str(diameter)
cat("\nFIRST FEW ROWS:\n")
print(head(diameter))
cat("\nMISSING DATA SUMMARY:\n")
print(colSums(is.na(diameter)))
cat("\nSUMMARY STATISTICS:\n")
print(summary(diameter))

# L Component (Length)
cat("\n--- L COMPONENT (AXIAL LENGTH) ---\n")
length <- read_csv("CSV_FILE_FOR_LENGTH_RESULTS")
cat("Dataset dimensions:", nrow(length), "rows,", ncol(length), "columns\n")
cat("DATA STRUCTURE:\n")
str(length)
cat("\nFIRST FEW ROWS:\n")
print(head(length))
cat("\nMISSING DATA SUMMARY:\n")
print(colSums(is.na(length)))
cat("\nSUMMARY STATISTICS:\n")
print(summary(length))

#====================================================================
# F component (Fleiss' Kappa)
#====================================================================
cat("BASIC FLEISS' KAPPA:\n")
kappam.fleiss(flap, detail = TRUE)

cat("FLEISS' KAPPA WITH 95% CI:\n")
library(DescTools)
KappaM(flap)
fleiss_kappa_result <- KappaM(flap, conf.level = 0.95)
print(fleiss_kappa_result)

#====================================================================
# Diagnostic plots for D and L components to check normality assumptions
#====================================================================

# D Component (Diameter) Diagnostics
cat("\n--- DIAGNOSTIC PLOTS FOR DIAMETER DATA ---\n")

# Calculate diameter residuals from the mean for each case
diameter_residuals <- as.vector(as.matrix(diameter) - rowMeans(diameter, na.rm = TRUE))
diameter_residuals <- diameter_residuals[!is.na(diameter_residuals)]

par(mfrow = c(2, 2))

hist(diameter_residuals, 
     main = "Histogram of Residuals (Diameter)",
     xlab = "Residuals", 
     breaks = "Sturges",
     col = "lightblue",
     border = "darkblue")

qqnorm(diameter_residuals, main = "Q-Q Plot (Diameter)")
qqline(diameter_residuals)

boxplot(diameter_residuals, 
        main = "Boxplot of Residuals (Diameter)",
        ylab = "Residuals",
        col = "lightblue")

plot(density(diameter_residuals), 
     main = "Density Plot of Residuals (Diameter)",
     xlab = "Residuals",
     col = "darkblue",
     lwd = 2)

par(mfrow = c(1, 1))

# L Component (Length) Diagnostics
cat("\n--- DIAGNOSTIC PLOTS FOR LENGTH DATA ---\n")

# Calculate length residuals from the mean for each case
length_residuals <- as.vector(as.matrix(length) - rowMeans(length, na.rm = TRUE))
length_residuals <- length_residuals[!is.na(length_residuals)]

par(mfrow = c(2, 2))

hist(length_residuals, 
     main = "Histogram of Residuals (Length)",
     xlab = "Residuals", 
     breaks = "Sturges",
     col = "lightgreen",
     border = "darkgreen")

qqnorm(length_residuals, main = "Q-Q Plot (Length)")
qqline(length_residuals)

boxplot(length_residuals, 
        main = "Boxplot of Residuals (Length)",
        ylab = "Residuals",
        col = "lightgreen")

plot(density(length_residuals), 
     main = "Density Plot of Residuals (Length)",
     xlab = "Residuals",
     col = "darkgreen",
     lwd = 2)

par(mfrow = c(1, 1))

cat("\n=== NORMALITY ASSUMPTIONS VIOLATED (PARTICULARLY FOR THE D COMPONENT) - PROCEEDING WITH BOOTSTRAP ===\n")
#====================================================================
# BOOTSTRAP FUNCTION FOR ICC (SINGLE-RATING, ABSOLUTE AGREEMENT, TWO-WAY RANDOM EFFECTS)
#====================================================================

icc_bootstrap_agreement <- function(data, indices) {
  boot_data <- data[indices, ]
  icc_result <- icc(boot_data, model = "twoway", type = "agreement", unit = "single")
  return(icc_result$value)
}

#====================================================================
# ICC ANALYSIS WITH BOOTSTRAP
#====================================================================

bootstrap_icc_analysis_agreement <- function(data, component_name) {
  
  cat("=== ", component_name, " ===\n")
  
  # Original ICC calculation with absolute agreement
  original_icc <- icc(data, model = "twoway", type = "agreement", unit = "single")
  
  cat("Original ICC Results (Absolute Agreement):\n")
  print(original_icc)
  
  # Bootstrap ICC with 10,000 iterations
  cat("\nCalculating bootstrap confidence intervals (BCa method)...\n")
  set.seed(123)
  boot_result <- boot(data, icc_bootstrap_agreement, R = 10000)
  
  # Calculate BCa confidence intervals with fallback to percentile if BCa fails
  boot_ci <- boot.ci(boot_result, type = c("bca", "perc"))
  
  # Extract values with robust handling
  icc_value <- original_icc$value
  p_value <- original_icc$p.value
  
  # Try BCa first, fallback to percentile if BCa returns NA
  if (!is.null(boot_ci$bca) && !any(is.na(boot_ci$bca[4:5]))) {
    ci_lower <- boot_ci$bca[4]
    ci_upper <- boot_ci$bca[5]
    ci_method <- "BCa"
  } else {
    cat("BCa calculation failed, using percentile method as fallback...\n")
    ci_lower <- boot_ci$percent[4]
    ci_upper <- boot_ci$percent[5]
    ci_method <- "percentile"
  }
  
  # Bootstrap diagnostics
  boot_mean <- mean(boot_result$t)
  boot_bias <- boot_mean - boot_result$t0
  boot_sd <- sd(boot_result$t)
  
  # Interpretation
  interpretation <- if(icc_value < 0.5) {
    "Poor reliability"
  } else if(icc_value < 0.75) {
    "Moderate reliability"
  } else if(icc_value < 0.9) {
    "Good reliability"
  } else {
    "Excellent reliability"
  }
  
  # Results summary
  cat("\nBootstrap ICC Summary (Absolute Agreement,", ci_method, "CI):\n")
  cat("ICC =", round(icc_value, 4), "\n")
  cat("Bootstrap 95%", ci_method, "CI: [", round(ci_lower, 4), ",", round(ci_upper, 4), "]\n")
  cat("p-value =", format(p_value, scientific = TRUE), "\n")
  cat("Interpretation:", interpretation, "\n")
  
  cat("\nBootstrap Diagnostics:\n")
  cat("Bootstrap mean:", round(boot_mean, 4), "\n")
  cat("Bootstrap bias:", round(boot_bias, 4), "\n")
  cat("Bootstrap SD:", round(boot_sd, 4), "\n")
  
  cat("\n============================================================\n\n")
  
  return(list(
    icc = icc_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    interpretation = interpretation,
    boot_bias = boot_bias,
    boot_sd = boot_sd,
    boot_result = boot_result,
    ci_method = ci_method
  ))
}

#====================================================================
# BOOTSTRAP DISTRIBUTION VISUALIASTION
#====================================================================

# Set up plotting area for bootstrap visualisations
par(mfrow = c(1, 2))
diameter_results <- bootstrap_icc_analysis_agreement(diameter, "D Component (Diameter)")
length_results <- bootstrap_icc_analysis_agreement(length, "L Component (Length)")

# Visualise bootstrap distributions
hist(diameter_results$boot_result$t,
     main = "Bootstrap Distribution: D Component",
     xlab = "ICC",
     breaks = 30,
     col = "lightblue",
     border = "darkblue")
abline(v = diameter_results$icc, col = "red", lwd = 2, lty = 2)
abline(v = c(diameter_results$ci_lower, diameter_results$ci_upper), col = "blue", lwd = 2)
legend("topleft", c("Original ICC", "95% BCa CI"), col = c("red", "blue"), lty = c(2, 1), lwd = 2, cex = 0.8)

hist(length_results$boot_result$t,
     main = "Bootstrap Distribution: L Component", 
     xlab = "ICC",
     breaks = 30,
     col = "lightgreen",
     border = "darkgreen")
abline(v = length_results$icc, col = "red", lwd = 2, lty = 2)
abline(v = c(length_results$ci_lower, length_results$ci_upper), col = "blue", lwd = 2)
legend("topleft", c("Original ICC", "95% BCa CI"), col = c("red", "blue"), lty = c(2, 1), lwd = 2, cex = 0.8)

# Reset plotting
par(mfrow = c(1, 1))
