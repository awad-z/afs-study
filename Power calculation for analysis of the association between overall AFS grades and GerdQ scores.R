# Power calculation for AFS grade effect in multiple regression model
# Model: Outcome ~ AFS_grade + Age + Sex + BMI + PPI_status

library(pwr)

u <- 3           # numerator df for AFS grades (4 levels - 1 = 3)
f2 <- 0.08       # assuming a small-to-medium effect size
power <- 0.80
sig.level <- 0.05

# Calculate required denominator df
result <- pwr.f2.test(u = u, f2 = f2, sig.level = sig.level, power = power)
print(result)

v <- ceiling(result$v)
covariate_df <- 5
total_n <- v + u + covariate_df + 1

cat("\n=== SAMPLE SIZE CALCULATION ===\n")
cat("Denominator df (v):", v, "\n")
cat("Numerator df (u; AFS grade):", u, "\n")
cat("Covariate df:", covariate_df, "\n")
cat("  - Age: 1 df\n")
cat("  - Sex: 1 df\n")
cat("  - BMI: 1 df\n")
cat("  - PPI status: 2 df\n")
cat("\nTotal sample size needed:", total_n, "\n")
cat("Breakdown: N = v (", v, ") + u (", u, ") + covariates (", covariate_df, ") + 1 = ", total_n, "\n")
