# Load data
IQdata <- read.table("data/IQ.txt", header = TRUE)
treegrowthdata <- read.table("data/treegrowth.txt",header=TRUE)

# Load necessary libraries (if not already installed, uncomment the install.packages lines)
# install.packages("ggplot2") # For visualization (optional)
# install.packages("dplyr")    # For data manipulation (optional)

# Question 1: Five-Number Summary Statistics
# -------------------------------------------------

# Calculate the five-number summary
five_num_summary <- fivenum(IQdata$IQ)
names(five_num_summary) <- c("Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum")

# Print the five-number summary
print("Question 1: Five-Number Summary of IQ")
print(five_num_summary)

# Question 2: Identify Mild Outliers Using Inner Fences
# --------------------------------------------------------

# Calculate Q1, Q3, and IQR
Q1 <- quantile(IQdata$IQ, 0.25)
Q3 <- quantile(IQdata$IQ, 0.75)
IQR_value <- IQR(IQdata$IQ)

# Calculate inner fence limits for mild outliers
lower_fence <- Q1 - 1.5 * IQR_value
upper_fence <- Q3 + 1.5 * IQR_value

# Identify mild outliers
mild_outliers <- IQdata$IQ[IQdata$IQ < lower_fence | IQdata$IQ > upper_fence]

# Print the inner fence limits and mild outliers
print("\nQuestion 2: Inner Fence Limits for Mild Outliers")
print(paste("Lower Fence:", lower_fence))
print(paste("Upper Fence:", upper_fence))

if(length(mild_outliers) > 0){
  print("Mild Outliers:")
  print(mild_outliers)
} else {
  print("No mild outliers detected.")
}

# Question 3: 95% Confidence Interval for the Population Mean (Assuming Normality)
# -----------------------------------------------------------------------------------

# Calculate sample mean and standard error
sample_mean <- mean(IQdata$IQ)
sample_sd <- sd(IQdata$IQ)
n <- length(IQdata$IQ)
standard_error <- sample_sd / sqrt(n)

# Calculate the t-score for 95% confidence
alpha <- 0.05
t_score <- qt(1 - alpha/2, df = n - 1)

# Calculate the confidence interval
ci_lower <- sample_mean - t_score * standard_error
ci_upper <- sample_mean + t_score * standard_error

# Print the confidence interval
print("\nQuestion 3: 95% Confidence Interval for the Population Mean IQ")
print(paste("Mean IQ:", sample_mean))
print(paste("95% CI: [", ci_lower, ", ", ci_upper, "]", sep = ""))

# Alternatively, using t.test
# t_test_result <- t.test(IQdata$IQ, conf.level = 0.95)
# print(t_test_result$conf.int)

# Question 4: 95% Confidence Interval for the Population Proportion of IQ's > 100 (Wald Approximation)
# ----------------------------------------------------------------------------------------------------------

# Calculate the number of IQ > 100
count_over_100 <- sum(IQdata$IQ > 100)
p_hat <- count_over_100 / n

# Calculate the Wald confidence interval
z <- qnorm(1 - alpha/2)
wald_lower <- p_hat - z * sqrt((p_hat * (1 - p_hat)) / n)
wald_upper <- p_hat + z * sqrt((p_hat * (1 - p_hat)) / n)

# Ensure the limits are within [0,1]
wald_lower <- max(0, wald_lower)
wald_upper <- min(1, wald_upper)

# Print the Wald confidence interval
print("\nQuestion 4: 95% Wald Confidence Interval for Proportion of IQ > 100")
print(paste("Proportion (p̂):", p_hat))
print(paste("95% Wald CI: [", wald_lower, ", ", wald_upper, "]", sep = ""))

# Question 5: 95% Confidence Interval for the Population Proportion of IQ's > 100 (Quadratic Approximation)
# --------------------------------------------------------------------------------------------------------------

# Using prop.test which by default uses the Clopper-Pearson (exact) interval
prop_test_result <- prop.test(count_over_100, n, conf.level = 0.95, correct = FALSE)

# Extract the confidence interval
quad_lower <- prop_test_result$conf.int[1]
quad_upper <- prop_test_result$conf.int[2]

# Print the quadratic (default R) confidence interval
print("\nQuestion 5: 95% Quadratic (Default R) Confidence Interval for Proportion of IQ > 100")
print(paste("Proportion (p̂):", p_hat))
print(paste("95% Confidence Interval: [", quad_lower, ", ", quad_upper, "]", sep = ""))

# Question 6: Fit a Simple Linear Regression Model
# ---------------------------------------------------

# Fit the linear regression model: leafarea ~ age
model <- lm(leafarea ~ age, data = treegrowthdata)

# Extract the coefficients (alpha and beta)
coefficients <- coef(model)
alpha_hat <- coefficients[1]  # Intercept
beta_hat <- coefficients[2]   # Slope

# Extract the residual standard error (sigma)
sigma_hat <- summary(model)$sigma

# Print the point estimates
print("Question 6: Point Estimates of the Regression Model Parameters")
print(paste("Intercept (alpha):", alpha_hat))
print(paste("Slope (beta):", beta_hat))
print(paste("Residual Standard Error (sigma):", sigma_hat))

# Question 7: 95% Confidence Intervals for Intercept and Slope
# -------------------------------------------------------------

# Calculate the 95% confidence intervals using the summary of the model
conf_intervals <- confint(model, level = 0.95)

# Print the confidence intervals
print("\nQuestion 7: 95% Confidence Intervals for Intercept (alpha) and Slope (beta)")
print(conf_intervals)

# Question 8: Predicted Mean Leaf Areas and 90% Confidence Intervals for Ages 3 and 5
# -------------------------------------------------------------------------------------

# Define the new data for predictions
new_ages <- data.frame(age = c(3, 5))

# Calculate predictions with 90% confidence intervals
predictions <- predict(model, newdata = new_ages, interval = "confidence", level = 0.90)

# Print the predictions and confidence intervals
print("\nQuestion 8: Predicted Mean Leaf Areas and 90% Confidence Intervals")
print(data.frame(Age = new_ages$age, predictions))

# Explanation:
# The lengths of the confidence intervals may differ due to the variability of the predictions at different ages.
# Typically, predictions further from the mean of the predictor variable have wider confidence intervals.

# Question 9: Diagnostic Plots for the Linear Regression Model
# -------------------------------------------------------------

# Produce diagnostic plots
# Plot 1: Residuals vs Fitted
# Plot 2: Normal Q-Q
# Plot 3: Scale-Location
# Plot 4: Residuals vs Leverage

# To display all four base R diagnostic plots, uncomment the following line:
# par(mfrow = c(2, 2))
# plot(model)

# Alternatively, for a single plot at a time, you can use:
# Plot Residuals vs Fitted
plot(model$fitted.values, model$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")

# Plot Normal Q-Q
qqnorm(model$residuals, main = "Normal Q-Q")
qqline(model$residuals, col = "red")

# Plot Scale-Location
sqrt_abs_residuals <- sqrt(abs(model$residuals))
plot(model$fitted.values, sqrt_abs_residuals,
     main = "Scale-Location",
     xlab = "Fitted Values",
     ylab = "Sqrt(|Residuals|)")
abline(h = 0, col = "red")

# Plot Residuals vs Leverage
plot(model, which = 5)

# Comment:
# After generating the diagnostic plots, examine them for:
# 1. Linearity: Residuals vs Fitted should show no systematic pattern.
# 2. Normality: Q-Q plot should follow the straight line closely.
# 3. Homoscedasticity: Scale-Location plot should have a horizontal line with equally spread points.
# 4. Influential Points: Residuals vs Leverage plot should not have points with high leverage or high residuals.

# Question 10: ANOVA Table and Test if Slope Beta is Significantly Different from Zero at 1% Level
# -----------------------------------------------------------------------------------------------

# Produce the ANOVA table
anova_table <- anova(model)

# Print the ANOVA table
print("\nQuestion 10: ANOVA Table for the Regression Model")
print(anova_table)

# Extract the p-value for the slope (age)
p_value_slope <- anova_table["age", "Pr(>F)"]

# Determine if the slope is significantly different from zero at 1% level
if(p_value_slope < 0.01){
  conclusion <- "The slope (beta) is significantly different from zero at the 1% significance level."
} else {
  conclusion <- "The slope (beta) is NOT significantly different from zero at the 1% significance level."
}

# Print the conclusion
print(paste("\nConclusion:", conclusion))
