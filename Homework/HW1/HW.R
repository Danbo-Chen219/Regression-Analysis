########################################
rm(list = ls())
cat("\14")

library(tidyverse)
library(boot)
data <- read.csv("housepricedata.csv")

model <- lm(lprice ~ llotsize + lsqrft + bdrms, data = data)

summary(model)

boot_fn <- function(data, indices) {
  d <- data[indices, ] # Resample data
  coef(lm(lprice ~ llotsize + lsqrft + bdrms, data = d))
}

set.seed(123)
boot_results <- boot(data, boot_fn, R = 1000)

boot_se <- apply(boot_results$t, 2, sd)

results_table <- data.frame(
  Coefficients = coef(model),
  `Bootstrapped SE` = boot_se
)

results_table <- cbind(
  results_table,
  `P-Value` = summary(model)$coefficients[, 4],
  `R-squared` = summary(model)$r.squared
)
# `P-Value` = summary(model)$coefficients[, 4]
# Add goodness of fit directly
# results_table$`R-squared` <- summary(model)$r.squared

print(results_table)


##############################################################
rm(list = ls())
cat("\14")

library(tidyverse)
library(boot)

data <- read.csv("housepricedata.csv")

hist(data$price, main = "Distribution of Prices", xlab = "Price")
hist(data$lotsize, main = "Distribution of Lotsize", xlab = "Lotsize")
hist(data$sqrft, main = "Distribution of Sqrft", xlab = "Sqrft")
model_levels <- lm(price ~ lotsize + sqrft + bdrms, data = data)

summary(model_levels)

new_data <- data.frame(
  lotsize = 10000,
  sqrft = 2300,
  bdrms = 4
)

predicted_price <- predict(model_levels, newdata = new_data)

cat("The predicted price is:", round(predicted_price, 0), "thousand dollars\n")

predicted_with_ci <- predict(model_levels, newdata = new_data, interval = "confidence", level = 0.95)

cat("Predicted Price:", round(predicted_with_ci[1], 2), " thousand dollars\n")
cat("95% Confidence Interval: [", round(predicted_with_ci[2], 2), ", ", round(predicted_with_ci[3], 2), "] dollars\n")

# Model in Levels
model_levels <- lm(price ~ lotsize + sqrft + bdrms, data = data)
summary(model_levels)$adj.r.squared

# Model in Logs
model_logs <- lm(lprice ~ llotsize + lsqrft + bdrms, data = data)
summary(model_logs)$adj.r.squared

print("The model with the higher adjusted R-squared better explains the variation in price.")

# From residual perspectives
# Residual plot for Levels
plot(model_levels$fitted.values, model_levels$residuals,
  main = "Residuals (Levels Model)",
  xlab = "Fitted Values", ylab = "Residuals"
)
abline(h = 0, col = "red")

# Residual plot for Logs
plot(model_logs$fitted.values, model_logs$residuals,
  main = "Residuals (Logs Model)",
  xlab = "Fitted Values", ylab = "Residuals"
)
abline(h = 0, col = "red")

print("Since the residuals from the logs model show more consistent variance and a better fit, the logs model is preferred")

##########################################
rm(list = ls())
cat("\14")

library(tidyverse)
library(boot)
library(stargazer)
install.packages("xtable")
data <- read.csv("epadata.csv")

# Split the data into participants (pstatus = 1) and non-participants (pstatus = 0)
participants <- subset(data, pstatus == 1)
non_participants <- subset(data, pstatus == 0)

str(data)
colnames(data)

# List of variables to analyze (excluding "pstatus" and "Co_name")
variables <- c(colnames(data)[2:11])
# print(variables)
# Initialize a results table
results <- data.frame(
  Variable = character(),
  Mean_Participants = numeric(),
  Mean_Non_Participants = numeric(),
  P_Value = numeric(),
  Significant = logical()
)


# Loop through each variable
for (var in variables) {
  # Compute means

  mean_participants <- mean(participants[[var]])
  mean_non_participants <- mean(non_participants[[var]])

  # Perform t-test
  t_test <- t.test(participants[[var]], non_participants[[var]])

  # Store results
  results <- rbind(results, data.frame(
    Variable = var,
    Mean_Participants = mean_participants,
    Mean_Non_Participants = mean_non_participants,
    P_Value = t_test$p.value,
    Significant = t_test$p.value < 0.05 # Significance level at 5%
  ))
}


# Print the results table
print(results)

# Save results to a CSV file
# write.csv(results, "results_comparison_table2.csv", row.names = FALSE)

# Create squared terms for Fac and Empl
data$fac_sq <- data$fac^2
data$empl_sq <- data$empl^2

# Fit the OLS model
model <- lm(release ~ fac + fac_sq + herf + empl + empl_sq + fg + strictbar +
  educbar + lawbar + spendbar + pstatus, data = data)

summary(model)

# Generate a professionally styled table
stargazer(model,
  type = "text",
  title = "Pollution Equation OLS Estimation",
  dep.var.labels = "release",
  covariate.labels = c(
    "fac", "fac_sq", "herf", "empl", "empl_sq",
    "fg", "strictbar", "educbar", "lawbar",
    "spendbar", "pstatus"
  ),
  align = TRUE
)
