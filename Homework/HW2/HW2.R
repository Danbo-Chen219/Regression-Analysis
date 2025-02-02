########################################
{
    want <- c("tidyverse", "boot", "lmtest", "sandwich", "lmtest", "stargazer")
    need <- want[!(want %in% installed.packages()[, "Package"])]
    if (length(need)) install.packages(need, repos = "https://cloud.r-project.org/")
    lapply(want, function(i) require(i, character.only = TRUE))
    rm(need)
}
rm(list = ls())
library(tidyverse)
library(boot)
library(stargazer)
library(sandwich)
library(lmtest)
library(boot)
####################### ----Question 2------##########################
data <- read.csv("housepricedata.csv")
ols <- lm(lprice ~ llotsize + lsqrft + bdrms, data = data)
# stargazer(ols, type = "text")
boot_fn <- function(data, indices) {
    d <- data[indices, ] # Resample data
    coef(lm(lprice ~ llotsize + lsqrft + bdrms, data = d))
}
set.seed(123)
boot_results <- boot(data, boot_fn, R = 1000)
boot_se <- apply(boot_results$t, 2, sd)
stargazer(ols, type = "text", title = "OLS Regression Results")
results_table <- data.frame(
    Coefficients = coef(ols),
    `Bootstrapped SE` = boot_se,
    `P-Value` = summary(ols)$coefficients[, 4]
) %>%
    mutate(`R-squared` = summary(ols)$r.squared)
# Display results
print(results_table)
####################### ----Question 2.2------##########################
ols2 <- lm(price ~ lotsize + sqrft + bdrms, data = data)
stargazer(ols, type = "text", title = " Regression (2) Results")
boot_fn2 <- function(data, indices) {
    d2 <- data[indices, ] # Resample data
    coef(lm(price ~ lotsize + sqrft + bdrms, data = d2))
}
set.seed(123)
boot_results2 <- boot(data, boot_fn2, R = 1000)
boot_se <- apply(boot_results2$t, 2, sd)
results_table <- data.frame(
    Coefficients = coef(ols2),
    `Bootstrapped SE` = boot_se,
    `P-Value` = summary(ols2)$coefficients[, 4]
) %>%
    mutate(`R-squared` = summary(ols2)$r.squared)
print(results_table)
# Display the prediction results
new_data <- data.frame(
    lotsize = 10000,
    sqrft = 2300,
    bdrms = 4
)
predicted_price <- predict(ols2, newdata = new_data)
cat("The predicted price is:", round(predicted_price, 0), "thousand dollars\n")
####################### ----Question 2.3------##########################
predicted_with_ci <- predict(ols2, newdata = new_data, interval = "confidence", level = 0.95)
cat("Predicted Price:", round(predicted_with_ci[1], 2), " thousand dollars\n")
cat("95% Confidence Interval: [", round(predicted_with_ci[2], 2), ", ", round(predicted_with_ci[3], 2), "] thousand dollars\n")
####################### ----Question 2.4------##########################
# Model in Levels
model_levels <- lm(price ~ lotsize + sqrft + bdrms, data = data)
summary(model_levels)$adj.r.squared
# Model in Logs
model_logs <- lm(lprice ~ llotsize + lsqrft + bdrms, data = data)
summary(model_logs)$adj.r.squared
print("The model with the higher adjusted R-squared better explains the variation in price.")
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
####################### ----Question 3------##########################
rm(list = ls())
library(tidyverse)
library(boot)
library(stargazer)
library(sandwich)
library(lmtest)
library(boot)
getwd()
dirname(getwd())
data <- read.csv("epadata.csv")
# Split the data into participants (pstatus = 1) and non-participants (pstatus = 0)
participants <- data %>% filter(pstatus == 1)
non_participants <- data %>% filter(pstatus == 0)
# Variables to analyze (replace with actual variable names from your dataset)
variables <- c(colnames(data)[2:11])
# Initialize a results table
results_table <- data.frame(
    Variable = variables,
    Participant_Mean = numeric(length(variables)),
    Non_Participant_Mean = numeric(length(variables)),
    P_Value = numeric(length(variables))
)
# Loop through each variable, calculate means and p-values
for (i in seq_along(variables)) {
    var <- variables[i]
    # Calculate means
    participant_mean <- mean(participants[[var]], na.rm = TRUE)
    non_participant_mean <- mean(non_participants[[var]], na.rm = TRUE)
    # Perform t-test
    t_test <- t.test(data[[var]] ~ data$pstatus)
    # Store results
    results_table$Participant_Mean[i] <- participant_mean
    results_table$Non_Participant_Mean[i] <- non_participant_mean
    results_table$P_Value[i] <- t_test$p.value
}
# Print the final results table
stargazer(results_table, type = "text", title = "Comparison Table (Means and P-Values):", no.space = TRUE, single.row = TRUE, out = "3.1_Result.tex")
# print("Comparison Table (Means and P-Values):")
# print(results_table)
# Draw conclusions
cat("\nConclusions:\n")
for (i in 1:nrow(results_table)) {
    if (results_table$P_Value[i] < 0.05) {
        cat(paste(
            "Variable", results_table$Variable[i],
            "shows a statistically significant difference (p < 0.05).\n"
        ))
    } else {
        cat(paste(
            "Variable", results_table$Variable[i],
            "does not show a statistically significant difference (p >= 0.05).\n"
        ))
    }
}
######################## Question 3.2########################
# Create squared terms for Fac and Empl
data$facsq <- data$fac^2
data$emplsq <- data$empl^2
# Fit the OLS model
ols3 <- lm(release ~ fac + facsq + herf + empl + emplsq + fg + strictbar +
    educbar + lawbar + spendbar + pstatus, data = data)
stargazer(ols3,
    type = "text", title = "OLS Regression Results", no.space = TRUE,
    single.row = TRUE,
    out = "3.2_Result.tex"
)
clustered_se <- vcovCL(ols3, cluster = ~pstatus)
######################## Question 3.3########################
# Generate a professionally styled table using stargazer
stargazer(ols3,
    type = "text", # Use "html" or "latex" for better-formatted output
    se = list(sqrt(diag(clustered_se))), # Use clustered standard errors
    title = "OLS Regression Results with Clustered Standard Errors",
    align = TRUE,
    covariate.labels = c(
        "fac", "facsq", "herf", "empl", "emplsq",
        "fg", "strictbar", "educbar", "lawbar",
        "spendbar", "pstatus"
    ),
    dep.var.labels = c("Toxic Releases"),
    no.space = TRUE,
    single.row = TRUE,
    out = "3.3_Result.tex"
)
# cat(tab_out, sep = "\n", file = "results.tex")
