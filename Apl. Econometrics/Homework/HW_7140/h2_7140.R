library(readxl)
library(dplyr)
library(stargazer)

# Load the Excel file
data <- read_excel("Apl. Econometrics/Homework/HW_7140/HW2 landvalue class 2025.xlsx")

# Inspect the data structure
summary(data)

# Identify the absurd Rain value (should be -999)
summary(data$Rain)

# Replace -999 in Rain with mean of valid observations
mean_rain <- mean(data$Rain[data$Rain != -999])
data$Rain <- ifelse(data$Rain == -999, mean_rain, data$Rain)

# Check that the replacement worked
summary(data$Rain)

# Create dummy variables for UseType
# Base category: Shrubs (UseType = 1)
data <- data %>%
  mutate(
    Orchard = ifelse(UseType == 2, 1, 0),
    Range = ifelse(UseType == 3, 1, 0),
    Crop = ifelse(UseType == 4, 1, 0)
  )

# Run the OLS regression
model <- lm(Value ~ Building + Genertn + Rain + Orchard + Range + Crop, data = data)

# Display summary of the regression
summary(model)

# Suggest log transformation (optional)
log_model <- lm(log(Value) ~ Building + Genertn + log(Rain) + Orchard + Range + Crop, data = data)
summary(log_model)
stargazer(log_model, type = "text", title = "Log-Transformed Model Results",out = "3.1_Result.tex")


# ------------------------------
#(2)Estimate a Log-Lin, Lin-Log, and Log-Log model
# ------------------------------

# Model 1: Log-Lin (log(Value) ~ x)
log_lin_model <- lm(log(Value) ~ Building + Genertn + Rain + Orchard + Range + Crop, data = data)
summary(log_lin_model)

# ------------------------------
# Model 2: Lin-Log (Value ~ log(Rain))
lin_log_model <- lm(Value ~ Building + Genertn + log(Rain) + Orchard + Range + Crop, data = data)
summary(lin_log_model)

# ------------------------------
# Model 3: Log-Log (log(Value) ~ log(Rain))
log_log_model <- lm(log(Value) ~ Building + Genertn + log(Rain) + Orchard + Range + Crop, data = data)
summary(log_log_model)

stargazer(log_lin_model, lin_log_model, log_log_model, type = "text", title = "Model Comparison",out = "3.2_Result.tex")