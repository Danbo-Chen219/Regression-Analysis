########################################

rm(list = ls())
# Install fixest package
library(fixest)
library(tidyverse)
library(boot)
library(stargazer)
library(sandwich)
library(lmtest)
library(boot)
library(wildrwolf)
library(dplyr)
library(tidyr)
# Multiple Hypothesis Testing
# Bonferroni and Holm
setwd("D:/文档/GitHub/Regression-Analysis/Apl. Econometrics/homework/HW2")
FILE <- getwd()
lesotho <- read.csv(paste0(FILE, "/lesotho_cgp_data.csv"))
####################### ----Question 1.1------##########################

# df_baseline <- lesotho %>% filter(period == "Baseline")

# Separate the treated and control groups
baseline_control <- lesotho %>% filter(period == "Baseline" & cgp == 0)
followup_control <- lesotho %>% filter(period == "Follow u" & cgp == 0) # only focus on control group
cat("Baseline Control group rows:", nrow(baseline_control), "\n")
cat("Follow-up Control group rows:", nrow(followup_control), "\n")
# **Step 2: define baseline variables**
baseline_vars <- c(
    "foodexp", "nfoodexp", "healthexp", "eduexp", "children017",
    "hheduc", "hhsize", "remit_amount", "cellphone", "hungry_adults",
    "hungry_kids"
)

# **Step 3: initial t-test result table**
t_test_results <- data.frame(
    Variable = character(),
    Mean_Baseline = numeric(),
    Mean_Followup_Control = numeric(),
    P_Value = numeric(),
    Stability = character(), # 变量是否平稳
    stringsAsFactors = FALSE
)

# **Step 4: t test**
for (var in baseline_vars) {
    if (sum(!is.na(baseline_control[[var]])) > 1 & sum(!is.na(followup_control[[var]])) > 1) {
        t_test <- t.test(baseline_control[[var]], followup_control[[var]], var.equal = FALSE)

        stability_flag <- ifelse(t_test$p.value < 0.05, "No (Significant Change)", "Yes (Stable)")

        t_test_results <- rbind(t_test_results, data.frame(
            Variable = var,
            Mean_Baseline = mean(baseline_control[[var]], na.rm = TRUE),
            Mean_Followup_Control = mean(followup_control[[var]], na.rm = TRUE),
            P_Value = t_test$p.value,
            Stability = stability_flag
        ))
    } else {
        cat("Skipping", var, "due to insufficient data\n")
    }
}


print(t_test_results)

####################### ----Question 1.2------##########################
# Step 1: Filter Follow-up Data Only

rm(list = ls())
# Install fixest package
library(fixest)
library(tidyverse)
library(boot)
library(stargazer)
library(sandwich)
library(lmtest)
library(boot)
library(wildrwolf)
library(dplyr)
library(tidyr)
# Multiple Hypothesis Testing
# Bonferroni and Holm
setwd("D:/文档/GitHub/Regression-Analysis/Apl. Econometrics/homework/HW2")
FILE <- getwd()
lesotho <- read.csv(paste0(FILE, "/lesotho_cgp_data.csv"))
followup_data <- lesotho %>% filter(period == "Follow u")
cat("Number of rows in Follow-up data:", nrow(followup_data), "\n")
# Step 2: Run OLS Regressions
model_foodexp <- lm(foodexp ~ assign, data = followup_data)
model_nfoodexp <- lm(nfoodexp ~ assign, data = followup_data)

# Step 3: Display Regression Results
stargazer(model_foodexp, model_nfoodexp, type = "text", title = "Wage Regression Results")


####################### ----Question 1.3------##########################
# **Step 2: Merge Baseline Covariates into Follow-up Data**
baseline_data <- lesotho %>%
    filter(period == "Baseline") %>%
    select(unihhid, children017, hheduc, hhsize, remit_amount) # Baseline covariates

# followup_data <- followup_data %>%
#     left_join(baseline_data, by = "unihhid") # Merge based on unique household ID
# Merge Baseline Covariates, renaming `.y` columns correctly
followup_data <- followup_data %>%
    left_join(
        baseline_data %>% select(unihhid, children017, hheduc, hhsize, remit_amount),
        by = "unihhid"
    ) %>%
    rename(
        children017 = children017.y,
        hheduc = hheduc.y,
        hhsize = hhsize.y,
        remit_amount = remit_amount.y
    ) %>%
    select(-children017.x, -hheduc.x, -hhsize.x, -remit_amount.x) # Remove duplicate `.x` columns

# Check if the issue is resolved
print(names(followup_data))

# **Step 3: Handle Missing Data (Drop NAs)**
followup_data <- followup_data %>% drop_na(foodexp, nfoodexp, children017, hheduc, hhsize, remit_amount)

# **Step 4: Run OLS Regressions with Additional Controls**
model_foodexp <- lm(foodexp ~ assign + children017 + hheduc + hhsize + remit_amount, data = followup_data)
model_nfoodexp <- lm(nfoodexp ~ assign + children017 + hheduc + hhsize + remit_amount, data = followup_data)

stargazer(model_foodexp, model_nfoodexp, type = "text", title = "Wage Regression Results")


####################### ----Question 2.1------##########################


####################### ----Question 3.1------##########################
# install.packages("sensemakr", repos = "https://cloud.r-project.org/")

library(tidyverse)
library(boot)
library(stargazer)
library(sandwich)
library(lmtest)
library(boot)
library(wildrwolf)
library(dplyr)
library(tidyr)
library(sensemakr)

setwd("D:/文档/GitHub/Regression-Analysis/Apl. Econometrics/homework/HW2")
FILE <- getwd()
epadata <- read.csv(paste0(FILE, "/epadata.csv"))

# Create squared terms for Fac (facility size)
epadata <- epadata %>%
    mutate(facsq = fac^2, emplsq = empl^2)

# Fit the OLS model
ols_model <- lm(release ~ fac + facsq + herf + empl + emplsq + fg + strictbar +
    educbar + lawbar + spendbar + pstatus, data = epadata)

# Conduct robustness check for pstatus using `fac` as benchmark
sensitivity_analysis <- sensemakr(
    model = ols_model,
    treatment = "pstatus", # Variable of interest
    benchmark_covariates = "fac", # Benchmark variable
    kd = 1:3 # Strength of confounding from 1x to 3x the effect of `fac`
)

# Print sensitivity analysis summary
summary(sensitivity_analysis)

# Generate sensitivity plot
plot(sensitivity_analysis)

# Generate LaTeX-formatted regression table with sensitivity analysis
stargazer(
    ols_model,
    type = "latex",
    title = "OLS Regression with Sensitivity Analysis",
    single.row = TRUE, no.space = TRUE,
    out = "sensitivity_analysis.tex"
)
