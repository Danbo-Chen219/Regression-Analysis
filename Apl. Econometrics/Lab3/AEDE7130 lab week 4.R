rm(list = ls())

# install.packages("pacman", repos = "https://cloud.r-project.org/")

pacman::p_load(
  car, dplyr, fixest, stargazer, sensemakr,
  fabricatr, JuliaConnectoR, collapse, dqrng, gtools
)
# install.packages("devtools", repos = "https://cran.r-project.org") # Install devtools if not installed
# devtools::install_github("s3alfisc/wildrwolf")
# install.packages("fwildclusterboot", repos = "https://cran.r-project.org")
# install.packages("remotes", repos = "https://cran.r-project.org")
# remotes::install_github("s3alfisc/fwildclusterboot")
# install.packages("fixest", repos = "https://cran.r-project.org")
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
# Multiple Hypothesis Testing
# Bonferroni and Holm
setwd("D:/文档/GitHub/Regression-Analysis/Apl. Econometrics/Lab3")
FILE <- getwd()


# FILE <- dirname(rstudioapi::getActiveDocumentContext()$path) # root directory of file
ugandadata <- read.csv(paste0(FILE, "/uganda.csv"))

uganda_sub <- ugandadata %>%
  data.frame() %>%
  mutate(logfarmrev = log(sfarmrev)) %>%
  dplyr::select(
    logfarmrev, avghrsperwker, farmsize,
    hhsize, extvisits, agehead, hhavgeduc,
    fertilizer, seedtechs, malehead, distance, drought, urban,
    year, hhid, comm
  )

ols1 <- feols(logfarmrev ~ fertilizer, data = uganda_sub, vcov = "HC1") # vcov = "HC1" applies heteroskedasticity-robust standard errors.
ols2 <- feols(logfarmrev ~ fertilizer | hhid, data = uganda_sub, vcov = "HC1")
ols3 <- feols(farmsize ~ fertilizer + hhsize + malehead + seedtechs | hhid, data = uganda_sub, vcov = "HC1") # household fixed effects (| hhid)

# extract p-value
p1 <- ols1$coeftable["fertilizer", "Pr(>|t|)"] # Pr(>|t|) gives the probability of observing a t-statistic, cannot reject the null H0
p2 <- ols2$coeftable["fertilizer", "Pr(>|t|)"]
p3 <- ols3$coeftable[, "Pr(>|t|)"]

p_holm <- p.adjust(p3, method = "holm", n = length(p3))
p_bon <- p.adjust(p3, method = "bonferroni", n = length(p3))

print(cbind(p3, p_bon, p_holm))

# Romano-Wolf adjusted p-value
# Romano, J.P., and M. Wolf. 2016. "Efficient computation of adjusted p -values for resampling-based stepdown multiple testing." Statistics & Probability Letters 113:38-40.
# res_rwolf2 <- rwolf(
#  models = list(ols2, ols3),
#  param = "fertilizer",
#  bootstrap_type = "fnw11",
#  B = 200
# )


# Sensitivity Test
# Cinelli, C., & Hazlett, C. (2020). Making sense of sensitivity: Extending omitted variable bias. Journal of the Royal Statistical Society Series B: Statistical Methodology, 82(1), 39-67.
wagedata <- read.csv(paste0(FILE, "/wage2.csv"))
wagedata <- mutate(wagedata, logwage = log(wage), exper2 = exper^2)

# OLS estimation--linear-linear
ols4 <- lm(logwage ~ educ + exper + exper2, data = wagedata)

# OLS estimation--log-linear
ols5 <- lm(logwage ~ educ + exper + exper2 + tenure + married + black + south + urban, data = wagedata)

stargazer(ols4, ols5, type = "text", title = "Wage Regression Results")

### The estimated effect of education is likely biased due to omission of traits such as "ability" and "work ethic"
### The statements below implement estimate bounds for the effect of education under the assumption that the omitted
### counfounders are up to three times strongly related to education and wages as the variable tenure.

### The sensemakr package is required to run these statements.
sensitivity_test <- sensemakr(
  model = ols5,
  treatment = "educ",
  benchmark_covariates = "tenure",
  kd = 1:3
)
sensitivity_test
summary(sensitivity_test)

# Prints sensitivity analysis to the unobserved confounding


ovb_minimal_reporting(sensitivity_test, type = "latex", out = "wage_results.tex")
# prints latex code for minimal sensitivity analysis reporting


# dev.off()
# clears plots


plot(sensitivity_test)
# plotting contour lines indicating adjusted estimated effects

plot(sensitivity_test, sensitivity.of = "t-value")
# Contour plot indicating adjusted estimated t-ratios


plot(sensitivity_test, type = "extreme")
# Extreme scenarios for the partial R2 of the confounder with the outcome variable
# The red tick marks at the bottom indicate the bounds of confounding:
# once, twice, or three times as strong as the treatment variable
