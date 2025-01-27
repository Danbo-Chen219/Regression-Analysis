rm(list = ls())

{
  want <- c("stargazer", "car", "tidyverse", "cowplot", "clubsandwich", "sandwich", "multiwayvcov", "vtable")
  need <- want[!(want %in% installed.packages()[, "Package"])]
  if (length(need)) install.packages(need, repos = "https://cloud.r-project.org/")
  lapply(want, function(i) require(i, character.only = TRUE))
  rm(need)
}

library(stargazer)
# Load data
# dplyr::mutate()
#--------------------------------Code#1--------------------------------
# generate data
set.seed(1)

# sample size
n <- 20

# true coefficients
bo <- 4
b1 <- 8
b2 <- -4

# regressors
x0 <- rep(x = 1, times = n)
x1 <- rnorm(n = n, mean = 2, sd = 4)
x2 <- rnorm(n = n, mean = 10, sd = 1)
e <- rnorm(n = n, mean = 0, sd = 3)


b <- c(bo, b1, b2)
x <- cbind(x0, x1, x2)
y <- x %*% b + e

print(b)
print(x)
print(y)


# step 2: regression

bhat <- solve(t(x) %*% x) %*% t(x) %*% y

yhat <- x %*% bhat
ehat <- y - yhat

print(b)
print(bhat)
print(cbind(y, yhat, ehat))

dfe <- nrow(x) - ncol(x) # residual degrees of freedom
tss <- sum((y - mean(y))^2) # total sum of squares
ess <- sum((yhat - mean(y))^2) # explained sum of squares
rss <- sum(ehat^2) # residual sum of squares
rsq <- ess / tss # R-squared
sigma2 <- rss / dfe # residual variance

# step 3: Hypothesis testing
print(c(sigma2, rsq))
# t-test
stdbhat <- diag(solve(t(x) %*% x) * sigma2) # standard errors of bhat

# extracting the diagonal elements of the variance-covariance matrix (X'X)^-1
t <- bhat / stdbhat # t-statistics

prob1 <- 2 * (1 - pt(abs(t), dfe)) # p-values, using t distribution
prob2 <- 1 - pf(t^2, 1, dfe) # p-values, using F distribution

# print estimated coefficients, their standard errors, t-statistics, and p-values

print(cbind(bhat, stdbhat, t, prob1, prob2))

# step 4: data export and import

# data frame
xydata <- data.frame(y, x)

# export data
write.csv(xydata, file = "xydata.csv")

# import data
xydata.csv <- read.csv(file = "xydata.csv")

# step 5: linear regression with lm function

# use the imported data to run OLS regression
ols <- lm(formula = y ~ x1 + x2, data = xydata.csv)

summary(ols)
ols$coefficients # estimated bhats
ols$residuals # residuals ehat
ols$fitted.values # yhat
stargazer(ols, type = "text")

#----------------------------Code # 2-------------------------------------------

getwd()
dirname(getwd())
# install.packages("ggplot2", repos = "https://cloud.r-project.org/")
install.packages("knitr", repos = "https://cloud.r-project.org/")
library(dplyr)
library(ggplot2)
library(cowplot)
library(stargazer)
library(knitr)
# working directory where datasets are saved
wagedata <- read.csv("wage2.csv")
# loading wage data

# pipe: https://www.youtube.com/watch?v=Stt3qEuIeso
# dplyr package (included in tidyverse series)
wagedata2 <- mutate(wagedata, logwage = log(wage), exper2 = exper^2)

wagedata <- wagedata %>% mutate(logwage = log(wage), exper2 = exper^2) # %>%
# mutate(exper3 = exper^3) #%>%
# dplyr::select(-c("IQ","educ"))

# computes the log of wages

graph1 <- ggplot(wagedata, aes(x = wage)) +
  geom_histogram(color = "black", fill = "gray")
graph2 <- ggplot(wagedata, aes(x = logwage)) +
  geom_histogram(color = "black", fill = "gray")
plot_grid(graph1, graph2, labels = "AUTO")
# plot_grid (come from cowplot package) allows plotting more than one graph in the same space
ggsave("graph2.jpeg", plot = graph2, width = 6, height = 3.5, units = "in", dpi = 200, bg = "white")


# Summary statistics
wagedata %>%
  dplyr::select(wage, educ, exper) %>%
  stargazer(type = "text")

wagedata <- dplyr::select(
  wagedata,
  c(
    "logwage", "educ", "exper", "exper2",
    "tenure", "married", "black", "south", "urban"
  )
)
# extracting only the variables needed for the regression


# OLS estimation--log-linear
ols1 <- lm(logwage ~ educ + exper + exper2, data = wagedata)

# OLS estimation--log-linear with controls
ols2 <- lm(logwage ~ educ + exper + exper2 + tenure + married + black + south + urban, data = wagedata)

stargazer(ols1, ols2, type = "text", title = "Wage Regression Results")
### get standard error of each coefficients

## 1) using vcov

coefs <- summary(ols2)$coefficients
se <- sqrt(diag(vcov(ols2)))
# Getting OLS coefficients and their standard errors from the second model

plot_order <- c("educ", "exper", "exper2", "tenure", "married", "black", "south", "urban")

plot <- data.frame(
  se = c(summary(ols2)$coefficients[, 2][plot_order]),
  mean = c(coef(ols2)[plot_order]),
  label = c("educ", "exper", "exper2", "tenure", "married", "black", "south", "urban")
)

plot %>%
  ggplot(aes(
    x = label, y = mean,
    ymin = mean - 1.96 * se,
    ymax = mean + 1.96 * se
  )) +
  geom_hline(yintercept = 0, color = "black") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Variable name") +
  ylab("Parameter estimate")

# bootstrapping the education coefficient

m <- 1000 # of bootstraps;

bhat_educ <- rep(0, m)
for (i in 1:m) {
  random_sample <- wagedata[sample(nrow(wagedata), size = nrow(wagedata), replace = TRUE), ]
  ols_random_sample <- lm(logwage ~ educ + exper + exper2 + tenure + married + black + south + urban, data = random_sample)
  bhat_educ[i] <- coefficients(ols_random_sample)[2]
  print(paste(i, "loop finished"))
}


hist(bhat_educ, breaks = 50, freq = FALSE)
# remove freq=FALSE to obtain counts/frequencies as opposed to probabilities. More breaks to obtain a finer grid.

x <- bhat_educ
# rename the variable to be plotted as x because that's the default in the curve function below

mean_x <- mean(bhat_educ)
sd_x <- sd(bhat_educ)
curve(dnorm(x, mean = mean_x, sd = sd_x), add = TRUE, col = "blue")


quantile(bhat_educ, prob = 0.025)
quantile(bhat_educ, prob = 0.975)
# finding the 5% two-sided confidence interval by the percentile bootstrap method
# 95% confident that the true coefficient on education lies between the 2.5 and 97.5th percentiles



#----------------------------Code # 3-------------------------------------------

rm(list = ls())
# install.packages("jtools", repos = "https://cloud.r-project.org/")

# install.packages("fpc", repos = "https://cloud.r-project.org/")
library(fpc)
library(sandwich)
library(skimr)
# ls("package:skimr")
library(gtsummary)
library(knitr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(stargazer)
library(jtools)
library(stringr)
uganda <- read.csv("uganda.csv")

uganda <- mutate(uganda, logfarmrev = log(sfarmrev))

uganda <- uganda %>%
  mutate(logfarmrev = log(sfarmrev))

# computes and adds the log of farm revenue

uganda_sub <- dplyr::select(
  uganda, logfarmrev, farmsize, hhsize,
  extvisits, agehead, hhavgeduc,
  fertilizer, seedtechs, malehead,
  distance, drought, urban,
  year, hhid, comm
)

stargazer(uganda_sub %>% data.frame(), type = "text") # outputs a summary of data
# Summary statistics of the data

# Doing the same commands with pipes (%.%) which allow the sequencing  of multiple operations.

uganda %>%
  data.frame() %>%
  mutate(logfarmrev = log(sfarmrev)) %>%
  dplyr::select(
    logfarmrev, farmsize, hhsize, extvisits, agehead, hhavgeduc,
    fertilizer, seedtechs, malehead, distance, drought, urban,
    year, hhid, comm
  ) %>%
  stargazer(type = "text")
# Computes and adds the log of farm revenue and selects a subset of variables
# summary statistics of the data are added
# Without the data.frame command, the code was generating the following an error message:
# Error in UseMethod("mutate") :
# no applicable method for 'mutate' applied to an object of class "character"

# uganda_sub %>%
#   dplyr::select(-year, -hhid, -comm) %>%
#   sumtable(group = "fertilizer", group.test = TRUE, file = "balance", out = "viewer")
# # Balance test (difference of means) by fertilizer use, after eliminating variables year, hhid and comm

# uganda_sub %>%
#   select(-year, -hhid, -comm) %>%
#   group_by(fertilizer) %>%
#   summarise(mean_value = mean(value, na.rm = TRUE)) %>%
#   kable()

# OLS estimation
ols1 <- lm(logfarmrev ~ fertilizer, data = uganda)


# OLS estimation
ols2 <- lm(logfarmrev ~ fertilizer + farmsize + hhsize + extvisits + agehead + hhavgeduc +
  seedtechs + malehead + distance + drought + urban, data = uganda)


# OLS estimation
ols3 <- lm(logfarmrev ~ fertilizer + farmsize + hhsize + extvisits + agehead + hhavgeduc +
  seedtechs + malehead + distance + drought + urban + factor(comm), data = uganda)

stargazer(ols1, ols2, ols3,
  type = "text",
  # Do not report community FE
  omit = "comm",
  # Omit residual SE and R2
  omit.stat = c("ser", "rsq"),
  add.lines = list(c("Community Fixed effects", "No", "No", "Yes")),
  notes = "Assume homoscedasticity.",
  column.labels = str_c("Model", rep(1:3, 3)),
  # Dependent variable label
  dep.var.caption = "\\textit{Dependent variable:} Log of Farm Revenue",
  dep.var.labels = NULL,
  dep.var.labels.include = F,
  title = "Wage Regression Results"
)


### Computing robust standard errors based on Model 2 ###

count(uganda) # number of observations
length(unique(uganda$comm))
# number of communities (clusters). Minimum of 30 is required for clustering

uganda_sub %>%
  group_by(comm) %>%
  count()
# Reports the number of observations per community

# statements require the dplyr library


# Variance matrices
OLS <- vcov(ols2) # ordinary

HC <- vcovHC(ols2, type = "HC1") # Heteroscedasticity-consistent (HC)
# HC0 gives White's robust errors. HC1 applies a small sample adjustment
# HC1 applies a degrees of freedom-based correction: (n-1/n-k)

CR <- vcov(ols2, type = "CR1", cluster = uganda$comm) # Cluster-robust
# See pdf file titled Package clubSandwich July 20, 2023 for more information on options

IB <- cluster.boot(ols2, cluster = 1:nrow(uganda), R = 100) # Individual data bootstrap

# First, transform the cluster variable to factor and transform to numeric or integer
# for cluster bootstrap to work. No sure why that's the case.
uganda$comm <- as.numeric(as.factor(uganda$comm))

CB <- cluster.boot(ols2, cluster = uganda$comm, R = 100) # Cluster bootstrap

# getting standard errors
std.error <- list(OLS, HC, CR, IB, CB) %>% # list of VCOV matrices
  map(~ sqrt(diag(.))) %>% # extract diagonals and find their square roots
  reduce(data.frame) %>% # reduce a list to a data.frame
  setNames(c("Unadjusted", "HC", "CL", "IB", "CB")) # set column names


# Table of results saved in working directory
stargazer(rep(list(ols2), 5),
  se = std.error, type = "text", no.space = T, df = F,
  column.labels = c("Unadjusted", "HC", "CR", "IB", "CB"),
  title = "Determinants of Farm Income in Uganda",
  out = "table1.txt"
)

stargazer(rep(list(ols2), 5),
  se = std.error, type = "text", no.space = T, df = F,
  column.labels = c("Unadjusted", "HC", "CR", "IB", "CB"),
  title = "Determinants of Farm Income in Uganda",
  out = "table1.html"
)

dev.off()
# clears plots
