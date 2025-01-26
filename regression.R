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
install.packages("ggplot2", repos = "https://cloud.r-project.org/")
install.packages("cowplot", repos = "https://cloud.r-project.org/")
library(dplyr)
library(ggplot2)
library(cowplot)
library(stargazer)
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
