rm(list = ls())

{
  want <- c("stargazer", "car", "tidyverse", "cowplot", "clubsandwich", "sandwich", "multiwayvcov", "vtable")
  need <- want[!(want %in% installed.packages()[, "Package"])]
  if (length(need)) install.packages(need, repos = "https://cloud.r-project.org/")
  lapply(want, function(i) require(i, character.only = TRUE))
  rm(need)
}



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
print(x)
# t-test
stdbhat <- diag(solve(t(x) %*% x) * sigma2) # standard errors of bhat

# extracting the diagonal elements of the variance-covariance matrix (X'X)^-1
t <- bhat / stdbhat # t-statistics

prob1 <- 2 * pt(-abs(t), df = dfe) # p-values, using t distribution
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
