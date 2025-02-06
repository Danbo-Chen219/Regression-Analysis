
library(readr)

dt_env <- read_csv("epadata.csv")

# summary(dt_env$pstatus)  # 0.5205 participation rate

set.seed(136)
sam_pool <- dt_env$pstatus

dt_env$new_random_assign <- sample(sam_pool,dim(dt_env)[1],replace = F)


kk <- rnorm(1000,mean=-3,sd=5)
quantile(kk)

quantile(kk,probs=c(0.025,0.975))
