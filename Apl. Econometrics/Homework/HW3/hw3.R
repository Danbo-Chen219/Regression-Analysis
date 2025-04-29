install.packages(mlogit, repos = "https://cloud.r-project.org/")

library(mlogit)
library(readxl)
library(dplyr)

setwd("D:/文档/GitHub/Regression-Analysis/Apl. Econometrics/homework/HW3")
FILE <- getwd()
df <- read_excel(paste0(FILE, "/Data503_longformfull.xlsx"))
df <- df %>%
  mutate(
    US = ifelse(Origin == 1, 1, 0),                   # 1 = USA, 0 = Canada
    plantm = ifelse(ProductType == 2, 1, 0),          # 2 = plant-based
    culturem = ifelse(ProductType == 3, 1, 0)         # 3 = cultured
  )