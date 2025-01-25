rm(list = ls())
{
  want <- c("stargazer", "car", "tidyverse", "cowplot", "clubsandwich", "sandwich", "multiwayvcov", "vtable")
  need <- want[!(want %in% installed.packages()[, "Package"])]
  if (length(need)) install.packages(need)
  lapply(want, function(i) require(i, character.only = TRUE))
  rm(need)
}
install.packages('languageserver', repos = 'https://cloud.r-project.org/')
library(languageserver)
# Load data
