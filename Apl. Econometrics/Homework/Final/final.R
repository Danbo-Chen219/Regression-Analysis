rm(list = ls())
# install.packages("lfe", repos = "https://cloud.r-project.org/")
# install.packages("did", repos = "https://cloud.r-project.org/")
library(did)
library(dplyr)
library(lfe)
library(readr)
library(stargazer)
library(tidyr)

setwd("D:/文档/GitHub/Regression-Analysis/Apl. Econometrics/homework/Final")
FILE <- getwd()
# Read the dataset
data <- read.csv("finalsp25dataset.csv")

#------------------------------------------------------------
# （1）Replace the few missing values for toxic releases
#------------------------------------------------------------
# Check for missing values
data <- data %>%
    group_by(firm_id) %>%
    mutate(releases = ifelse(releases == 0, mean(releases[releases != 0], na.rm = TRUE), releases)) %>%
    ungroup()

# Save the modified dataset
# write.csv(df, "finalsp25dataset_modified.csv", row.names = FALSE)

# Display the first few rows of the modified dataset
head(data)

#------------------------------------------------------------
# （2）Descriptive Statistics
#------------------------------------------------------------
desc_full <- data %>%
    summarise(
        across(
            c(releases, sales, rd, insp),
            list(
                mean = ~ mean(.x, na.rm = TRUE),
                sd   = ~ sd(.x, na.rm = TRUE),
                min  = ~ min(.x, na.rm = TRUE),
                max  = ~ max(.x, na.rm = TRUE),
                n    = ~ sum(!is.na(.x))
            )
        )
    )

## 4b) By ISO 14001 certification
desc_by_iso <- data %>%
    group_by(iso14001) %>%
    summarise(
        across(
            c(releases, sales, rd, insp),
            list(
                mean = ~ mean(.x, na.rm = TRUE),
                sd   = ~ sd(.x, na.rm = TRUE),
                min  = ~ min(.x, na.rm = TRUE),
                max  = ~ max(.x, na.rm = TRUE),
                n    = ~ sum(!is.na(.x))
            )
        )
    ) %>%
    ungroup()

# Pivot so that iso14001=0 and iso14001=1 stats become columns in one row
desc_by_iso_wide <- desc_by_iso %>%
    pivot_wider(
        names_from = iso14001,
        values_from = c(
            releases_mean, releases_sd, releases_min, releases_max, releases_n,
            sales_mean, sales_sd, sales_min, sales_max, sales_n,
            rd_mean, rd_sd, rd_min, rd_max, rd_n,
            insp_mean, insp_sd, insp_min, insp_max, insp_n
        ),
        names_glue = "{.value}_{iso14001}"
    )


vars <- c("releases", "sales", "rd", "insp")

t_tests <- lapply(vars, function(var) {
    t.test(as.formula(paste0(var, " ~ iso14001")), data = data)
})
names(t_tests) <- vars

# Extract p-values for each variable
p_values <- sapply(t_tests, function(x) x$p.value)

descriptive_table <- data.frame(
    Variable = c("Toxic Releases", "Sales", "R&D Expenditure", "Inspections"),

    # Full sample columns
    Mean_Full = c(
        desc_full$releases_mean, desc_full$sales_mean,
        desc_full$rd_mean, desc_full$insp_mean
    ),
    SD_Full = c(
        desc_full$releases_sd, desc_full$sales_sd,
        desc_full$rd_sd, desc_full$insp_sd
    ),
    Min_Full = c(
        desc_full$releases_min, desc_full$sales_min,
        desc_full$rd_min, desc_full$insp_min
    ),
    Max_Full = c(
        desc_full$releases_max, desc_full$sales_max,
        desc_full$rd_max, desc_full$insp_max
    ),
    N_Full = c(
        desc_full$releases_n, desc_full$sales_n,
        desc_full$rd_n, desc_full$insp_n
    ),

    # Certified (iso14001 = 1)
    Mean_Cert = c(
        desc_by_iso_wide$releases_mean_1, desc_by_iso_wide$sales_mean_1,
        desc_by_iso_wide$rd_mean_1, desc_by_iso_wide$insp_mean_1
    ),
    SD_Cert = c(
        desc_by_iso_wide$releases_sd_1, desc_by_iso_wide$sales_sd_1,
        desc_by_iso_wide$rd_sd_1, desc_by_iso_wide$insp_sd_1
    ),
    Min_Cert = c(
        desc_by_iso_wide$releases_min_1, desc_by_iso_wide$sales_min_1,
        desc_by_iso_wide$rd_min_1, desc_by_iso_wide$insp_min_1
    ),
    Max_Cert = c(
        desc_by_iso_wide$releases_max_1, desc_by_iso_wide$sales_max_1,
        desc_by_iso_wide$rd_max_1, desc_by_iso_wide$insp_max_1
    ),
    N_Cert = c(
        desc_by_iso_wide$releases_n_1, desc_by_iso_wide$sales_n_1,
        desc_by_iso_wide$rd_n_1, desc_by_iso_wide$insp_n_1
    ),

    # Non-certified (iso14001 = 0)
    Mean_NonCert = c(
        desc_by_iso_wide$releases_mean_0, desc_by_iso_wide$sales_mean_0,
        desc_by_iso_wide$rd_mean_0, desc_by_iso_wide$insp_mean_0
    ),
    SD_NonCert = c(
        desc_by_iso_wide$releases_sd_0, desc_by_iso_wide$sales_sd_0,
        desc_by_iso_wide$rd_sd_0, desc_by_iso_wide$insp_sd_0
    ),
    Min_NonCert = c(
        desc_by_iso_wide$releases_min_0, desc_by_iso_wide$sales_min_0,
        desc_by_iso_wide$rd_min_0, desc_by_iso_wide$insp_min_0
    ),
    Max_NonCert = c(
        desc_by_iso_wide$releases_max_0, desc_by_iso_wide$sales_max_0,
        desc_by_iso_wide$rd_max_0, desc_by_iso_wide$insp_max_0
    ),
    N_NonCert = c(
        desc_by_iso_wide$releases_n_0, desc_by_iso_wide$sales_n_0,
        desc_by_iso_wide$rd_n_0, desc_by_iso_wide$insp_n_0
    ),

    # T-test p-values
    P_value = c(
        p_values["releases"],
        p_values["sales"],
        p_values["rd"],
        p_values["insp"]
    )
)

# Use stargazer to display
stargazer(
    descriptive_table,
    summary = FALSE,
    type = "text", # or "latex" / "html" for other outputs
    title = "Descriptive Statistics: Full Sample vs. ISO 14001 Certification",
    rownames = FALSE,
    digits = 3, # number of decimal places
    out = "Descriptive_Statistics.tex"
)

#------------------------------------------------------------
# （3）Two-Way Fixed Effects Model
#------------------------------------------------------------
# Create log of toxic releases
# Adding 1 avoids log(0) issues
data <- data %>%
    mutate(log_releases = log(releases + 1))

twfe_model <- felm(
    formula = log_releases ~ iso14001 + insp + rd + sales |
        firm_id + year |
        0 |
        firm_id,
    data = data
)

# View summary
# summary(twfe_model)
stargazer(twfe_model, type = "text", title = "Two-Way Fixed Effects Model", out = "Two_Way_FE_Model.tex")

#------------------------------------------------------------
# （4）Cross-Sectional Difference-in-Differences
#------------------------------------------------------------
data <- data %>%
    group_by(firm_id) %>%
    mutate(
        first_year_cert = ifelse(any(iso14001 == 1),
            min(year[iso14001 == 1]),
            0
        ) # 0 if never treated
    ) %>%
    ungroup()

cs_nocontrols <- att_gt(
    yname       = "log_releases", # outcome
    tname       = "year", # time variable
    idname      = "firm_id", # firm identifier
    gname       = "first_year_cert", # first treatment year
    data        = data,
    panel       = TRUE,
    clustervars = "firm_id" # cluster by firm
)

# summary(cs_nocontrols)

cs_controls <- att_gt(
    yname       = "log_releases",
    tname       = "year",
    idname      = "firm_id",
    gname       = "first_year_cert",
    xformla     = ~ sales + rd + insp, # Covariates
    data        = data,
    panel       = TRUE,
    clustervars = "firm_id"
)

agg_nocontrols <- aggte(cs_nocontrols, type = "group")
# summary(agg_nocontrols)

agg_controls <- aggte(cs_controls, type = "group")
# summary(agg_controls)

es_nocontrols <- aggte(cs_nocontrols, type = "dynamic")
# summary(es_nocontrols)

es_controls <- aggte(cs_controls, type = "dynamic")
# summary(es_controls)

# Create a data frame of overall ATT from the 'group' aggregation
agg_table <- data.frame(
    Model = c("No Controls", "With Controls"),
    ATT   = c(agg_nocontrols$att, agg_controls$att),
    SE    = c(agg_nocontrols$se, agg_controls$se)
)

# Use stargazer to display
stargazer(
    agg_table,
    summary = FALSE,
    type = "text",
    title = "CS DiD: Overall Group-Averaged ATT",
    out = "CS_DiD_ATT.tex"
)

#------------------------------------------------------------
# （5）Event Study
#------------------------------------------------------------
es_nocontrols <- aggte(cs_nocontrols, type = "dynamic", min_e = -6, max_e = 6)
es_controls <- aggte(cs_controls, type = "dynamic", min_e = -6, max_e = 6)

# summary(es_nocontrols)
# summary(es_controls)

# For the no-controls model:
ggdid(es_nocontrols)

# For the with-controls model:
ggdid(es_controls)
