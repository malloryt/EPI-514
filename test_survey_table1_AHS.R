setwd("C:/Users/Austin Hammermeister/Desktop/PhD/Year 1 Coursework/SPR_2023/EPI 514/Group Project/")

library(data.table)
library(survey)
library(tidyverse)
library(gtsummary)

load("clean_BRFSS.RData")

# Subset to a random set of 1000 rows to test creating the table
test_rows = sample(1:nrow(BRFSS_merged),1000,replace = FALSE)
test= test[test_rows,]

# Create the survey design object
design <- svydesign(data = test,
                    id = ~X_PSU, strata = ~X_STSTR, weights = ~LCCPWT_5Y, nest = TRUE)
options(survey.lonely.psu = "adjust")

# Create a table_1
table_1 <-
  design %>%
  tbl_svysummary(
    by = COV_YEAR,
    include = c(MEDCOST,HLTHPLN1,SEX),
    label=list(HLTHPLN1 ~ "Healthcare coverage", Sex ~ "Sex", MEDCOST ~ "Financial barrier to care"),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing="ifany",
    missing_text="Missing/Don't know/Refused" 
