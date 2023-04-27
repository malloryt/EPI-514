library(data.table)
library(survey)
library(tidyverse)
library(gtsummary)

load("C:/Users/austi/Downloads/clean_BRFSS.RData")

# Drop New Jersy (will not have to do this soon)
test= BRFSS_final_merged[BRFSS_final_merged$X_STATE!=34]


# Subset to a random set of 1000 rows
test_rows = sample(1:nrow(test),1000,replace = FALSE)
test= test[test_rows,]

# Create the survey design object
design <- svydesign(data = test,
                    id = ~X_PSU, strata = ~X_STSTR, weights = ~state_year_LLCPWT, nest = TRUE)
options(survey.lonely.psu = "adjust")

# Create a table_1
table_1 <-
  design %>%
  tbl_svysummary(
    by = COV_YEAR,
    include = c(SEX,X_RACE),
    label=list(SEX ~ "Sex", X_RACE ~ "Race/Ethnicity group",COV_YEAR~"COVID-19 Year"),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing="ifany",
    missing_text="Missing/Don't know/Refused" 
  )
