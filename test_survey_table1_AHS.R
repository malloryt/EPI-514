setwd("C:/Users/Austin Hammermeister/Desktop/PhD/Year 1 Coursework/SPR_2023/EPI 514/Group Project/")

library(data.table)
library(survey)
library(tidyverse)
library(gtsummary)

load("clean_BRFSS.RData")

# Subset to a random set of 1000 rows to test creating the table
test_rows = sample(1:nrow(BRFSS_merged),10000,replace = FALSE)
test= BRFSS_merged[test_rows,]

test$COV_YEAR = test$COV_YEAR %>% factor(levels = c(0,1),
                                            labels = c("2017-2019","2020-2021"))

test$HLTHPLN1 = test$HLTHPLN1 %>% factor(levels = c(1,2),
                               labels = c("Coverage","No coverage"))

test$MEDCOST = test$MEDCOST %>% factor(levels = c(1,2),
                                         labels = c("Barrier","No barrier"))

test$SEX = test$SEX %>% factor(levels = c(1,2),
                                         labels = c("Male","Female"))

# Create the survey design object
design <- svydesign(data = test,
                    id = ~X_PSU, strata = ~X_STSTR, weights = ~LLCPWT_5Y, nest = TRUE)
options(survey.lonely.psu = "adjust")

# Create a table_1
table_1 <-
  design %>%
  tbl_svysummary(
    by = COV_YEAR,
    include = c(MEDCOST,HLTHPLN1,SEX),
    label=list(MEDCOST ~ "Financial barrier to care", HLTHPLN1 ~ "Healthcare coverage", SEX ~ "Sex",
               COV_YEAR ~ "COVID-19 Year"),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing="ifany",
    missing_text="Missing/Don't know/Refused" 
  )
