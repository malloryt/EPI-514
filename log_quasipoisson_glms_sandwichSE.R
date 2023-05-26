################################################################################
# Script purpose: Survey weighted glm for main analyses
# Author: Austin Hammermeister Suger 
# Last Updated: 5/26/2023
# Required dependencies:
  # data.table
  # survey
  # tidyverse
  # lmtest
  # sandwich
################################################################################

## Clear the global environment ##
rm(list=ls())

## Load the required dependencies ## 
library(data.table)
library(survey)
library(tidyverse)
library(lmtest)
library(sandwich)

## Set the working directory ## 
setwd("C:/Users/Austin Hammermeister/Desktop/PhD/Year 1 Coursework/SPR_2023/EPI 514/Group Project/")

## Load the cleaned data set ##
load("clean_BRFSS.RData")

## Subset to the complete case set ##

complete_data = BRFSS_merged %>% filter(is.na(HIV_test)== FALSE & 
                                          is.na(COV_YEAR)== FALSE &
                                          is.na(MEDCOST)== FALSE & 
                                          is.na(HLTHPLN1) == FALSE)

rm(list=c("BRFSS_merged","subset_2017","subset_2018",
     "subset_2019","subset_2020","subset_2021"))
## Apply descriptive levels to variables ##

# COVID-19 Year #

complete_data$COV_YEAR = complete_data$COV_YEAR %>% factor(levels = c(0,1),
                                                           labels = c("2017-2019","2020-2021"))


# Sex #

complete_data$SEX = complete_data$SEX %>% factor(levels = c(1,2),
                                                 labels = c("Male","Female"))


# Race/ethnicity #

complete_data$X_RACE = complete_data$X_RACE %>% factor(levels = c(1,2,3,4,5,6,7,8),
                                                       labels = c("White, non-Hispanic",
                                                                  "Black, non-Hispanic",
                                                                  "American Indian or Alaskan Native, non-Hispanic",
                                                                  "Asian, non-Hispanic",
                                                                  "Native Hawaiian or other Pacific Islander, non-Hispanic",
                                                                  "Other race, non-Hispanic",
                                                                  "Multiracial, non-Hispanic",
                                                                  "Hispanic"))

# Age group #
complete_data$X_AGEG5YR = complete_data$X_AGEG5YR %>% factor(levels = c(1,2,3,4,5,
                                                                      6,7,8,9,
                                                                      10,11,12,
                                                                      13),
                                                           labels = c("18-24",
                                                                      "25-29",
                                                                      "30-34",
                                                                      "35-39",
                                                                      "40-44",
                                                                      "45-49",
                                                                      "50-54",
                                                                      "55-59",
                                                                      "60-64",
                                                                      "65-69",
                                                                      "70-74",
                                                                      "75-79",
                                                                      "80+"))


# Urban/rural status #
complete_data$X_URBSTAT = complete_data$X_URBSTAT %>% factor(levels = c(1,2),
                                                             labels = c("Urban counties","Rural counties"))

# Employment #

complete_data$EMPLOY1 = complete_data$EMPLOY1 %>% factor(levels = c(1,2,3,4,5,6,7,8),
                                                         labels = c("Employed for wages",
                                                                    "Self-employed",
                                                                    "Out of work (>=1 year)",
                                                                    "Out of work (<1 year)",
                                                                    "A homemaker",
                                                                    "A student",
                                                                    "Retired",
                                                                    "Unable to work"))
# Annual household income #

complete_data$INCOME2 = complete_data$INCOME2 %>% factor(levels = c(1,2,3,4,5,6,7,8),
                                                         labels = c("<$10K",
                                                                    "$10-15K",
                                                                    "$15-20K",
                                                                    "$20-25K",
                                                                    "$25-35K",
                                                                    "$35-50K",
                                                                    "$50-75K",
                                                                    ">$75K"))


# Home ownership #
complete_data$RENTHOM1 = complete_data$RENTHOM1 %>% factor(levels = c(1,2,3),
                                                           labels = c("Own","Rent",
                                                                      "Other"))

# Level of education #
complete_data$X_EDUCAG = complete_data$X_EDUCAG %>% factor(levels = c(1,2,3,4),
                                                           labels = c("Less than high school","High school",
                                                                      "Some college or technical school",
                                                                      "College or technical school"))

# Sexual orientation #

# Create a single sexual orientation variable 
complete_data = complete_data %>% mutate(SO = case_when(SOMALE==1 | SOFEMALE==1 ~ 1,
                                                        SOMALE==2 | SOFEMALE==2 ~ 2,
                                                        SOMALE==3 | SOFEMALE==3 ~ 3,
                                                        SOMALE==4 | SOFEMALE==4 ~ 4))

complete_data$SO = complete_data$SO %>% factor(levels = c(1,2,3,4),
                                               labels = c("Gay or Lesbian",
                                                          "Straight",
                                                          "Bisexual",
                                                          "Something else"))

# Transgender status #

complete_data$TRNSGNDR = complete_data$TRNSGNDR %>% factor(levels = c(1,2,3,4),
                                                           labels = c("Transgender, male-to-female",
                                                                      "Transgender, female-to-male",
                                                                      "Transgender, gender nonconforming",
                                                                      "Not transgender"))


## Create the survey design object ##

design <- svydesign(data = complete_data,
                    id = ~X_PSU, strata = ~X_STSTR, weights = ~LLCPWT_5Y, nest = TRUE)
options(survey.lonely.psu = "adjust")

## Financial barriers fully adjusted survey-weighted log-poisson model w/ period interaction ##
FB_P_results = svyglm(HIV_test~MEDCOST*COV_YEAR+HLTHPLN1+SEX+X_RACE+
                   X_AGEG5YR+INCOME2+EMPLOY1+RENTHOM1+X_EDUCAG,
                 design = design, family = quasipoisson(link="log"))
save(FB_P_results,file="FB_P_results.rds")

## number of responses ##
length(FB_P_results$residuals)

## 2020-2021 PR point estimate ##
FB_P_PR_COV=exp(coef(FB_P_results)["MEDCOST"]+coef(FB_P_results)["MEDCOST:COV_YEAR2020-2021"])
round(FB_P_PR_COV,3)

### 2020-2021 PR 95% CI ##
log.se.robust = sqrt(vcovHC(FB_P_results)["MEDCOST","MEDCOST"]+
                       vcovHC(FB_P_results)["MEDCOST:COV_YEAR2020-2021",
                                       "MEDCOST:COV_YEAR2020-2021"]+
                       2*vcovHC(FB_P_results)["MEDCOST","MEDCOST:COV_YEAR2020-2021"])

FB_P_CI_COV=exp(coef(FB_P_results)["MEDCOST"]+coef(FB_P_results)["MEDCOST:COV_YEAR2020-2021"]+
      c(-1,1)*1.96*log.se.robust)
round(FB_P_CI_COV,3)



## 2017-2019 PR point estimate ##
FB_P_PR_preCOV=exp(coef(FB_P_results)["MEDCOST"])
round(FB_P_PR_preCOV,3)

### 2017-2019 PR 95% CI ##
FB_P_CI_COV=coefci(FB_P_results,vcov.=vcovHC(FB_P_results))[2,] %>% exp()
round(FB_P_CI_COV,3)



## Healthcare coverage fully adjusted survey-weighted log-poisson model w/ period interaction ##
HC_P_results = svyglm(HIV_test~HLTHPLN1*COV_YEAR+MEDCOST+SEX+X_RACE+
                        X_AGEG5YR+INCOME2+EMPLOY1+RENTHOM1+X_EDUCAG,
                      design = design, family = quasipoisson(link="log"))
save(HC_P_results,file="HC_P_results.rds")


## 2020-2021 PR point estimate ##
HC_P_PR_COV=exp(coef(HC_P_results)["HLTHPLN1"]+coef(HC_P_results)["HLTHPLN1:COV_YEAR2020-2021"])
round(HC_P_PR_COV,3)

### 2020-2021 PR 95% CI ##
log.se.robust = sqrt(vcovHC(HC_P_results)["HLTHPLN1","HLTHPLN1"]+
                       vcovHC(HC_P_results)["HLTHPLN1:COV_YEAR2020-2021",
                                            "HLTHPLN1:COV_YEAR2020-2021"]+
                       2*vcovHC(HC_P_results)["HLTHPLN1","HLTHPLN1:COV_YEAR2020-2021"])

HC_P_CI_COV=exp(coef(HC_P_results)["HLTHPLN1"]+coef(HC_P_results)["HLTHPLN1:COV_YEAR2020-2021"]+
                  c(-1,1)*1.96*log.se.robust)
round(HC_P_CI_COV,3)


## 2017-2019 PR point estimate ##
HC_P_PR_preCOV=exp(coef(HC_P_results)["HLTHPLN1"])
round(HC_P_PR_preCOV,3)

### 2017-2019 PR 95% CI ##
HC_P_CI_preCOV=coefci(HC_P_results,vcov.=vcovHC(HC_P_results))[2,] %>% exp()
round(HC_P_CI_preCOV,3)


## Fully adjusted survey-weighted log-poisson model no period interaction ##
results = svyglm(HIV_test~MEDCOST+HLTHPLN1+COV_YEAR+SEX+X_RACE+
                        X_AGEG5YR+INCOME2+EMPLOY1+RENTHOM1+X_EDUCAG,
                      design = design, family = quasipoisson(link="log"))
save(results,file="results.rds")


## PR point estimates ##
FB_PR=exp(coef(results)["MEDCOST"])
round(FB_PR,3)

HC_PR=exp(coef(results)["HLTHPLN1"])
round(HC_PR,3)

### PR 95% CIs ##
FB_CI=coefci(results,vcov.=vcovHC(results))[2,] %>% exp()
HC_CI=coefci(results,vcov.=vcovHC(results))[3,] %>% exp()
round(FB_CI,3)
round(HC_CI,3)






