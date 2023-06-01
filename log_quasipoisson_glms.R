################################################################################
# Script purpose: Survey weighted glm for main analyses
# Author: Austin Hammermeister Suger 
# Last Updated: 5/31/2023
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

## Filter to relevant variables ##
subset = BRFSS_merged[,!c("X_RACE","X_URBSTAT","SOMALE","SOFEMALE","BIRTHSEX",
                          "TRNSGNDR","HIVTST6","HIVTSTD3")]

## Subset to the complete case set ##

complete_data = subset[complete.cases(subset),] 


## Apply descriptive levels to variables ##

# COVID-19 Year #

complete_data$COV_YEAR = complete_data$COV_YEAR %>% factor(levels = c(0,1),
                                                           labels = c("2017-2019","2020-2021"))

# Financial barriers to care #
complete_data$MEDCOST = ifelse(complete_data$MEDCOST==1,1,0)

# Healthcare coverage #
complete_data$HLTHPLN1 = ifelse(complete_data$HLTHPLN1==2,1,0)


# Sex #

complete_data$SEX = complete_data$SEX %>% factor(levels = c(1,2),
                                                 labels = c("Male","Female"))


# Race #
complete_data$X_MRACE1 = complete_data$X_MRACE1 %>% factor(levels = c(1,2,3,4,5,6,7),
                                                           labels = c("White",
                                                                      "Black",
                                                                      "American Indian or Alaskan Native",
                                                                      "Asian",
                                                                      "Native Hawaiian or other Pacific Islander",
                                                                      "Another race",
                                                                      "Multiracial"))

# Ethnicity #
complete_data$X_HISPANC = complete_data$X_HISPANC %>% factor(levels = c(1,2),
                                                             labels = c("Hispanic","Non-Hispanic"))

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


## Create the survey design object ##

design <- svydesign(data = complete_data,
                    id = ~X_PSU, strata = ~X_STSTR, weights = ~LLCPWT_5Y, nest = TRUE)
options(survey.lonely.psu = "adjust")


## Overall Counts and Weighted Prevalence for Financial Barriers ##

u_n_table = table(complete_data$COV_YEAR,complete_data$HIV_test,
                  complete_data$MEDCOST,deparse.level = 2)
u_n_table

u_n_table = table(complete_data$COV_YEAR,
                  complete_data$MEDCOST,deparse.level = 2)
u_n_table

u_n_table = table(complete_data$HIV_test,
                  complete_data$MEDCOST,deparse.level = 2)
u_n_table

u_n_table = table(complete_data$MEDCOST,deparse.level = 2)
u_n_table


## Overall Counts and Weighted Prevalence for Healthcare Coverage ##

u_n_table = table(complete_data$COV_YEAR,complete_data$HIV_test,
                  complete_data$HLTHPLN1,deparse.level = 2)
u_n_table

u_n_table = table(complete_data$COV_YEAR,
                  complete_data$HLTHPLN1,deparse.level = 2)
u_n_table

u_n_table = table(complete_data$HIV_test,
                  complete_data$HLTHPLN1,deparse.level = 2)
u_n_table

u_n_table = table(complete_data$HLTHPLN1,deparse.level = 2)
u_n_table



#### Quasi-Poisson Models ####

## Financial barriers fully adjusted survey-weighted log-quasipoisson model w/ period interaction ##
FB_P_results = svyglm(HIV_test~MEDCOST*COV_YEAR+HLTHPLN1+SEX+X_MRACE1+X_HISPANC+
                   X_AGEG5YR+INCOME2+EMPLOY1+RENTHOM1+X_EDUCAG,
                 design = design, family = quasipoisson(link="log"))
save(FB_P_results,file="FB_P_results.rds")

## number of responses ##
length(FB_P_results$residuals)

## 2020-2021 PR point estimate ##
FB_P_PR_COV=exp(coef(FB_P_results)["MEDCOST"]+coef(FB_P_results)["MEDCOST:COV_YEAR2020-2021"])
round(FB_P_PR_COV,3)

### 2020-2021 PR 95% CI ##
log.se.robust = sqrt(vcov(FB_P_results)["MEDCOST","MEDCOST"]+
                       vcov(FB_P_results)["MEDCOST:COV_YEAR2020-2021",
                                       "MEDCOST:COV_YEAR2020-2021"]+
                       2*vcov(FB_P_results)["MEDCOST","MEDCOST:COV_YEAR2020-2021"])

FB_P_CI_COV=exp(coef(FB_P_results)["MEDCOST"]+coef(FB_P_results)["MEDCOST:COV_YEAR2020-2021"]+
      c(-1,1)*1.96*log.se.robust)
round(FB_P_CI_COV,3)



## 2017-2019 PR point estimate ##
FB_P_PR_preCOV=exp(coef(FB_P_results)["MEDCOST"])
round(FB_P_PR_preCOV,3)

### 2017-2019 PR 95% CI ##
FB_P_CI_COV=coefci(FB_P_results,vcov.=vcov(FB_P_results))[2,] %>% exp()
round(FB_P_CI_COV,3)



## Healthcare coverage fully adjusted survey-weighted log-quasipoisson model w/ period interaction ##
HC_P_results = svyglm(HIV_test~HLTHPLN1*COV_YEAR+MEDCOST+SEX+X_MRACE1+X_HISPANC+
                        X_AGEG5YR+INCOME2+EMPLOY1+RENTHOM1+X_EDUCAG,
                      design = design, family = quasipoisson(link="log"))
save(HC_P_results,file="HC_P_results.rds")


## 2020-2021 PR point estimate ##
HC_P_PR_COV=exp(coef(HC_P_results)["HLTHPLN1"]+coef(HC_P_results)["HLTHPLN1:COV_YEAR2020-2021"])
round(HC_P_PR_COV,3)

### 2020-2021 PR 95% CI ##
log.se.robust = sqrt(vcov(HC_P_results)["HLTHPLN1","HLTHPLN1"]+
                       vcov(HC_P_results)["HLTHPLN1:COV_YEAR2020-2021",
                                            "HLTHPLN1:COV_YEAR2020-2021"]+
                       2*vcov(HC_P_results)["HLTHPLN1","HLTHPLN1:COV_YEAR2020-2021"])

HC_P_CI_COV=exp(coef(HC_P_results)["HLTHPLN1"]+coef(HC_P_results)["HLTHPLN1:COV_YEAR2020-2021"]+
                  c(-1,1)*1.96*log.se.robust)
round(HC_P_CI_COV,3)


## 2017-2019 PR point estimate ##
HC_P_PR_preCOV=exp(coef(HC_P_results)["HLTHPLN1"])
round(HC_P_PR_preCOV,3)

### 2017-2019 PR 95% CI ##
HC_P_CI_preCOV=coefci(HC_P_results,vcov.=vcov(HC_P_results))[2,] %>% exp()
round(HC_P_CI_preCOV,3)


## Fully adjusted survey-weighted log-quasipoisson model no period interaction ##
results = svyglm(HIV_test~MEDCOST+HLTHPLN1+COV_YEAR+SEX+X_MRACE1+X_HISPANC+
                        X_AGEG5YR+INCOME2+EMPLOY1+RENTHOM1+X_EDUCAG,
                      design = design, family = quasipoisson(link="log"))
save(results,file="results.rds")


## PR point estimates ##
FB_PR=exp(coef(results)["MEDCOST"])
round(FB_PR,3)

HC_PR=exp(coef(results)["HLTHPLN1"])
round(HC_PR,3)

### PR 95% CIs ##
FB_CI=coefci(results,vcov.=vcov(results))[2,] %>% exp()
HC_CI=coefci(results,vcov.=vcov(results))[3,] %>% exp()
round(FB_CI,3)
round(HC_CI,3)


#### Stratified models ####

