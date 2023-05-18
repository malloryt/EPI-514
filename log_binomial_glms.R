################################################################################
# Script purpose: Survey weighted glm for main analyses
# Author: Austin Hammermeister Suger 
# Last Updated: 5/15/2023
# Required dependencies:
# tidyverse
# data.table
# survey
################################################################################

## Clear the global environment ##
rm(list=ls())

## Load the required dependencies ## 
library(data.table)
library(survey)
library(tidyverse)
library(broom)
library(finalfit)
library(kableExtra)
library(lmtest)

## Set the working directory ## 
setwd("C:/Users/Austin Hammermeister/Desktop/PhD/Year 1 Coursework/SPR_2023/EPI 514/Group Project/")

## Load the cleaned data set ##
load("clean_BRFSS.RData")

## Subset to the complete case set ##

complete_data = BRFSS_merged %>% filter(is.na(HIV_test)== FALSE & 
                                          is.na(COV_YEAR)== FALSE &
                                          is.na(MEDCOST)== FALSE & 
                                          is.na(HLTHPLN1) == FALSE)

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

# Collapse age groups #

complete_data = complete_data %>% mutate(AGEG20YR = case_when(X_AGEG5YR<=3 ~ 1,
                                                              X_AGEG5YR>3 & X_AGEG5YR<=7 ~ 2,
                                                              X_AGEG5YR>7 & X_AGEG5YR<=11 ~ 3,
                                                              X_AGEG5YR>11 ~ 4,))

complete_data$AGEG20YR = complete_data$AGEG20YR %>% factor(levels = c(1,2,3,4),
                                                           labels = c("18-34",
                                                                      "35-54",
                                                                      "55-74",
                                                                      "75+"))


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

# Collapse annual household income #

complete_data = complete_data %>% mutate(INCOME = case_when(INCOME2 %in% c("<$10K","$10-15K","$15-20K") ~ 1,
                                                              INCOME2 %in% c("$20-25K","$25-35K","$35-50K") ~ 2,
                                                              INCOME2 %in% c("$50-75K") ~ 3,
                                                              INCOME2 %in% c(">$75K") ~ 4,))

complete_data$INCOME = complete_data$INCOME %>% factor(levels = c(1,2,3,4),
                                                           labels = c("<$20,000",
                                                                      "$20,000 to <$50,0000",
                                                                      "$50,000 to <$75,000",
                                                                      "$75,000+"))



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


## Fit an unweighted log-binomial glm ##

# Minimally adjusted #

results = glm(HIV_test~MEDCOST*COV_YEAR+HLTHPLN1*COV_YEAR+X_RACE+SEX+AGEG20YR+INCOME, data = complete_data, family = binomial(link="log"))
reduced_results1 = glm(HIV_test~MEDCOST+HLTHPLN1*COV_YEAR+X_RACE+SEX+AGEG20YR+INCOME, data = complete_data, family = binomial(link="log"))
reduced_results2 = glm(HIV_test~MEDCOST*COV_YEAR+HLTHPLN1+COV_YEAR+X_RACE+SEX+AGEG20YR+INCOME, data = complete_data, family = binomial(link="log"))

lrtest(results,reduced_results1)
lrtest(results,reduced_results2)


## Create a tidy data frame with exponentiated regression coefficients and CIs
tidy_results = tidy(results,conf.int = FALSE, pars = "coefs")

## Create a table for model results

est = tidy_results[,"estimate"] %>% exp()%>% round(2)
lower_ci = (tidy_results[,"estimate"]-(1.96*tidy_results[,"std.error"])) %>% 
  exp() %>% round(2) %>% unlist() %>% as.vector()
upper_ci = (tidy_results[,"estimate"]+(1.96*tidy_results[,"std.error"])) %>% 
  exp() %>% round(2) %>% unlist() %>% as.vector()
p = tidy_results[,"p.value"] %>% unlist() %>% as.vector() %>% as.numeric() %>%
  finalfit::p_tidy(digits = 3,prefix = NULL)

table = data.frame(Est = est, 
                   CI  = paste0("(",lower_ci,"-",upper_ci,")"),
                   p = p)
colnames(table) = c("PR estimate","95% CI","Wald test p-value")
rownames(table) = tidy_results$term

table[,1:2]
table


# Fully adjusted without SOGI variables #

results = glm(HIV_test~MEDCOST*COV_YEAR+HLTHPLN1*COV_YEAR+SEX+X_RACE+AGEG20YR+INCOME+EMPLOY1+
                   RENTHOM1+X_EDUCAG+X_URBSTAT,
              data = complete_data, family = binomial(link="log"))

reduced_results1 = glm(HIV_test~MEDCOST+COV_YEAR*HLTHPLN1+SEX+X_RACE+AGEG20YR+INCOME+EMPLOY1+
                        RENTHOM1+X_EDUCAG+X_URBSTAT,
                       data = complete_data, family = binomial(link="log"))

reduced_results2 = glm(HIV_test~MEDCOST*COV_YEAR+HLTHPLN1+SEX+X_RACE+AGEG20YR+INCOME+EMPLOY1+
                         RENTHOM1+X_EDUCAG+X_URBSTAT,
                       data = complete_data, family = binomial(link="log"))


lrtest(results,reduced_results1)
lrtest(results,reduced_results2)



## Create a tidy data frame with exponentiated regression coefficients and CIs
tidy_results = tidy(results,conf.int = FALSE, pars = "coefs")

## Create a table for model results

est = tidy_results[,"estimate"] %>% exp()%>% round(2)
lower_ci = (tidy_results[,"estimate"]-(1.96*tidy_results[,"std.error"])) %>% 
  exp() %>% round(2) %>% unlist() %>% as.vector()
upper_ci = (tidy_results[,"estimate"]+(1.96*tidy_results[,"std.error"])) %>% 
  exp() %>% round(2) %>% unlist() %>% as.vector()
p = tidy_results[,"p.value"] %>% unlist() %>% as.vector() %>% as.numeric() %>%
  finalfit::p_tidy(digits = 3,prefix = NULL)

table = data.frame(Est = est, 
                          CI  = paste0("(",lower_ci,"-",upper_ci,")"),
                          p = p)
colnames(table) = c("PR estimate","95% CI","Wald test p-value")
rownames(table) = tidy_results$term

table[,1:2]
table

