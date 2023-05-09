################################################################################
# Script purpose: To produce a results table for the secondary aims
# Author: Austin Hammermeister Suger 
# Last Updated: 5/9/2023
# Required dependencies:
# tidyverse
# data.table
# epiR
################################################################################
## Clear the global environment ##
rm(list=ls())

## Load the required dependencies ## 
library(tidyverse)
library(epiR)
library(data.table)

## Set the working directory ## 
setwd("C:/Users/Austin Hammermeister/Desktop/PhD/Year 1 Coursework/SPR_2023/EPI 514/Group Project/")

## Load the cleaned data set ##
load("clean_BRFSS.RData")

## Subset to the complete case set ##

complete_data = BRFSS_merged %>% filter(is.na(HIV_test)== FALSE & 
                                          is.na(COV_YEAR)== FALSE &
                                          is.na(MEDCOST)== FALSE & 
                                          is.na(HLTHPLN1) == FALSE)

# 285,885 individuals (13.5% of all responses) are missing data for at least one of the exposures or outcomes

## Apply descriptive levels to the relevant variables ##

# HIV test within the BRFSS year #

complete_data$HIV_test = complete_data$HIV_test %>% factor(levels = c(1,0),
                                                           labels = c("HIV test within the BRFSS survey year",
                                                                      "No HIV test within the BRFSS survey year"))
# COVID-19 Year #

complete_data$COV_YEAR = complete_data$COV_YEAR %>% factor(levels = c(0,1),
                                                           labels = c("2017-2019","2020-2021"))

# Financial barriers to care #

complete_data$MEDCOST = complete_data$MEDCOST %>% factor(levels = c(1,2),
                                                         labels = c("Barriers","No barriers"))

# Healthcare coverage #

complete_data$HLTHPLN1 = complete_data$HLTHPLN1 %>% factor(levels = c(2,1),
                                                           labels = c("No coverage","Coverage"))

# Sex #

complete_data$SEX = complete_data$SEX %>% factor(levels = c(1,2),
                                               labels = c("Male","Female"))
## Financial barriers ##

table=table(complete_data$MEDCOST,complete_data$HIV_test,complete_data$SEX,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

## Heath coverage ##

table=table(complete_data$HLTHPLN1,complete_data$HIV_test,complete_data$SEX,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)



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

## Financial barriers ##

table=table(complete_data$MEDCOST,complete_data$HIV_test,complete_data$X_RACE,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

## Heath coverage ##

table=table(complete_data$HLTHPLN1,complete_data$HIV_test,complete_data$X_RACE,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)


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
## Financial barriers ##

table=table(complete_data$MEDCOST,complete_data$HIV_test,complete_data$AGEG20YR,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

## Heath coverage ##

table=table(complete_data$HLTHPLN1,complete_data$HIV_test,complete_data$AGEG20YR,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)


# Urban/rural status #
complete_data$X_URBSTAT = complete_data$X_URBSTAT %>% factor(levels = c(1,2),
                                                           labels = c("Urban counties","Rural counties"))

## Financial barriers ##

table=table(complete_data$MEDCOST,complete_data$HIV_test,complete_data$X_URBSTAT,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

## Heath coverage ##

table=table(complete_data$HLTHPLN1,complete_data$HIV_test,complete_data$X_URBSTAT,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

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

## Financial barriers ##

table=table(complete_data$MEDCOST,complete_data$HIV_test,complete_data$EMPLOY1,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

## Heath coverage ##

table=table(complete_data$HLTHPLN1,complete_data$HIV_test,complete_data$EMPLOY1,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

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

## Financial barriers ##

table=table(complete_data$MEDCOST,complete_data$HIV_test,complete_data$INCOME2,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

## Heath coverage ##

table=table(complete_data$HLTHPLN1,complete_data$HIV_test,complete_data$INCOME2,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)


# Home ownership #
complete_data$RENTHOM1 = complete_data$RENTHOM1 %>% factor(levels = c(1,2,3),
                                                         labels = c("Own","Rent",
                                                                    "Other"))

## Financial barriers ##

table=table(complete_data$MEDCOST,complete_data$HIV_test,complete_data$RENTHOM1,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

## Heath coverage ##

table=table(complete_data$HLTHPLN1,complete_data$HIV_test,complete_data$RENTHOM1,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)


# Level of education #
complete_data$X_EDUCAG = complete_data$X_EDUCAG %>% factor(levels = c(1,2,3,4),
                                                         labels = c("Less than high school","High school",
                                                                    "Some college or technical school",
                                                                    "College or technical school"))
## Financial barriers ##

table=table(complete_data$MEDCOST,complete_data$HIV_test,complete_data$X_EDUCAG,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

## Heath coverage ##

table=table(complete_data$HLTHPLN1,complete_data$HIV_test,complete_data$X_EDUCAG,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)
