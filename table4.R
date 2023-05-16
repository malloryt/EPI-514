################################################################################
# Script purpose: To produce a results table for the secondary aims
# Author: Austin Hammermeister Suger 
# Last Updated: 5/11/2023
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

# Create a single sexual orientation variable 
complete_data = complete_data %>% mutate(SO = case_when(SOMALE==1 | SOFEMALE==1 ~ 1,
                                                      SOMALE==2 | SOFEMALE==2 ~ 2,
                                                      SOMALE==3 | SOFEMALE==3 ~ 3,
                                                      SOMALE==4 | SOFEMALE==4 ~ 4))

## Subset states which used the SOGI modules for all 5 years ##

non_miss = complete_data %>% filter(is.na(SO)== FALSE & 
                                 is.na(TRNSGNDR)== FALSE)

year_by_state = table(non_miss$state_name,non_miss$BRFSS_YEAR) %>% as.data.frame()
missing_year = year_by_state %>% filter(Freq==0) %>% select(Var1) %>% unique()

`%!in%` = Negate(`%in%`)

complete_data = non_miss %>% filter(state_name %!in% missing_year$Var1)

# Double check work #

table(complete_data$state_name,complete_data$BRFSS_YEAR)

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
# Sexual orientation #
BRFSS_merged$SO = BRFSS_merged$SO %>% factor(levels = c(1,2,3,4),
                                             labels = c("Gay or Lesbian",
                                                        "Straight",
                                                        "Bisexual",
                                                        "Something else"))

## Cost barriers ##
table=table(complete_data$MEDCOST,complete_data$HIV_test,complete_data$SO,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

## Heath coverage ##

table=table(complete_data$HLTHPLN1,complete_data$HIV_test,complete_data$SO,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)



# Transgender status #

BRFSS_merged$TRNSGNDR = BRFSS_merged$TRNSGNDR %>% factor(levels = c(1,2,3,4),
                                                         labels = c("Transgender, male-to-female",
                                                                    "Transgender, female-to-male",
                                                                    "Transgender, gender nonconforming",
                                                                    "Not transgender"))
## Cost barriers ##
table=table(complete_data$MEDCOST,complete_data$HIV_test,complete_data$TRNSGNDR,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

## Heath coverage ##

table=table(complete_data$HLTHPLN1,complete_data$HIV_test,complete_data$TRNSGNDR,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR # 

round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)


