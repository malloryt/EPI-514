################################################################################
# Script purpose: To produce a results table for the primary aims
# Author: Austin Hammermeister Suger 
# Last Updated: 5/4/2023
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

u_n_table = table(complete_data$HIV_test)
u_p_table = u_n_table %>% prop.table() %>% round(3)*100
u_n_table
u_p_table

# COVID-19 Year #

complete_data$COV_YEAR = complete_data$COV_YEAR %>% factor(levels = c(0,1),
                                                         labels = c("2017-2019","2020-2021"))

# Healthcare coverage #

complete_data$HLTHPLN1 = complete_data$HLTHPLN1 %>% factor(levels = c(2,1),
                                                         labels = c("No coverage","Coverage"))


u_n_table = table(complete_data$HLTHPLN1,complete_data$HIV_test)
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_n_table
u_p_table

# Financial barriers to care #

complete_data$MEDCOST = complete_data$MEDCOST %>% factor(levels = c(1,2),
                                                       labels = c("Barriers","No barriers"))
u_n_table = table(complete_data$MEDCOST,complete_data$HIV_test)
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_n_table
u_p_table

## Financial barriers prevalence ratios ##

table=table(complete_data$MEDCOST,complete_data$HIV_test,complete_data$COV_YEAR,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR #
round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

# Stratum-specific PRs #

round(results$massoc.detail$PR.strata.wald,2)

## Healthcare coverage prevalence ratios ##

table=table(complete_data$HLTHPLN1,complete_data$HIV_test,complete_data$COV_YEAR,deparse.level = 2)
results=epi.2by2(table, method = "cross.sectional")

# Crude PR #
round(results$massoc.detail$PR.crude.wald,2)

# MH adjusted PR #

round(results$massoc.detail$PR.mh.wald,2)

# Stratum-specific PRs #

round(results$massoc.detail$PR.strata.wald,2)




