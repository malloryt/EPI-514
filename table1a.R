################################################################################
# Script purpose: To produce a table of survey weighted descriptive statistics
# Author: Austin Hammermeister Suger 
# Last Updated: 5/31/2023
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


## Create the survey design object for the full data set ##
design <- svydesign(data = complete_data,
                    id = ~X_PSU, strata = ~X_STSTR, weights = ~LLCPWT_5Y, nest = TRUE)
options(survey.lonely.psu = "adjust")

## Calculate survey weighted and unweighted counts and percentages

# COVID-19 Year #

total_N_weighted = sum(weights(design)) %>% round(0)
total_N_unweighted = nrow(complete_data)

w_n_table = svytable(~COV_YEAR,design=design)
u_n_table = table(complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

u_n_table
w_p_table

pre_total_N_weighted = w_n_table[1]
pre_total_N_unweighted = u_n_table[1]
during_total_N_weighted = w_n_table[2]
during_total_N_unweighted = u_n_table[2]
  
# Sex #

w_n_table = svytable(~SEX+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$SEX,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_p_table
u_n_table

w_n_table = svytable(~SEX,design=design)
u_n_table = table(complete_data$SEX)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_p_table
u_n_table

# Age group #

w_n_table = svytable(~X_AGEG5YR+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$X_AGEG5YR,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_p_table
u_n_table

w_n_table = svytable(~X_AGEG5YR,design=design) %>% round(0)
u_n_table = table(complete_data$X_AGEG5YR)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_p_table
u_n_table


# Race #

w_n_table = svytable(~X_MRACE1+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$X_MRACE1,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_p_table
u_n_table

w_n_table = svytable(~X_MRACE1,design=design) %>% round(0)
u_n_table = table(complete_data$X_MRACE1)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_p_table
u_n_table

# Ethnicity #

w_n_table = svytable(~X_HISPANC+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$X_HISPANC,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_p_table
u_n_table

w_n_table = svytable(~X_HISPANC,design=design) %>% round(0)
u_n_table = table(complete_data$X_HISPANC)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_p_table
u_n_table


# Employment #

w_n_table = svytable(~EMPLOY1+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$EMPLOY1,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_p_table
u_n_table


w_n_table = svytable(~EMPLOY1,design=design) %>% round(0)
u_n_table = table(complete_data$EMPLOY1)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_p_table
u_n_table


# Annual household income #

w_n_table = svytable(~INCOME2+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$INCOME2,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_p_table
u_n_table


w_n_table = svytable(~INCOME2,design=design) %>% round(0)
u_n_table = table(complete_data$INCOME2)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_p_table
u_n_table

# Home ownership #

w_n_table = svytable(~RENTHOM1+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$RENTHOM1,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_p_table
u_n_table

w_n_table = svytable(~RENTHOM1,design=design) %>% round(0)
u_n_table = table(complete_data$RENTHOM1)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_p_table
u_n_table


# Level of education #
w_n_table = svytable(~X_EDUCAG+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$X_EDUCAG,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_p_table
u_n_table

w_n_table = svytable(~X_EDUCAG,design=design) %>% round(0)
u_n_table = table(complete_data$X_EDUCAG)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_p_table
u_n_table
