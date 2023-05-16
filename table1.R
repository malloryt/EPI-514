################################################################################
# Script purpose: To produce a table of survey weighted descriptive statistics
# Author: Austin Hammermeister Suger 
# Last Updated: 5/12/2023
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

w_n_table
w_p_table
u_n_table
u_p_table

pre_total_N_weighted = w_n_table[1]
pre_total_N_unweighted = u_n_table[1]
during_total_N_weighted = w_n_table[2]
during_total_N_unweighted = u_n_table[2]
  
# Sex #

w_n_table = svytable(~SEX+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$SEX,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table

total_N_unweighted - sum(u_n_table) %>% round(0)
(total_N_unweighted - sum(u_n_table))/total_N_unweighted

total_N_weighted - sum(w_n_table) %>% round(0)
(total_N_weighted - sum(w_n_table))/total_N_weighted

w_n_table = svytable(~SEX,design=design)
u_n_table = table(complete_data$SEX)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table

# Age group #

w_n_table = svytable(~AGEG20YR+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$AGEG20YR,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table

total_N_unweighted - sum(u_n_table) %>% round(0)
(total_N_unweighted - sum(u_n_table))/total_N_unweighted

total_N_weighted - sum(w_n_table) %>% round(0)
(total_N_weighted - sum(w_n_table))/total_N_weighted

w_n_table = svytable(~AGEG20YR,design=design) %>% round(0)
u_n_table = table(complete_data$AGEG20YR)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


# Race/ethnicity #

w_n_table = svytable(~X_RACE+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$X_RACE,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


total_N_unweighted - sum(u_n_table) %>% round(0)
(total_N_unweighted - sum(u_n_table))/total_N_unweighted

total_N_weighted - sum(w_n_table) %>% round(0)
(total_N_weighted - sum(w_n_table))/total_N_weighted

w_n_table = svytable(~X_RACE,design=design) %>% round(0)
u_n_table = table(complete_data$X_RACE)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table

# Urban/rural status #

w_n_table = svytable(~X_URBSTAT+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$X_URBSTAT,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


total_N_unweighted - sum(u_n_table) %>% round(0)
(total_N_unweighted - sum(u_n_table))/total_N_unweighted

total_N_weighted - sum(w_n_table) %>% round(0)
(total_N_weighted - sum(w_n_table))/total_N_weighted

pre_total_N_unweighted - sum(u_n_table[,1]) %>% round(0)
(pre_total_N_unweighted - sum(u_n_table[,1]))/pre_total_N_unweighted

pre_total_N_weighted - sum(w_n_table[,1]) %>% round(0)
(pre_total_N_weighted - sum(w_n_table[,1]))/pre_total_N_weighted

during_total_N_unweighted - sum(u_n_table[,2]) %>% round(0)
(during_total_N_unweighted - sum(u_n_table[,2]))/during_total_N_unweighted

during_total_N_weighted - sum(w_n_table[,2]) %>% round(0)
(during_total_N_weighted - sum(w_n_table[,2]))/during_total_N_weighted

w_n_table = svytable(~X_URBSTAT,design=design) %>% round(0)
u_n_table = table(complete_data$X_URBSTAT)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table

# Employment #

w_n_table = svytable(~EMPLOY1+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$EMPLOY1,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


total_N_unweighted - sum(u_n_table) %>% round(0)
(total_N_unweighted - sum(u_n_table))/total_N_unweighted

total_N_weighted - sum(w_n_table) %>% round(0)
(total_N_weighted - sum(w_n_table))/total_N_weighted

w_n_table = svytable(~EMPLOY1,design=design) %>% round(0)
u_n_table = table(complete_data$EMPLOY1)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


# Annual household income #

w_n_table = svytable(~INCOME2+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$INCOME2,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


total_N_unweighted - sum(u_n_table) %>% round(0)
(total_N_unweighted - sum(u_n_table))/total_N_unweighted

total_N_weighted - sum(w_n_table) %>% round(0)
(total_N_weighted - sum(w_n_table))/total_N_weighted

pre_total_N_unweighted - sum(u_n_table[,1]) %>% round(0)
(pre_total_N_unweighted - sum(u_n_table[,1]))/pre_total_N_unweighted

pre_total_N_weighted - sum(w_n_table[,1]) %>% round(0)
(pre_total_N_weighted - sum(w_n_table[,1]))/pre_total_N_weighted

during_total_N_unweighted - sum(u_n_table[,2]) %>% round(0)
(during_total_N_unweighted - sum(u_n_table[,2]))/during_total_N_unweighted

during_total_N_weighted - sum(w_n_table[,2]) %>% round(0)
(during_total_N_weighted - sum(w_n_table[,2]))/during_total_N_weighted

w_n_table = svytable(~INCOME2,design=design) %>% round(0)
u_n_table = table(complete_data$INCOME2)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table

# Home ownership #

w_n_table = svytable(~RENTHOM1+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$RENTHOM1,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


total_N_unweighted - sum(u_n_table) %>% round(0)
(total_N_unweighted - sum(u_n_table))/total_N_unweighted

total_N_weighted - sum(w_n_table) %>% round(0)
(total_N_weighted - sum(w_n_table))/total_N_weighted

w_n_table = svytable(~RENTHOM1,design=design) %>% round(0)
u_n_table = table(complete_data$RENTHOM1)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


# Level of education #

w_n_table = svytable(~X_EDUCAG+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$X_EDUCAG,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


total_N_unweighted - sum(u_n_table) %>% round(0)
(total_N_unweighted - sum(u_n_table))/total_N_unweighted

total_N_weighted - sum(w_n_table) %>% round(0)
(total_N_weighted - sum(w_n_table))/total_N_weighted

w_n_table = svytable(~X_EDUCAG,design=design) %>% round(0)
u_n_table = table(complete_data$X_EDUCAG)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


# Sexual orientation #

w_n_table = svytable(~SO+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$SO,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


total_N_unweighted - sum(u_n_table) %>% round(0)
(total_N_unweighted - sum(u_n_table))/total_N_unweighted

total_N_weighted - sum(w_n_table) %>% round(0)
(total_N_weighted - sum(w_n_table))/total_N_weighted

pre_total_N_unweighted - sum(u_n_table[,1]) %>% round(0)
(pre_total_N_unweighted - sum(u_n_table[,1]))/pre_total_N_unweighted

pre_total_N_weighted - sum(w_n_table[,1]) %>% round(0)
(pre_total_N_weighted - sum(w_n_table[,1]))/pre_total_N_weighted

during_total_N_unweighted - sum(u_n_table[,2]) %>% round(0)
(during_total_N_unweighted - sum(u_n_table[,2]))/during_total_N_unweighted

during_total_N_weighted - sum(w_n_table[,2]) %>% round(0)
(during_total_N_weighted - sum(w_n_table[,2]))/during_total_N_weighted

w_n_table = svytable(~SO,design=design) %>% round(0)
u_n_table = table(complete_data$SO)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


# Transgender status #

w_n_table = svytable(~TRNSGNDR+COV_YEAR,design=design) %>% round(0)
u_n_table = table(complete_data$TRNSGNDR,complete_data$COV_YEAR)

w_p_table = w_n_table %>% prop.table(margin = 2) %>% round(3)*100
u_p_table = u_n_table %>% prop.table(margin = 2) %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table


total_N_unweighted - sum(u_n_table) %>% round(0)
(total_N_unweighted - sum(u_n_table))/total_N_unweighted

total_N_weighted - sum(w_n_table) %>% round(0)
(total_N_weighted - sum(w_n_table))/total_N_weighted

pre_total_N_unweighted - sum(u_n_table[,1]) %>% round(0)
(pre_total_N_unweighted - sum(u_n_table[,1]))/pre_total_N_unweighted

pre_total_N_weighted - sum(w_n_table[,1]) %>% round(0)
(pre_total_N_weighted - sum(w_n_table[,1]))/pre_total_N_weighted

during_total_N_unweighted - sum(u_n_table[,2]) %>% round(0)
(during_total_N_unweighted - sum(u_n_table[,2]))/during_total_N_unweighted

during_total_N_weighted - sum(w_n_table[,2]) %>% round(0)
(during_total_N_weighted - sum(w_n_table[,2]))/during_total_N_weighted

w_n_table = svytable(~TRNSGNDR,design=design) %>% round(0)
u_n_table = table(complete_data$TRNSGNDR)

w_p_table = w_n_table %>% prop.table() %>% round(3)*100
u_p_table = u_n_table %>% prop.table() %>% round(3)*100

w_n_table
w_p_table
u_n_table
u_p_table
