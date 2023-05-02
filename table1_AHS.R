################################################################################
# Script purpose: To produce a table of survey weighted descriptive statistics
# Author: Austin Hammermeister Suger 
# Last Updated: 5/2/2023
# Required dependencies:
  # tidyverse
  # foreign
  # data.table
  # gtsummary
  # webshot2
################################################################################

## Clear the global environment ##
rm(list=ls())

## Load the required dependencies ## 
library(data.table)
library(survey)
library(tidyverse)
library(gtsummary)
library(webshot2)

## Set the working directory ## 
setwd("C:/Users/Austin Hammermeister/Desktop/PhD/Year 1 Coursework/SPR_2023/EPI 514/Group Project/")

## Load the cleaned data set ##
load("clean_BRFSS.RData")

## Apply descriptive levels to the relevant variables ##

# HIV test within the BRFSS year #

BRFSS_merged$HIV_test = BRFSS_merged$HIV_test %>% factor(levels = c(0,1),
                                         labels = c("No HIV test within the BRFSS survey year", "HIV test within the BRFSS survey year"))

# COVID-19 Year #

BRFSS_merged$COV_YEAR = BRFSS_merged$COV_YEAR %>% factor(levels = c(0,1),
                                            labels = c("2017-2019","2020-2021"))

# Healthcare access #


BRFSS_merged$HLTHPLN1 = BRFSS_merged$HLTHPLN1 %>% factor(levels = c(1,2),
                               labels = c("Coverage","No coverage"))

# Financial barriers to care #

BRFSS_merged$MEDCOST = BRFSS_merged$MEDCOST %>% factor(levels = c(1,2),
                                         labels = c("Barriers","No barriers"))

# Sex #

BRFSS_merged$SEX = BRFSS_merged$SEX %>% factor(levels = c(1,2),
                                         labels = c("Male","Female"))

# Race/ethnicity #

BRFSS_merged$X_RACE = BRFSS_merged$X_RACE %>% factor(levels = c(1,2,3,4,5,6,7,8),
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

BRFSS_merged = BRFSS_merged %>% mutate(AGEG20YR = case_when(X_AGEG5YR<=3 ~ 1,
                                                            X_AGEG5YR>3 & X_AGEG5YR<=7 ~ 2,
                                                            X_AGEG5YR>7 & X_AGEG5YR<=11 ~ 3,
                                                            X_AGEG5YR>11 ~ 4,))

BRFSS_merged$AGEG20YR = BRFSS_merged$AGEG20YR %>% factor(levels = c(1,2,3,4),
                                                       labels = c("18-34",
                                                                  "35-54",
                                                                  "55-74",
                                                                  "75+"))


# Urban/rural status #
BRFSS_merged$X_URBSTAT = BRFSS_merged$X_URBSTAT %>% factor(levels = c(1,2),
                                       labels = c("Urban counties","Rural counties"))

# Employment #

BRFSS_merged$EMPLOY1 = BRFSS_merged$EMPLOY1 %>% factor(levels = c(1,2,3,4,5,6,7,8),
                                           labels = c("Employed for wages",
                                                      "Self-employed",
                                                      "Out of work (>=1 year)",
                                                      "Out of work (<1 year)",
                                                      "A homemaker",
                                                      "A student",
                                                      "Retired",
                                                      "Unable to work"))
# Annual household income #

BRFSS_merged$INCOME2 = BRFSS_merged$INCOME2 %>% factor(levels = c(1,2,3,4,5,6,7,8),
                                       labels = c("<$10K",
                                                  "$10-15K",
                                                  "$15-20K",
                                                  "$20-25K",
                                                  "$25-35K",
                                                  "$35-50K",
                                                  "$50-75K",
                                                  ">$75K"))

# Home ownership #
BRFSS_merged$RENTHOM1 = BRFSS_merged$RENTHOM1 %>% factor(levels = c(1,2,3),
                                           labels = c("Own","Rent",
                                                      "Other"))

# Level of education #
BRFSS_merged$X_EDUCAG = BRFSS_merged$X_EDUCAG %>% factor(levels = c(1,2,3,4),
                                         labels = c("Less than high school","High school",
                                                    "Some college or technical school",
                                                    "College or technical school"))

# Sexual orientation #

# Create a single sexual orientation variable 
BRFSS_merged = BRFSS_merged %>% mutate(SO = case_when(SOMALE==1 | SOFEMALE==1 ~ 1,
                                      SOMALE==2 | SOFEMALE==2 ~ 2,
                                      SOMALE==3 | SOFEMALE==3 ~ 3,
                                      SOMALE==4 | SOFEMALE==4 ~ 4))

BRFSS_merged$SO = BRFSS_merged$SO %>% factor(levels = c(1,2,3,4),
                                         labels = c("Gay or Lesbian",
                                                    "Straight",
                                                    "Bisexual",
                                                    "Something else"))

# Transgender status #

BRFSS_merged$TRNSGNDR = BRFSS_merged$TRNSGNDR %>% factor(levels = c(1,2,3,4),
                             labels = c("Transgender, male-to-female",
                                        "Transgender, female-to-male",
                                        "Transgender, gender nonconforming",
                                        "Not transgender"))


# Create the survey design object for the full data set
design <- svydesign(data = BRFSS_merged,
                    id = ~X_PSU, strata = ~X_STSTR, weights = ~LLCPWT_5Y, nest = TRUE)
options(survey.lonely.psu = "adjust")


# Create a table 1 for the full data set
table_1 <-
  design %>%
  tbl_svysummary(
    by = COV_YEAR,
    include = c(HIV_test,HLTHPLN1,MEDCOST,SEX,AGEG20YR, X_RACE,X_URBSTAT,
                EMPLOY1,INCOME2,RENTHOM1,X_EDUCAG,SO,TRNSGNDR),
    label=list(HIV_test~ "HIV test within the BRFSS survey year", 
               HLTHPLN1 ~ "Healthcare coverage",
               MEDCOST ~ "Financial barriers to healthcare",SEX ~ "Sex",
               AGEG20YR ~ "Age group", 
               X_RACE ~ "Race/ethnicity", X_URBSTAT ~ "Urban/rural status",
               EMPLOY1 ~ "Employment status",
               INCOME2 ~ "Annual household income", RENTHOM1 ~ "Home ownership",
               X_EDUCAG ~ "Level of education", SO ~ "Sexual orientation",
               TRNSGNDR ~ "Transgender status"),
    statistic = list(all_categorical() ~ "{n} ({p}%) [{p_unweighted}%]"),
    missing="ifany",
    missing_text="Missing/Don't know/Refused",
  ) %>%
  modify_caption("<b>Table 1: National BRFSS respondent characteristics prior to (2017-2019) and during the COVID-19 pandemic (2020-2021).</b>") %>% 
  bold_labels() %>%
  add_n() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Rural/urban status was not collected in 2017; Sexual orientation and transgender questions are only asked by a subset of states each year*"))

# Export .png
gt::gtsave(table_1, file = "group24_table1.png")
