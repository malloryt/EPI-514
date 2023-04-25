################################################################################
# Script purpose: To load, clean, recode, reweight and merge BRFSS years 2017-21
# Author: Austin Hammermeister Suger 
# Last Updated: 4/25/2023
# Required dependencies:
  # tidyverse
  # foreign

################################################################################

## Clear the global environment ##
rm(list=ls())

## Load the required dependencies ## 
library(tidyverse)
library(foreign)

## Set the working directory ## 
setwd("C:/Users/Austin Hammermeister/Desktop/PhD/Year 1 Coursework/SPR_2023/EPI 514/Group Project/")


## BRFSS Year 2017 ##

# Load SAS transport files #
BRFSS_2017 = read.xport("LLCP2017.XPT") 

# Subset to relevant variables #
variables = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
              "X_LLCPWT","X_STSTR","X_AGEG5YR","SEX","X_RACE","EMPLOY1","INCOME2",
              "RENTHOM1","X_EDUCAG","SXORIENT","TRNSGNDR","MEDCOST","HLTHPLN1",
              "HIVTST6","HIVTSTD3")
subset_2017 = BRFSS_2017 %>% select(variables)

## BRFSS Year 2018 ##

# Load SAS transport files #
BRFSS_2018 = read.xport("LLCP2018.XPT") 

# Subset to relevant variables #
variables = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
                   "X_LLCPWT","X_STSTR","X_AGEG5YR","SEX1","X_RACE","EMPLOY1",
              "INCOME2","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
              "TRNSGNDR","MEDCOST","HLTHPLN1","HIVTST6","HIVTSTD3")
subset_2018 = BRFSS_2018 %>% select(variables)

## BRFSS Year 2019 ##

# Load SAS transport files #
BRFSS_2019 = read.xport("LLCP2019.XPT") 

# Subset to relevant variables #
variables = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
              "X_LLCPWT","X_STSTR","X_AGEG5YR","SEXVAR","X_RACE","EMPLOY1",
              "INCOME2","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
              "BIRTHSEX","TRNSGNDR","MEDCOST","HLTHPLN1","HIVTST7","HIVTSTD3")
subset_2019 = BRFSS_2019 %>% select(variables)

## BRFSS Year 2020 ##

# Load SAS transport files #
BRFSS_2020 = read.xport("LLCP2020.XPT") 

# Subset to relevant variables #
variables = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
              "X_LLCPWT","X_STSTR","X_AGEG5YR","SEXVAR","X_RACE","EMPLOY1",
              "INCOME2","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
              "BIRTHSEX", "TRNSGNDR","MEDCOST","HLTHPLN1","HIVTST7","HIVTSTD3")
subset_2020 = BRFSS_2020 %>% select(variables)

# Load SAS transport files #
BRFSS_2021 = read.xport("LLCP2021.XPT") 

# Subset to relevant variables #
variables = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
              "X_LLCPWT","X_STSTR","X_AGEG5YR","SEXVAR","X_RACE","EMPLOY1",
              "INCOME3","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
              "TRNSGNDR","BIRTHSEX","MEDCOST1","PRIMINSR","HIVTST7","HIVTSTD3")
subset_2021 = BRFSS_2021 %>% select(variables)



