################################################################################
# Script purpose: To load, clean, recode, reweight and merge BRFSS years 2017-21
# Author: Austin Hammermeister Suger 
# Last Updated: 4/27/2023
# Required dependencies:
  # tidyverse
  # foreign
  # data.table
################################################################################

## Clear the global environment ##
rm(list=ls())

## Load the required dependencies ## 
library(tidyverse)
library(foreign)
library(data.table)

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

# Add an empty urban status variable for merging #
subset_2017$X_URBSTAT = NA

# Add an empty BIRTHSEX variable for merging #
subset_2017$BIRTHSEX = NA

# Split the sexual orientation variable into sex-specific variables #
subset_2017 = subset_2017 %>% mutate(SOMALE = case_when(SXORIENT==1 & SEX==1 ~ 1,
                                                        SXORIENT==2 & SEX==1 ~ 2,
                                                        SXORIENT==3 & SEX==1 ~ 3,
                                                        SXORIENT==4 & SEX==1 ~ 4),
                                     SOFEMALE = case_when(SXORIENT==1 & SEX==2 ~ 1,
                                                        SXORIENT==2 & SEX==2 ~ 2,
                                                        SXORIENT==3 & SEX==2 ~ 3,
                                                        SXORIENT==4 & SEX==2 ~ 4))
# Remove the single sexual orientation variable #
subset_2017 = subset_2017 %>% select(-SXORIENT)

# Add a BRFSS year variable #
subset_2017$BRFSS_YEAR = 2017


# Define a common column order #
columns = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
            "X_LLCPWT","X_STSTR","X_AGEG5YR","SEX","X_RACE","EMPLOY1",
            "INCOME2","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
            "BIRTHSEX","TRNSGNDR","MEDCOST","HLTHPLN1","HIVTST6","HIVTSTD3",
            "BRFSS_YEAR")

subset_2017 = subset_2017[,columns]

# Remove the full 2017 data set from memory #
rm(BRFSS_2017)



## BRFSS Year 2018 ##

# Load SAS transport files #
BRFSS_2018 = read.xport("LLCP2018.XPT") 

# Subset to relevant variables #
variables = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
                   "X_LLCPWT","X_STSTR","X_AGEG5YR","SEX1","X_RACE","EMPLOY1",
              "INCOME2","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
              "TRNSGNDR","MEDCOST","HLTHPLN1","HIVTST6","HIVTSTD3")
subset_2018 = BRFSS_2018 %>% select(variables)

# Add an empty BIRTHSEX variable for merging #
subset_2018$BIRTHSEX = NA

# Rename the sex variable #
names(subset_2018)[names(subset_2018) == 'SEX1'] <- 'SEX'

# Add a BRFSS year variable #
subset_2018$BRFSS_YEAR = 2018


# Define a common column order #
columns = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
            "X_LLCPWT","X_STSTR","X_AGEG5YR","SEX","X_RACE","EMPLOY1",
            "INCOME2","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
            "BIRTHSEX","TRNSGNDR","MEDCOST","HLTHPLN1","HIVTST6","HIVTSTD3",
            "BRFSS_YEAR")

subset_2018 = subset_2018[,columns]

# Remove the full 2018 data set from memory #
rm(BRFSS_2018)



## BRFSS Year 2019 ##

# Load SAS transport files #
BRFSS_2019 = read.xport("LLCP2019.XPT") 

# Subset to relevant variables #
variables = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
              "X_LLCPWT","X_STSTR","X_AGEG5YR","SEXVAR","X_RACE","EMPLOY1",
              "INCOME2","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
              "BIRTHSEX","TRNSGNDR","MEDCOST","HLTHPLN1","HIVTST7","HIVTSTD3")
subset_2019 = BRFSS_2019 %>% select(variables)

# Rename the sex variable #
names(subset_2019)[names(subset_2019) == 'SEXVAR'] <- 'SEX'

# Rename the HIV test indicator variable #
names(subset_2019)[names(subset_2019) == 'HIVTST7'] <- 'HIVTST6'

# Add a BRFSS year variable #
subset_2019$BRFSS_YEAR = 2019

# Define a common column order #
columns = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
            "X_LLCPWT","X_STSTR","X_AGEG5YR","SEX","X_RACE","EMPLOY1",
            "INCOME2","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
            "BIRTHSEX","TRNSGNDR","MEDCOST","HLTHPLN1","HIVTST6","HIVTSTD3",
            "BRFSS_YEAR")

subset_2019 = subset_2019[,columns]

# Remove the full 2019 data set from memory #
rm(BRFSS_2019)




## BRFSS Year 2020 ##

# Load SAS transport files #
BRFSS_2020 = read.xport("LLCP2020.XPT") 

# Subset to relevant variables #
variables = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
              "X_LLCPWT","X_STSTR","X_AGEG5YR","SEXVAR","X_RACE","EMPLOY1",
              "INCOME2","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
              "BIRTHSEX", "TRNSGNDR","MEDCOST","HLTHPLN1","HIVTST7","HIVTSTD3")
subset_2020 = BRFSS_2020 %>% select(variables)

# Rename the sex variable #
names(subset_2020)[names(subset_2020) == 'SEXVAR'] <- 'SEX'


# Rename the HIV test indicator variable #
names(subset_2020)[names(subset_2020) == 'HIVTST7'] <- 'HIVTST6'


# Add a BRFSS year variable #
subset_2020$BRFSS_YEAR = 2020


# Define a common column order #
columns = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
            "X_LLCPWT","X_STSTR","X_AGEG5YR","SEX","X_RACE","EMPLOY1",
            "INCOME2","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
            "BIRTHSEX","TRNSGNDR","MEDCOST","HLTHPLN1","HIVTST6","HIVTSTD3",
            "BRFSS_YEAR")

subset_2020 = subset_2020[,columns]

# Remove the full 2020 data set from memory #
rm(BRFSS_2020)




## BRFSS Year 2021 ##

# Load SAS transport files #
BRFSS_2021 = read.xport("LLCP2021.XPT") 

# Subset to relevant variables #
variables = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
              "X_LLCPWT","X_STSTR","X_AGEG5YR","SEXVAR","X_RACE","EMPLOY1",
              "INCOME3","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
              "TRNSGNDR","BIRTHSEX","MEDCOST1","X_HLTHPLN","HIVTST7","HIVTSTD3")
subset_2021 = BRFSS_2021 %>% select(variables)

# Recode INCOME3 to be equivalent to INCOME2 used by prior years #
subset_2021 = subset_2021 %>% mutate(INCOME2 = case_when(INCOME3 %in% c(8,9,10,11) ~ 8,
                                                         INCOME3 == 7 ~ 7,
                                                         INCOME3 == 6 ~ 6,
                                                         INCOME3 == 5 ~ 5,
                                                         INCOME3 == 4 ~ 4,
                                                         INCOME3 == 3 ~ 3,
                                                         INCOME3 == 2 ~ 2,
                                                         INCOME3 == 1 ~ 1,))
# Rename the health coverage variable #
names(subset_2021)[names(subset_2021) == 'MEDCOST1'] <- 'MEDCOST'

# Rename the health coverage variable #
names(subset_2021)[names(subset_2021) == 'X_HLTHPLN'] <- 'HLTHPLN1'

# Rename the sex variable #
names(subset_2021)[names(subset_2021) == 'SEXVAR'] <- 'SEX'

# Rename the HIV test indicator variable #
names(subset_2021)[names(subset_2021) == 'HIVTST7'] <- 'HIVTST6'


# Add a BRFSS year variable #
subset_2021$BRFSS_YEAR = 2021

# Define a common column order #
columns = c("X_STATE","FMONTH","IDATE","IYEAR","DISPCODE","SEQNO","X_PSU",
            "X_LLCPWT","X_STSTR","X_AGEG5YR","SEX","X_RACE","EMPLOY1",
            "INCOME2","RENTHOM1","X_EDUCAG","X_URBSTAT","SOMALE","SOFEMALE",
            "BIRTHSEX","TRNSGNDR","MEDCOST","HLTHPLN1","HIVTST6","HIVTSTD3",
            "BRFSS_YEAR")

subset_2021 = subset_2021[,columns]

# Remove the full 2021 data set from memory #
rm(BRFSS_2021)


## Append the BRFSS years ##
BRFSS_merged = rbindlist(list(subset_2017,subset_2018,subset_2019,subset_2020,
                              subset_2021))



## State number to name conversion ##
state_conversion = data.frame(X_STATE = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,
                                          19,20,21,22,23,24,25,26,27,28,29,30,
                                          31,32,33,34,35,36,37,38,39,40,41,42,
                                          44,45,46,47,48,49,50,51,53,54,55,56,
                                          66,72),
                              state_name = c("Alabama","Alaska","Arizona","Arkansas",
                                             "California","Colorado","Connecticut",
                                             "Delaware","District of Columbia",
                                             "Florida","Georgia","Hawaii","Idaho",
                                             "Illinois","Indiana","Iowa","Kansas",
                                             "Kentucky","Louisiana","Maine","Maryland",
                                             "Massachusetts","Michigan","Minnesota",
                                             "Mississippi","Missouri","Montana",
                                             "Nebraska","Nevada","New Hampshire",
                                             "New Jersey","New Mexico","New York",
                                             "North Carolina","North Dakota","Ohio",
                                             "Oklahoma","Oregon","Pennsylvania",
                                             "Rhode Island","South Carolina",
                                             "South Dakota","Tennessee","Texas",
                                             "Utah","Vermont","Virginia","Washington",
                                             "West Virginia","Wisconsin","Wyoming",
                                             "Guam","Puerto Rico"))

add_state_name = function(X) {
  X=merge(X,state_conversion,by="X_STATE")
  return(X)
}

BRFSS_merged = add_state_name(BRFSS_merged)

## Define HIV testing within the BRFSS year variable ##

# Creates a variable for HIV testing within the BRFSS year among individuals who received a test #
  # 1 indicates that the last HIV test date year matches the BRFSS year
  # 0 indicates that the last HIV test date year does not match the BRFSS year
year_test = ifelse(BRFSS_merged$BRFSS_YEAR %>% as.character() == 
                     str_sub(BRFSS_merged$HIVTSTD3,-4),1,0)

# Creates a variable for HIV testing within the BRFSS year among all individuals #
  # 1 indicates that the last HIV test date year matches the BRFSS year
  # 0 indicates that an individual did not receive an HIV test or recieved an HIV test but not within that year
BRFSS_merged = BRFSS_merged %>% mutate(HIV_test = case_when(HIVTST6==1 & year_test == 1 ~ 1,
                                                            HIVTST6==2 | year_test == 0 ~ 0))

## Define a COVID-19 indicator variable ##
BRFSS_merged$COV_YEAR = ifelse(BRFSS_merged$BRFSS_YEAR >2019,1,0)


## Create new weight variables for the merged data set ##

# Reweight responses by state and year #
  # This should make the survey weights accurate even if we are using a subset of states 
  # For example, if we want to exclude Guam and Puerto Rico
  # Or if we are just using the states with the SOGI module

BRFSS_final_merged=NULL
for (i in unique(BRFSS_merged$X_STATE)) {
  state = i
  BRFSS_temp = BRFSS_merged %>% filter(X_STATE==state)
  year_state_proportions = BRFSS_temp %>% group_by(BRFSS_YEAR) %>% 
    summarise(prop = n()/nrow(BRFSS_temp))
  BRFSS_temp2=NULL
  for (j in 2017:2021) {
    BRFSS_state_temp = BRFSS_temp %>% filter(BRFSS_YEAR==j)
    prop = year_state_proportions[j-2016,2] %>% as.numeric()
    BRFSS_state_temp$state_year_LLCPWT = BRFSS_state_temp$X_LLCPWT * prop
    BRFSS_temp2 = rbindlist(list(BRFSS_temp2,BRFSS_state_temp))
  }
  BRFSS_final_merged=rbindlist(list(BRFSS_final_merged,BRFSS_temp2))
}

# Remove temporary variables from memory #
rm(list=c("BRFSS_merged","BRFSS_state_temp","BRFSS_temp","BRFSS_temp2",
          "year_state_proportions"))

## Recode BRFSS missing/don't know to NA ##

# 5 year age group #
  # 14 indicates Don't know/Refused/Missing
BRFSS_final_merged$X_AGEG5YR[BRFSS_final_merged$X_AGEG5YR == 14] <- NA
          
# Sex #
  # Values above 2 indicate Don't know/Refused/Missing
BRFSS_final_merged$SEX[BRFSS_final_merged$SEX >2 ] <- NA

# Race/Ethnicity grouping #
  # 9 indicates Don't know/Refused/Missing
BRFSS_final_merged$X_RACE[BRFSS_final_merged$X_RACE == 9 ] <- NA

# Employment status #
  # 9 indicates Don't know/Refused/Missing
BRFSS_final_merged$EMPLOY1[BRFSS_final_merged$EMPLOY1 == 9 ] <- NA

# Income #
  # Values greater than 8 indicate Don't know/Refused/Missing
BRFSS_final_merged$INCOME2[BRFSS_final_merged$INCOME2 >8 ] <- NA

# Home ownership #
  # Values greater than 8 indicate Don't know/Refused/Missing
BRFSS_final_merged$RENTHOM1[BRFSS_final_merged$RENTHOM1 >3 ] <- NA

# Education #
  # 9 indicates Don't know/Refused/Missing
BRFSS_final_merged$X_EDUCAG[BRFSS_final_merged$X_EDUCAG == 9 ] <- NA

# Urban/Rural status #
  # Missingness already encoded

# Male sexual orientation #
  # Values greater than 4 indicate Don't know/Refused/Missing
BRFSS_final_merged$SOMALE[BRFSS_final_merged$SOMALE >4 ] <- NA

# Female sexual orientation #
  # Values greater than 4 indicate Don't know/Refused/Missing
BRFSS_final_merged$SOFEMALE[BRFSS_final_merged$SOFEMALE >4 ] <- NA

# Birth sex #
  # Values greater than 2 indicate Don't know/Refused/Missing
BRFSS_final_merged$BIRTHSEX[BRFSS_final_merged$BIRTHSEX >2 ] <- NA

# Transgender #
  # Values greater than 4 indicate Don't know/Refused/Missing
BRFSS_final_merged$TRNSGNDR[BRFSS_final_merged$TRNSGNDR >4 ] <- NA

# Medical cost barriers #
  # Values greater than 2 indicate Don't know/Refused/Missing
BRFSS_final_merged$MEDCOST[BRFSS_final_merged$MEDCOST >2 ] <- NA

# Health coverage #
  # Values greater than 2 indicate Don't know/Refused/Missing
BRFSS_final_merged$HLTHPLN1[BRFSS_final_merged$HLTHPLN1 >2 ] <- NA

# Ever HIV test #
  # Values greater than 2 indicate Don't know/Refused/Missing
BRFSS_final_merged$HIVTST6[BRFSS_final_merged$HIVTST6 >2 ] <- NA

# Clean environment #
rm(list=c("state_conversion","columns","i","j","prop","variables","year_test",
          "add_state_name","state"))


## Final notes ##

# The last 5 variables in the data set were added #
  # BRFSS_YEAR is the BRFSS survey year
  # state_name is the name of the U.S. State or territory
  # HIV test indicates whether an individual revived an HIV test within the BRFSS year
  # COV_YEAR indicates whether the survey response occurred during 2020-2021
  # state_year_LLCPWT are combined weights that have been reweighed by state and year

# The BRFSS_final_merged data.frame is the final cleaned/merged/reweighed/recoded data set #
# The subset_XXXX data.frames are the relevant variables directly from each BRFSS year #

## Save the environment ##
save.image("clean_BRFSS.RData", compress = "bzip2")

# To load the environmental variables into an R session you would use load() #
  # Requires ~ 5 Gb of memory
load("clean_BRFSS.RData")

