setwd("C:/Users/Austin Hammermeister/Desktop/PhD/Year 1 Coursework/SPR_2023/EPI 514/Group Project/")

library(data.table)
library(survey)
library(tidyverse)
library(gtsummary)
library(webshot2)

load("clean_BRFSS.RData")

# Subset to a random set of 10000 rows to test creating the table
test_rows = sample(1:nrow(BRFSS_merged),10000,replace = FALSE)
test= BRFSS_merged[test_rows,]

# HIV test within the BRFSS year #

test$HIV_test = test$HIV_test %>% factor(levels = c(0,1),
                                         labels = c("No HIV test within the BRFSS survey year", "HIV test within the BRFSS survey year"))

# COVID-19 Year #

test$COV_YEAR = test$COV_YEAR %>% factor(levels = c(0,1),
                                            labels = c("2017-2019","2020-2021"))

# Healthcare access #


test$HLTHPLN1 = test$HLTHPLN1 %>% factor(levels = c(1,2),
                               labels = c("Coverage","No coverage"))

# Financial barriers to care #

test$MEDCOST = test$MEDCOST %>% factor(levels = c(1,2),
                                         labels = c("Barriers","No barriers"))

# Sex #

test$SEX = test$SEX %>% factor(levels = c(1,2),
                                         labels = c("Male","Female"))

# Race/ethnicity #

test$X_RACE = test$X_RACE %>% factor(levels = c(1,2,3,4,5,6,7,8),
                               labels = c("White, non-Hispanic",
                                          "Black, non-Hispanic",
                                          "American Indian or Alaskan Native, non-Hispanic",
                                          "Asian, non-Hispanic",
                                          "Native Hawaiian or other Pacific Islander, non-Hispanic",
                                          "Other race, non-Hispanic",
                                          "Multiracial, non-Hispanic",
                                          "Hispanic"))

# Urban/rural status #
test$X_URBSTAT = test$X_URBSTAT %>% factor(levels = c(1,2),
                                       labels = c("Urban counties","Rural counties"))

# Employment #

test$EMPLOY1 = test$EMPLOY1 %>% factor(levels = c(1,2,3,4,5,6,7,8),
                                           labels = c("Employed for wages",
                                                      "Self-employed",
                                                      "Out of work (>=1 year)",
                                                      "Out of work (<1 year)",
                                                      "A homemaker",
                                                      "A student",
                                                      "Retired",
                                                      "Unable to work"))
# Annual household income #

test$INCOME2 = test$INCOME2 %>% factor(levels = c(1,2,3,4,5,6,7,8),
                                       labels = c("<$10K",
                                                  "$10-15K",
                                                  "$15-20K",
                                                  "$20-25K",
                                                  "$25-35K",
                                                  "$35-50K",
                                                  "$50-75K",
                                                  ">$75K"))

# Home ownership #
test$RENTHOM1 = test$RENTHOM1 %>% factor(levels = c(1,2,3),
                                           labels = c("Own","Rent",
                                                      "Other"))

# Level of education #
test$X_EDUCAG = test$X_EDUCAG %>% factor(levels = c(1,2,3,4),
                                         labels = c("Less than high school","High school",
                                                    "Some college or technical school",
                                                    "College or technical school"))

# Sexual orientation #

# Create a single sexual orientation variable 
test = test %>% mutate(SO = case_when(SOMALE==1 | SOFEMALE==1 ~ 1,
                                      SOMALE==2 | SOFEMALE==2 ~ 2,
                                      SOMALE==3 | SOFEMALE==3 ~ 3,
                                      SOMALE==4 | SOFEMALE==4 ~ 4))

test$SO = test$SO %>% factor(levels = c(1,2,3,4),
                                         labels = c("Gay or Lesbian",
                                                    "Straight",
                                                    "Bisexual",
                                                    "Something else"))

# Transgender status #

test$TRNSGNDR = test$TRNSGNDR %>% factor(levels = c(1,2,3,4),
                             labels = c("Transgender, male-to-female",
                                        "Transgender, female-to-male",
                                        "Transgender, gender nonconforming",
                                        "No"))


# Create the survey design object
design <- svydesign(data = test,
                    id = ~X_PSU, strata = ~X_STSTR, weights = ~LLCPWT_5Y, nest = TRUE)
options(survey.lonely.psu = "adjust")

# Create a table_1
table_1 <-
  design %>%
  tbl_svysummary(
    by = COV_YEAR,
    include = c(HIV_test,HLTHPLN1,MEDCOST,SEX,X_RACE,X_URBSTAT,
                EMPLOY1,INCOME2,RENTHOM1,X_EDUCAG,SO,TRNSGNDR),
    label=list(HIV_test~ "HIV test within the BRFSS survey year", 
               HLTHPLN1 ~ "Healthcare coverage",
               MEDCOST ~ "Financial barriers to care",SEX ~ "Sex",
               X_RACE ~ "Race/Ethnicity", X_URBSTAT ~ "Urban/rural status",
               EMPLOY1 ~ "Employment status",
               INCOME2 ~ "Annual household income", RENTHOM1 ~ "Home ownership",
               X_EDUCAG ~ "Level of education", SO ~ "Sexual orientation",
               TRNSGNDR ~ "Transgender status"),
    statistic = list(all_categorical() ~ "{n} ({p}%) [{n_unweighted} ({p_unweighted}]"),
    missing="ifany",
    missing_text="Missing/Don't know/Refused" 
  ) %>%
  modify_caption("<b>Table 1: BRFSS respondant characteristics prior to (2017-2019) and during the COVID-19 pandemic (2020-2021).</b>") %>% 
  bold_labels() %>%
  add_n() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Rural/urban status was not collected in 2017; Sexual orientation and transgender questions are only asked by a subset of states each year*"))

gt::gtsave(table_1, file = "example_table1.png")

