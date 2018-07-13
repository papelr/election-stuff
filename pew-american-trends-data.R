#' ---------
#' Title: Pew American Trends Data
#' Subtitle: Looking at survey data, as a whole
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)
library(foreign)
library(stringr)
library(broom)

trends_data <- read.spss("ATP W26.sav", to.data.frame = TRUE)
dim(trends_data)

#'###### -------------**Formatting**-------------- ######

# Picking specific variables
trends_data$F_INCOME_RECODE_FINAL
trends_data$F_PARTY_FINAL
trends_data$F_IDEO_FINAL
trends_data$F_EDUCCAT2_FINAL
trends_data$F_SEX_FINAL
trends_data$F_AGECAT_FINAL
trends_data$F_CREGION_FINAL
trends_data$GUNTHREAT_W26 # has anyone ever threatened your family w/ gun
trends_data$GROWUPGUN1_W26 # guns in household? yes, no, not sure
trends_data$GROWUPVIOL_W26 # gun violence problem in neighborhood
trends_data$GROWUPCOM_W26 # community had guns?
trends_data$SOCTRUST_W26 # can people be trusted with guns?
trends_data$KNOWGUN_W26 # ok for others to know I have guns?
trends_data$GUNIDENTITY_W26 # how important owning a gun is to person
trends_data$SAFECRIME_W26 # safety of local community?
trends_data$SATISF_W26 # satisfied/diss w/ country today?
trends_data$SATISFY_W26 # satisfied/diss w/ local community?

# Renaming variables to something other than Pew gobblydegook
selected_trends <- trends_data %>% 
  select(
    income = F_INCOME_RECODE_FINAL,
    party = F_PARTY_FINAL, 
    ideology = F_IDEO_FINAL, 
    edu_level = F_EDUCCAT2_FINAL, 
    sex = F_SEX_FINAL, 
    age = F_AGECAT_FINAL, 
    region = F_CREGION_FINAL, 
    family_threatened = GUNTHREAT_W26, 
    gun_in_child_home = GROWUPGUN1_W26, 
    gun_violence_child_comm = GROWUPVIOL_W26, 
    gun_child_comm = GROWUPCOM_W26, 
    social_trust = SOCTRUST_W26,
    importantance_guns = GUNIDENTITY_W26, 
    local_safe_comm = SAFECRIME_W26, 
    satisfy_US = SATISF_W26, 
    satisfy_local_comm = SATISFY_W26) 
 
