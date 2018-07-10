#' ---------
#' Title: Pew Political Typology Data
#' Subtitle: Looking at TN survey data, as a whole
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)
library(foreign)

pew_data <- read.spss("Typology 17 Public.sav", to.data.frame = TRUE)
head(pew_data)

#'###### -------------**Modeling**---------------------- ######

pew_data %>% 
  select(racecmb, age, income, party, ideo, state, marital) %>% 
  filter(state == "Tennessee") %>% 
  group_by(state)
  


# ideo:
#   liberal
#   moderate
#   conservative
#   very conservative
#   very liberal