#' ---------
#' Title: Pew Political Typology Data
#' Subtitle: Looking at TN survey data, as a whole
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)
library(foreign)
library(stringr)

pew_data <- read.spss("Typology 17 Public.sav", to.data.frame = TRUE)
head(pew_data)

#'###### -------------**Modeling**---------------------- ######

# Race filter
TN_race <- pew_data %>% 
  select(racecmb, age, income, party, ideo, state, marital) %>% 
  filter(state == "Tennessee") %>% 
  group_by(state) %>% 
  ggplot() +
  geom_bar(aes(racecmb))

# Ideology filter
pew_data %>% 
  select(racecmb, age, income, party, ideo, state, marital) %>%
  filter(state == "Tennessee") %>% 
  group_by(state) %>% 
  ggplot() +
  geom_bar(aes(ideo))

# Income filter
pew_data$income <- gsub('[[OR]]', '', pew_data$income) 
pew_data %>% 
  select(racecmb, age, income, party, ideo, state, marital) %>%
  filter(state == "Tennessee",
         income != "[VOL. DO NOT READ] Don't know/Refused",
         income != "[VL. D NT EAD] Don't know/efused") %>%
  group_by(income) %>% 
  ggplot() +
  geom_bar(aes(income))
