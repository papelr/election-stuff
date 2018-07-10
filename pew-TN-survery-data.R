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
pew_data %>% 
  select(racecmb, age, income, party, ideo, state, marital) %>%
  filter(state == "Tennessee",
         income != "[VOL. DO NOT READ] Don't know/Refused",
         income != "[VL. D NT EAD] Don't know/efused") %>%
  group_by(income) %>% 
  ggplot() +
  geom_bar(aes(income))

# GLM model on TN data segment
pew_data$income <- gsub("\\[|\\]", "", pew_data$income) 
pew_data$income <- gsub("[OR]", "", pew_data$income)

TN_filtered <- pew_data %>% 
  select(racecmb, age, income, party, ideo, state) %>%
  filter(state == "Tennessee",
         income != "VOL. DO NOT READ Don't know/Refused",
         income != "VL. D NT EAD Don't know/efused",
         ideo != "Don't know/Refused (VOL.)",
         party != "Don't know/Refused (VOL.)",
         party != "No preference (VOL.)") %>% 
  group_by(state) 

# GLM model on TN Data. It seems that age has nothing to do with party
# affiliation, but ideo has a small significance
glm(party ~ ideo , TN_filtered, family = "quasibinomial") %>%  
  summary() 

# GLM model formatting for entire set
pew_data$income <- gsub("\\[|\\]", "", pew_data$income) 
pew_data$income <- gsub("[OR]", "", pew_data$income)

# Plot of party affiliation by state, counted
full_filtered <- pew_data %>% 
  select(racecmb, age, income, party, ideo, state) %>%
  filter(income != "VOL. DO NOT READ Don't know/Refused",
         income != "VL. D NT EAD Don't know/efused",
         ideo != "Don't know/Refused (VOL.)",
         party != "Don't know/Refused (VOL.)",
         party != "No preference (VOL.)",
         party != "Other party (VOL.)",
         racecmb != "Don't know/Refused (VOL.)",
         age != "Don't know/Refused (VOL.)") %>% 
  arrange(state) %>% 
  # gather(key = state, value = party, -state) %>% 
  ggplot() +
  geom_bar(aes(state, fill = party)) +
  coord_flip() +
  theme_bw() +
  labs(
    title = "",
    subtitle = "",
    x = "States",
    y = "Survey Count",
    caption = "Data from www.people-press.org, Plot by R. Papel") +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(face = "italic"),
    plot.subtitle = element_text(face = "italic"),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(color = "grey70"), 
    axis.title.y = element_text(size = 11, face = "bold", color = "black"),
    axis.title.x = element_text(size = 11, face = "bold", color = "black")
  ) +
  guides(fill = guide_legend(title = "Party"))
  