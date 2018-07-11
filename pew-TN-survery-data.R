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
library(fastDummies)

pew_data <- read.spss("Typology 17 Public.sav", to.data.frame = TRUE)
head(pew_data)

#'###### -------------**First Modeling**---------------------- ######

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

#'###### -------------**TN formatting**---------------------- ######

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

#'###### -------------**GLM modeling on TN set**-------------- ######

# GLM model on TN Data. It seems that age has nothing to do with party
# affiliation, but ideo has a small significance
glm(party ~ ideo , TN_filtered, family = "quasibinomial") %>%  
  summary() 

# Using fastDummies package to code dummy variables on categorical objects
TN_filtered$ideo <- dummy_cols(TN_filtered$ideo, select_columns = ideo)


#'###### -------------**Entire set formatting**---------------------- ######

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
  scale_fill_manual(values = c("#E0162B", "#0052A5", "gray")) +
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
  guides(fill = guide_legend(title = "Party")) +
  scale_y_continuous(breaks = seq(0, 450, by = 25))


# ggsave for affiliation by state
ggsave(full_filtered, file = "/Users/robertpapel/Documents/Personal_R_Stuff/election-stuff/Plots (ggsave)/pew_party_affil_vis_July10.png", 
       device = "png",
       width = 10,
       height = 7)

#'###### -------------**GLM modeling entire set**------------- ######

# Modeling on full set, ideology is extremely significant
base <- pew_data %>% 
  select(racecmb, age, income, party, ideo, state) %>%
  filter(income != "VOL. DO NOT READ Don't know/Refused",
         income != "VL. D NT EAD Don't know/efused",
         ideo != "Don't know/Refused (VOL.)",
         party != "Don't know/Refused (VOL.)",
         party != "No preference (VOL.)",
         party != "Other party (VOL.)",
         racecmb != "Don't know/Refused (VOL.)",
         age != "Don't know/Refused (VOL.)") %>% 
  arrange(state)

# shows the important factors on who votes for what party
glm(party ~ ideo + racecmb + income, base, family = "quasibinomial") %>%  
  summary() # shows the important factors on who votes for what party

# More Independents describe themselves as moderate (duh)
ggplot(base) +
  geom_bar(aes(ideo, fill = party)) +
  scale_fill_manual(values = c("#E0162B", "#0052A5", "gray")) +
  coord_flip()

# Plotting the GLM with a predict funtion (can't get it to work)
base_mod <- glm(party ~ ideo + racecmb + income, family = "quasibinomial", base)
base_pred <- predict(base_mod, type = "response", se.fit = T)
base$pred <- base_pred$fit
base$se   <- base_pred$se.fit

# Plot of age and party affiliation across the US
age_party <- ggplot(base, aes(age, fill = party)) +
  geom_bar(size = 3, position = "fill") +
  scale_fill_manual(values = c("#E0162B", "#0052A5", "gray")) +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Age & Party",
    subtitle = "Age Proportion, by Party Affiliation",
    x = "Age",
    y = "Proportion",
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
  
# ggsave for affiliation by age
ggsave(age_party, file = "/Users/robertpapel/Documents/Personal_R_Stuff/election-stuff/Plots (ggsave)/age_and_party_July11.png", 
       device = "png",
       width = 10,
       height = 7)

















#'###### -------------**Detritus**------------- ######

  # geom_point(aes(party, ideo, color = party), size = 3) +
  # geom_errorbar(aes(ymin = base_pred - se, ymax = base_pred + se, 
  #                   color = income), width = 1.5) +
  # geom_abline(intercept = 0, slope = 1, color = "blue", linetype = 2) +
  # labs(x = "Actual", y = "Predicted")
