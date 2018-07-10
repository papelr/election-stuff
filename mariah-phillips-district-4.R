#' ---------
#' Title: TN District 4
#' Subtitle: Can Mariah Phillips Win?
#' Date: July 2018
#' Author: Robert Papel
#' ---------

#'###### -------------**Libraries**---------------------- ######

library(tidyverse)

#'###### -------------**Loading Past Election Data**---------- ######

# Compile 3 election years of party voting in TN District 4:
# Combine 3 election years of data into a tibble, from:
# http://history.house.gov/Institution/Election-Statistics/Election-Statistics/

District_4 <- tibble("Year" = c(2012, 2014, 2016),
                     "Republican Votes" = c(128568, 84815, 165796),
                     "Democrat Votes" = c(102222, 51357, 89141),
                     "Independent Votes" = c(0, 9246, 0))

# Quick visualization
party_colors <- c("E0162B", "0052A5")

District_4 %>%
  mutate(Year = as.Date(paste0(Year, "/01/01"))) %>%
  gather(key, VoteCount, -Year) %>%
  ggplot(aes(Year, VoteCount, fill = key)) +
  geom_bar(stat = "identity") + 
  theme_bw() +
  labs(
    title = "TN District 4 Votes by Party",
    subtitle = "Viability of Mariah Phillips",
    x = "Year",
    y = "",
    caption = "Data from Clerk of the House, Plot by R. Papel") +
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
  ) 

