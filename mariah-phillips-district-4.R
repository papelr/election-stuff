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

District_4 <- tibble("Year" = c(2010, 2012, 2014, 2016),
                     "Republican Votes" = c(103969, 128568, 84815, 165796),
                     "Democrat Votes" = c(70254, 102222, 51357, 89141),
                     "Independent Votes" = c(7968, 0, 9246, 0))

# Visualization not standardized on a percent scale
votes_numbers <- District_4 %>%
  mutate(Year = as.Date(paste0(Year, "/01/01"))) %>%
  gather(key, VoteCount, -Year) %>%
  ggplot(aes(Year, VoteCount, fill = key)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#0052A5", "gray", "#E0162B")) +
  # coord_flip() +
  theme_bw() +
  labs(
    title = "TN District 4 Votes by Party",
    subtitle = "Voting numbers, last 4 cycles",
    x = "Year",
    y = "Votes",
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
  ) +
  guides(fill = guide_legend(title = "Legend")) +
  scale_y_continuous(labels = scales::comma)

# Graph with position = "fill"
votes_standardized <- District_4 %>%
  mutate(Year = as.Date(paste0(Year, "/01/01"))) %>%
  gather(key, VoteCount, -Year) %>%
  ggplot(aes(Year, VoteCount, fill = key)) +
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values = c("#0052A5", "gray", "#E0162B")) +
  # coord_flip() +
  theme_bw() +
  labs(
    title = "TN District 4 Votes by Party",
    subtitle = "Voting numbers standardized, last 4 cycles",
    x = "Year",
    y = "Vote Proportion",
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
  ) +
  guides(fill = guide_legend(title = "Legend")) 


#'###### ---------------**ggsave**-------------------- ######

ggsave(votes_numbers, file = "/Users/robertpapel/Documents/Personal_R_Stuff/election-stuff/Plots (ggsave)/votes_numbers_July9.png", 
       device = "png",
       width = 10,
       height = 7)

ggsave(votes_standardized, file = "/Users/robertpapel/Documents/Personal_R_Stuff/election-stuff/Plots (ggsave)/votes_standardized_July9.png", 
       device = "png",
       width = 10,
       height = 7)
