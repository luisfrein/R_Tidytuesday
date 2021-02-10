#Load packages
library(tidyverse)
library(extrafont)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 7)

lifetime_earn <- tuesdata$lifetime_earn

income_dist <- tuesdata$income_distribution
#Explore
lifetime_earn %>% 
  ggplot(aes(lifetime_earn, race, fill = gender)) +
  geom_col(position = "dodge")

income_dist %>% 
  group_by(year, race) %>% 
  summarise(sum(income_distribution)) %>% view()

income_dist %>% 
  # #filter(!race %in% c("Asian Alone or in Combination",
  #                     "Black Alone or in Combination",
  #                     "White Alone, Not Hispanic")) %>% 
  ggplot(aes(year, income_mean, color = race)) +
  geom_point()

str_extract(income_dist$race, "Combination$")
