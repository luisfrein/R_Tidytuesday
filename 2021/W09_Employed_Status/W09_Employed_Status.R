#oad packages
library(tidyverse)
library(extrafont)
library(ggstream)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 9)

employed <- tuesdata$employed

#Explore!
employed %>% 
  group_by(major_occupation, year) %>% 
  summarise(employ = sum(industry_total, na.rm = TRUE)) %>% 
  ggplot(aes(year, employ, fill = major_occupation)) +
  geom_stream()

employed %>% 
  ggplot(aes(year, employ_n, fill = race_gender)) +
  geom_stream()
