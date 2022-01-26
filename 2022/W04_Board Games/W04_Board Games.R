#Load packages
library(tidyverse)
library(showtext)
library(gt)

#Get the data
tuesdata <- tidytuesdayR::tt_load('2022-01-25')
ratings <- tuesdata$ratings
details <- tuesdata$details

#Get to work
#Left join both data frames
left_join(ratings, details, by = c('name' = 'primary')) -> games

games %>% 
  select(3, 4, 6, 10, 17, 21) %>% 
  mutate(boardgamecategory = str_remove_all(boardgamecategory, '[[:punct:]]')) %>% 
  arrange(-playingtime) %>% 
  slice_head(n = 10) %>% 
  gt() -> table

gtsave(table, 'table.png')
