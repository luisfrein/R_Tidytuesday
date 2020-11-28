#Load packages
library(tidyverse)
library(gt)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 15)

tdf_winners <- tuesdata$tdf_winners

#Expore!
tdf_table <- tdf_winners %>% 
  filter(!is.na(time_overall)) %>% 
  mutate(start_date = lubridate::year(start_date)) %>% 
  slice_tail(n = 15) %>% 
  select(3, 19, 2, 1, 8, 5:6) %>% 
  gt()

tdf_table %>% 
  tab_header(
    title = md("**Tour de France Winners of the Past 15 Years**"),
    subtitle = html("<em>The British and Spaniards have dominated these last years.</em>")
    ) %>% 
  fmt_number(
    columns = 6:7,
    decimals = 2
  ) %>% 
  tab_source_note("Made by @luisfreii | Data: Alastair Rushworth's") %>% 
  tab_options(
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(3)
  )
