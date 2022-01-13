#Load packages
library(tidyverse)
library(extrafont)

#Get the data
tuesdata <- tidytuesdayR::tt_load('2022-01-11')
colony <- tuesdata$colony


#Get to work
colony %>% 
  filter(!is.na(colony_lost_pct),
         !is.na(colony_reno_pct),
         !state == 'United States', 
         year == 2021) %>% 
  mutate(year_result = case_when(colony_reno_pct - colony_lost_pct > 0 ~ 'net_reno',
                                 TRUE ~ 'net_lost')) %>% 
  pivot_longer(2:3, names_to = 'mean_lost_added', values_to = 'values') -> colony_means

colony_means %>% 
  ggplot(aes(values, fct_reorder(state, values), color = mean_lost_added)) +
  geom_line(aes(group = state), size = 1, color = '#525252') +
  geom_point(size = 4, show.legend = FALSE) +
  scale_color_manual(values = c('black', 'black'))
