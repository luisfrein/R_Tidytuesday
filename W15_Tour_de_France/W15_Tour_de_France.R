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
  select(3, 4, 19, 2, 1, 8, 5:6) %>% 
  gt() 
  

tdf_table %>%
  cols_merge(
    columns = vars(winner_name, winner_team)
) %>% 
  text_transform(
    locations = cells_body(
      columns = vars(winner_name)
    ),
    fn = function(x){
      name <- word(x, 1, 2)
      team <- word(x, 3, -1)
      glue::glue(
        "<div><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{name}</div>
        <div><span style ='font-weight:bold;color:grey;font-size:10px'>{team}</span></div>"
      )
      }
  )

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
