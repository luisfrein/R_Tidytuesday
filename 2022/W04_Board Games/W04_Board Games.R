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
  select(3,10, 21, 4, 17, 6) %>% 
  mutate(boardgamecategory = str_remove_all(boardgamecategory, '[[:punct:]]')) %>% 
  arrange(-playingtime) %>% 
  slice_head(n = 10) %>% 
  gt() %>% 
  cols_label(name = "Game",
             thumbnail = "Thumbnail",
             boardgamecategory = "Category",
             year = "Release Year",
             playingtime = "Playtime",
             average = "Average Rating") %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
    )
  ), 
  locations = list(
    cells_body(
      columns = vars(playingtime)
    )
  ) 
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  )

tablgtsave(table, 'table.png')

#Color playtime and rating
paletteer::paletteer_d("ggsci::red_material", n = 6) %>%
  as.character() %>%
  scales::show_col()
