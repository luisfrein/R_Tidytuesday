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
  select(3, 4, 21, 10, 17, 6) %>% 
  #Remove punctuation from category column except for ',' and convert playingtime from minutes to days
  mutate(boardgamecategory = str_remove_all(boardgamecategory, "(?!,)[[:punct:]]"),
         playingtime = round(playingtime / 1440, digits = 2)) %>% 
  arrange(-playingtime) %>% 
  slice_head(n = 15) %>% 
  gt() %>% 
  gtExtras::gt_theme_538() %>% 
  gtExtras::gt_img_rows(thumbnail, height = 60) %>% 
  cols_label(name = "Game",
             thumbnail = "",
             boardgamecategory = "Category",
             year = "Release Year",
             playingtime = "Playtime (In Days)",
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
      columns = c(playingtime)
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
  ) %>% 
  tab_header(title = md('**Can You and Your Friends Complete These Board Games?**'),
             subtitle = '14 out of the 15 board games that take the longest to complete have war in their categories.') %>% 
  tab_source_note(source_note = md('Source: **Kaggle by way of Board Games Geek**, Table: **@luisfreii**')) %>% 
  data_color(
    columns = c(average),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d('nord::lumina', n = 5)),
      domain = NULL
    )
  ) %>% 
  data_color(
    columns = c(playingtime),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d('nord::lumina')),
      domain = NULL
    )
  ) -> longest_games

#Code to save the table
gtsave(longest_games, 'W04_Board Games.png')


