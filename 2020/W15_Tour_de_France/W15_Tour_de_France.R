#Load packages
library(tidyverse)
library(gt)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 15)

tdf_winners <- tuesdata$tdf_winners

#Expore!
#Create table
tdf_table <- tdf_winners %>% 
  filter(!is.na(time_overall)) %>% 
  #Filter out NAs.
  mutate(start_date = lubridate::year(start_date), nationality = str_trim(nationality),
         nationality = recode_factor(nationality,'United States' = 'United%20States'),
         nationality = recode_factor(nationality,'Great Britain' = 'Great%20Britain')
         ) %>% 
  #Change factors names, to get the correct url in the next mutate.
  slice_tail(n = 20) %>% 
  mutate(img = paste0("https://github.com/Zetluis/R_Tidytuesday/blob/master/W15_Tour_de_France/Flag%20icons/",nationality, ".png?raw=true")) %>%
  #Pastes the url for each nationality.
  select(3, 4, 20, 2, 1, 5:6) %>% 
  gt() 
  
#Download images for the table, use HTML for the winner column and change columns names.
tdf_table <- tdf_table %>%
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
        "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{name}</div>
        <div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>{team}</span></div>"
      )
      }
  ) %>% 
  text_transform(
    locations = cells_body(vars(img)),
    fn = function(x){
      web_image(url = x, height = 20)
    }) %>% 
  cols_label(
    winner_name = md("**Winner**"),
    img = "",
    start_date = md("**Year**"),
    edition = md("**Edition**"),
    distance = md("**Distance (KM)**"),
    time_overall = md("**Overall Hours**")
  )

#Add style to the table
tdf_table %>% 
  tab_header(
    title = md("**Tour de France Winners 2000-2019**"),
    ) %>% 
  fmt_number(
    columns = 6:7,
    decimals = 0
  ) %>% 
  tab_source_note("Made by @luisfreii | Data: Alastair Rushworth's") %>% 
  tab_style(
    style = list(
      cell_text(font = "Arial Rounded MT Bold", weight = "normal")),
    locations = 
      cells_column_labels(columns = gt::everything()
      )
    ) %>% 
  tab_options(
    table.border.top.color = "#080D0A",
    column_labels.border.top.color = "#080D0A",
    column_labels.border.bottom.color = "#080D0A",
    column_labels.border.bottom.width = px(3),
    column_labels.background.color = "#080D0A",
    table_body.hlines.color = "#ADADAD"
  ) 
