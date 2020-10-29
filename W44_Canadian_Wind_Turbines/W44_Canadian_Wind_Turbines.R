#Load packages
library(tidyverse)
library(gt)

#Get the data
wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

#Create the table
#First find out the top 10 projects by capacity
wind_table <- wind_turbine %>% 
    group_by(province_territory, project_name, total_project_capacity_mw, manufacturer, commissioning_date) %>%
    summarise(n_distinct(project_name)) %>% arrange(desc(total_project_capacity_mw)) %>% 
    select(-6) %>% 
    head(10) %>%
    #Rename Columns
    rename('Province/Territory' = province_territory, Project = project_name, 'Project Capacity (Megawatts)' = total_project_capacity_mw, Manufacturer = manufacturer, 'Commissioning Date' = commissioning_date) %>%
    ungroup() %>% 
    #Change column order
    select(5, 1, 2, 4, 3) %>% 
    gt() %>% 
    tab_header(title = md('**Top 10 Canadian Wind Turbine Projects by Capacity**'), 
               subtitle = md('**The future is green**')) %>% 
    tab_source_note(md('*Table: @luisfreii | Data: by Government of Canada*'))
  

#Add style to the table
wind_table <- wind_table %>% 
  opt_all_caps() %>% 
  tab_options(
    heading.subtitle.font.size = 18,
    heading.align = "center",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(3)
    ) %>% 
  cols_width(1:3 ~ px(150), 4 ~ px(110), 5 ~ px(125)) %>% 
  data_color(
    columns = vars(`Project Capacity (Megawatts)`),
    colors = scales::col_numeric(
      palette = c('white', '#298738'),
      domain = NULL)
    ) %>% 
  tab_style(
    style = cell_text(color = '#01520e'),
    locations = cells_title('title')
  ) %>% 
  tab_style(
    style = cell_text(color = '#01520e'),
    locations = cells_title('subtitle')
  )

wind_table  


