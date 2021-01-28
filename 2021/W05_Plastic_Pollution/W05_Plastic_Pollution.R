#Load packages
library(tidyverse)
library(extrafont)
library(treemapify)
library(glue)
library(ggtext)
library(Cairo)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)

plastics <- tuesdata$plastics

#Explore
#Filter for the Philippines and the year 2020. If a company has under 300 counts of plastic polution assign "104 Other Companies" as its name.
plastics_philippines <- 
  plastics %>% 
  filter(year == 2020, !parent_company %in% c("Unbranded", "NULL", "null"), country == "Philippines") %>%    
  mutate(parent_company = case_when(grand_total < 300 ~ "104 Other Companies",
                                    TRUE ~ parent_company)) %>% 
  add_count(parent_company) 

#Total plastic objects in The Philippines
plastics_philippines %>% 
  summarise(total = sum(grand_total)) -> philippines_total

#Volunteers in The Philippines
philippines_volunteers <- plastics_philippines[1, 14]

#Companies in The Philippines
philippines_companies <- nrow(plastics_philippines)

#Plot colors  
bigger <- "#29274C"
middle <- "#7E52A0"
small <- "#BC629F"
other <- "#999999"
text <- "#0E0E1B"
backround <- "#F5F5F5"

#Plot  
plastics_philippines %>%  
    group_by(parent_company) %>%  
    summarise(grand_total = sum(grand_total)) %>% 
    mutate(parent_company = paste0(parent_company, "\n(", grand_total, ")"),
           group = case_when(grand_total > 5000 ~ glue("{bigger}"),
                             grand_total < 1050 ~ glue("{small}"),
                             grand_total == 4516 ~ glue("{other}"),
                             TRUE ~ glue("{middle}"))) %>% 
    ggplot(aes(area = grand_total, label = parent_company, fill = group)) +
    geom_treemap() +
    geom_treemap_text(place = "topleft", color = "white", reflow = TRUE, family = "Verdana") +
    scale_fill_identity() +
    labs(title = "**Philippines Worst Polluters**<br>",
         subtitle = glue("The **#breakfreefromplastic** Movement is a global movement, with more than 11,000 organizations<br>and individual supporters from across the world. The goal of the movement is the reduction of<br>single-use plastics and to find solutions to the plastic pollution crisis.<br><br>In 2020, {philippines_volunteers} volunteers collected **{philippines_total}** pieces of plastic from {philippines_companies} companies in The Philippines.<br>"),
         caption = "<br>Visualization: @luisfreii | Data source: #breakfreefromplastic") +
    theme(plot.background = element_rect(color = backround),
          plot.margin = unit(c(2, 2, 2, 2), "lines"),
          plot.title = element_markdown(hjust = .5, family = "Verdana", size = 14, color = text),
          plot.subtitle = element_markdown(hjust = .5, family = "Verdana", color = text),
          plot.caption = element_markdown(hjust = .5, family = "Verdana", color = text)
          )

#Save the plot
# ggsave("Philippines_worst.png",
#        height = 16,
#        width = 21,
#        units = "cm",
#        dpi = 320,
#        type = "cairo-png")
  

  
 
  
  
  
