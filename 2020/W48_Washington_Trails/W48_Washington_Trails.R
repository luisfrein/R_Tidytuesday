#Load packages
library(tidyverse)
library(extrafont)
library(ggridges)
library(Cairo)
#Get the data
# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load(2020, week = 48)

hike_data <- tuesdata$hike_data

#Explore
hike_data <- hike_data %>% 
  mutate(rating = as.numeric(rating))

#Get location name before the first "-".
hike_data_mod <- hike_data %>% 
    mutate(location = str_extract(location, "[^-]+")) 

#Remove whitespace
hike_data_mod$location <- str_trim(hike_data_mod$location)

#Plot  
hike_data_mod %>% 
  filter(rating > 0) %>% 
  ggplot(aes(rating, location, fill = location)) +
  geom_density_ridges(alpha = 0.9, show.legend = FALSE) +
  scale_fill_cyclical(values = c("#db3a34", "#ffc857")) +
  scale_x_continuous(n.breaks = 5) +
  coord_cartesian(clip = "off") +
  labs(x = "Rating", y = NULL, 
       title = "Distribution of Rating Scores by Hike Location", 
       subtitle = "All locations have a similar rating distribution,\nbut some areas like Issaquah Alps and Central\nWashington don't have many ratings above 4.", 
       caption = "Made by @luisfreii | Data: TidyX") +
  theme(plot.background = element_rect("#141414"),
        panel.background = element_blank(),
        plot.title = element_text(color = "#FFF1D6", face = "bold", family = "DejaVu Sans Mono", size = 16),
        plot.subtitle = element_text(color = "#FFF1D6", face = "bold.italic", family = "DejaVu Sans Mono", size = 14),
        plot.caption = element_text(color = "#FFF1D6", face = "bold", family = "DejaVu Sans Mono", hjust = 0.5, size = 10),
        axis.text = element_text(color = "#FFF1D6", face = "bold", family = "DejaVu Sans Mono", size = 11),
        axis.title = element_text(color = "#FFF1D6", face = "bold", family = "DejaVu Sans Mono"),
        panel.grid.major = element_line(color = "#292929"),
        panel.grid.minor = element_blank()
        )
  
# ggsave("washington_ridge_plot.png", 
#        height = 24.94, 
#        width = 21.6, 
#        units = "cm", 
#        dpi = 500,
#        type = "cairo-png")                
