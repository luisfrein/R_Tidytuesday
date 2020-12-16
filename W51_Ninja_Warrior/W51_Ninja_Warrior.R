#Load packages
library(tidyverse)
library(extrafont)
library(Cairo)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 51)

ninja_warrior <- tuesdata$ninja_warrior

#Explore
#Get the top 20 obstacles and plot it
plot <- ninja_warrior %>% 
  filter(!obstacle_name == "Warped Wall / Mega Wall") %>% #Filter out the Warped Wall / Mega Wall obstacle, to avoid repetition with the Warped wall.
  count(obstacle_name, sort = TRUE) %>% 
  head(20) %>% 
  ggplot(aes(obstacle_name, n)) +
  geom_segment(aes(x = obstacle_name, xend = obstacle_name, y = 0, yend = n), alpha = .5, color = "#935061", size = 1.15) +
  geom_point(aes(size = n), color = "#792F40") +
  coord_polar()
  
#Customize the plot
plot +
  labs(x = NULL, y = NULL, 
       title = "The 20 Most Common Ninja Warrior Obstacles",
       subtitle = "The data is from the first 10 seasons of the show.\nThe size of the dot represents how common the\nobstacle is.\n",
       caption = "Made by @luisfreii | Data: Data.World") +
  theme(plot.background = element_rect("#D6D6D6"),
        panel.background = element_rect("#D6D6D6"),
        axis.text = element_text(color = "#792F40", face = "bold", family = "Arial Narrow", size = 10),
        panel.grid.major =  element_line(color = "white"),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.5, 1),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "#D6D6D6", fill = "#D6D6D6"),
        legend.key = element_rect("#D6D6D6"),
        legend.title = element_blank(),
        plot.title = element_text(color = "#792F40", face = "bold", family = "Arial Narrow", size = 14),
        plot.subtitle = element_text(color = "#792F40", face = "bold", family = "Arial Narrow", size = 12),
        legend.text = element_text(color = "#792F40", face = "bold", family = "Arial Narrow", size = 11),
        plot.caption = element_text(color = "#792F40", face = "bold", family = "Arial Narrow", size = 11, hjust = 0.5)
        )

#Code to save the plot
# ggsave("ninja_obstacles.png",
#        height = 23.2,
#        width = 20.5,
#        units = "cm",
#        dpi = 500,
#        type = "cairo-png")
