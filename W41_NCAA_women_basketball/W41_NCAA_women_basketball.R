#Load the package
library(tidyverse)
library(ggthemes)
library(magick)
#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 41)

tournament <- tuesdata$tournament

#Filter data
Auburn <- tournament %>% 
  filter(school == "Auburn")

#Read Auburn Logo
Auburn_logo <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Auburn_Tigers_logo.svg/679px-Auburn_Tigers_logo.svg.png")

#Assign a number for each position in tourney_finish in order to plot.
Auburn_plot <- Auburn %>% 
  mutate(Rank = case_when(tourney_finish == "1st" ~ 1, tourney_finish == "2nd" ~ 2, tourney_finish == "RSF" ~ 3, tourney_finish == "RF" ~ 4, tourney_finish == "NSF" ~ 5, tourney_finish == "N2nd" ~ 6, tourney_finish == "Champ" ~ 7)) %>% 
  ggplot(aes(year, Rank)) +
  geom_line(color = "#012B5D", size = 1.5) +
  geom_point(aes(color = tourney_finish), size = 5) 

#Plot
Auburn_plot + labs(title = "Auburn's Female Basketball Team National Tournament Position Throughout the Years", x = NULL, y = "\nTournament Position\n", color = "Position", caption = "@luisfreii") +
  scale_x_continuous(n.breaks = 20) + 
  scale_y_continuous(n.breaks = 6) +
  scale_color_manual(labels = c("First Round Loss", "Second Round loss",        "National Runner-Up", "Loss in the Elite Eight", "Loss in the Sweet 16"),   values = c("#800000", "#F76018", "#9046CF", "#F5BB00","#51D6FF")) +
  theme_clean() +
  theme(legend.position = c(0.7, 0.775)) +
  annotation_raster(Auburn_logo, xmin = 2011, xmax = 2017, ymin = 4.1, ymax = 5.9)






