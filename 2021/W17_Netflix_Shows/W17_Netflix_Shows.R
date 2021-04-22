#Load packages
library(tidyverse)
library(extrafont)
library(gganimate)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 17)

netflix_raw <- tuesdata$netflix

#Get only the year and group by it
netflix_raw %>% 
  mutate(date_added = lubridate::mdy(date_added),
         year_added = lubridate::year(date_added)) %>% 
  filter(str_detect(listed_in, 'Anime'), 
         year_added <= 2020) %>% 
  group_by(year_added) %>% 
  count(type, sort = TRUE) -> netflix

#Tibble with annotations
tibble(
  annotation = c('MOVIE', 'TV SHOW', 'Anime is Getting Popular', 'Number of anime titles added to Netflix.\nFrom 2016 to 2020.'),
  x = c(2019, 2019, 2016.5, 2016.5),
  y = c(53, 25, 60, 52),
  size = c(8, 8, 8, 4),
  color = c('#F5F5F5', '#F5F5F5', '#525252', '#525252'),
  family = c('Fira Sans', 'Fira Sans', 'IBM Plex Sans', 'Fira Sans'),
  face = c('bold', 'bold', 'bold', 'plain')
) -> annotations

#plot
netflix %>% 
  ggplot(aes(year_added, n)) +
  geom_area(aes(fill = type),
            color = '#F5F5F5',
            size = 1,
            show.legend = FALSE) +
  geom_text(annotations,
            mapping = aes(x = x, y = y, label = annotation, size = size, color = color, family = family, fontface = face),
            hjust = 0) +
  scale_fill_manual(values = c("#E50914", "#1F1F1F")) +
  scale_size_identity() +
  scale_color_identity() +
  labs(x = NULL, y = "Number of Titles",
       caption = "Made by @luisfreii | Source: Kaggle") +
  coord_cartesian(expand = FALSE) +
  theme(plot.margin = margin(25, 25, 10, 25),
        plot.caption.position = "plot",
        panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = ggtext::element_markdown(color = "#291600",
                                                family = "IBM Plex Sans"),
        axis.text = element_text(family = "IBM Plex Sans"),
        axis.ticks.y = element_blank()) -> p1



#Add animation. 
a1 <- p1 + transition_reveal(year_added) 

anim <- animate(a1, 
                nframes = 40,
                fps = 10, 
                height = 13, width = 19, units = "cm", res = 150)

#Code to save the animation
#anim_save("W16.Netflix Titles.gif", animation = last_animation())

