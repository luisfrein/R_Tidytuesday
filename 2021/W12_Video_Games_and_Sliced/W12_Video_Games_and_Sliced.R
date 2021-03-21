#Load packages
library(tidyverse)
library(extrafont)
library(scales)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 12)

games <- tuesdata$games

#Tibble of annotations
text_df <- tibble(x = lubridate::ymd(c("2015-07-05", "2016-12-18", "2019-03-01", "2021-03-08")),
                  y = c(100, 650, 360, 330),
                  z = c(str_wrap("CrossCode released on Steam in early access", width = 21),
                        str_wrap("The full game released for Linux, macOS, and Windows in September 2018", width = 34),
                        str_wrap("The game released for PlayStation 4, Xbox One, and Nintendo Switch in July 2020", width = 38),
                        str_wrap("CrossCode DLC: A New Home launched in February 2021 on Steam", width = 32)
                        ))

#Vector of colors and font family
c(
  "line" = "#4186D1",
  "points" = "#F2F3F8",
  "title" = "#211B44",
  "box" = "#292929",
  family = "Eight Bit Dragon" 
) -> colores

#Plot
games %>% 
  filter(gamename == "CrossCode",
         !year == 2012) %>% 
  mutate(ym = lubridate::ymd(paste0(year, month, 1))) %>% 
  group_by(ym, gamename) %>% 
  summarise(avg = sum(avg), 
            peak = sum(peak)) %>% 
  ggplot(aes(ym, avg)) +
  geom_line(color = colores[["line"]],
            size = 2) +
  geom_point(pch = 21, 
             fill = colores[["points"]], 
             color = colores[["line"]],
             stroke = 1,
             size = 3) +
  annotate("curve", 
           x = lubridate::ymd("2015-07-01"),
           xend = lubridate::ymd("2015-05-01"),
           y = 90,
           yend = 20,
           curvature = 0.2, 
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           color = colores[["title"]]) +
  annotate("curve", 
           x = lubridate::ymd("2018-04-01"),
           xend = lubridate::ymd("2018-08-01"),
           y = 700,
           yend = 739.09,
           curvature = -0.2, 
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           color = colores[["title"]]) +
  annotate("curve", 
           x = lubridate::ymd("2020-05-01"),
           xend = lubridate::ymd("2020-06-27"),
           y = 320,
           yend = 245,
           curvature = -0.2, 
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           color = colores[["title"]]) +
  annotate("curve", 
           x = lubridate::ymd("2021-03-01"),
           xend = lubridate::ymd("2021-02-01"),
           y = 320,
           yend = 260,
           curvature = 0.2, 
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           color = colores[["title"]]) +
  geom_text(text_df, mapping = aes(x = x,
                                   y = y,
                                   label = z),
            hjust = 0,
            color = colores[["title"]],
            family = colores[["family"]],
            size = 3.5) +
  scale_x_date(labels = label_date_short(), 
               date_breaks = "6 month", 
               limit = lubridate::ymd(c("2015-05-01", "2021-12-01"))) +
  scale_y_continuous(breaks = breaks_pretty(n = 8)) +
  labs(title = "Number of Seekers in the Playground",
       subtitle = "Number of players playing CrossCode on Steam\n",
       caption = "\n\nMade by @luisfreii | Source: Steam",
       y = "Average Number\nof Players",
       x = NULL) +
  theme(axis.title.y = element_text(angle = 0,
                                    hjust = 0,
                                    color = colores[["title"]],
                                    family = colores[["family"]]),
        panel.background = element_rect(colores[["points"]]),
        plot.background = element_rect(colores[["points"]]),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = colores[["title"]],
                                  family = colores[["family"]],
                                  size = 15),
        plot.subtitle = element_text(family = colores[["family"]],
                                     color = colores[["title"]]),
        axis.text = element_text(family = colores[["family"]],
                                 color = colores[["title"]]),
        plot.caption = element_text(color = colores[["title"]],
                                   family = colores[["family"]],
                                   size = 10)
        )

#Code to save the plot
#SVG
# ggsave("crosscode.svg",
#        width = 48,
#        height = 16,
#        unit = "cm",
#        dpi = 320)       

#PNG
# ggsave("crosscode.png",
#        width = 48,
#        height = 16,
#        unit = "cm",
#        dpi = 320)       