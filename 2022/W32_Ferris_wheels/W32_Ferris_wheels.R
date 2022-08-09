#Load packages
library(tidyverse)
library(showtext)
library(scales)
library(glue)

#Get the data
tuesdata <- tidytuesdayR::tt_load('2022-08-09')
wheels_raw <- tuesdata$wheels

#Get the font for the plot
font_add_google('Fira Sans', family = 'fira')

#Automatically use showtext to render text for future devices
showtext_auto()
showtext_opts(dpi = 300)

#Explore
#I want to plot the evolution of construction cost through the years
wheels <- wheels_raw %>% 
  filter(!str_detect(construction_cost, 'Unknown'),
         !is.na(opened)) %>% 
  #Rename the column so I don't forget that cost are in Million USD
  rename(const_cost_millionUSD = construction_cost) %>% 
  #Excuse my regex
  mutate(const_cost_millionUSD = str_remove(const_cost_millionUSD, 'million USD'),
         const_cost_millionUSD = str_remove(const_cost_millionUSD, '\\$')) 


#Tibble of annotations
text_df <- tibble(x = lubridate::ymd("2006-10-01"),
                  y = 500,
                  z = str_wrap(glue("{wheels[14, 2]} with a height of {round(wheels[14, 3] / 3.281, digits = 0)} meters and a cost of {wheels[14, 15]} million USD."), width = 21)
                  )

#Plot
wheels %>% 
  mutate(const_cost_millionUSD = as.numeric(const_cost_millionUSD),
         #Convert height from feet to meters. I just prefer the metric system.
         height = height / 3.281) %>% 
  ggplot(aes(opened, const_cost_millionUSD, size = height)) +
  geom_point(color = '#613DC1', alpha = .6) +
  scale_size_continuous(range = c(4,7)) +
  annotate("curve", 
           x = lubridate::ymd("2016-06-01"),
           xend = lubridate::ymd("2013-06-01"),
           y = 580,
           yend = 550,
           curvature = 0.2, 
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           color = '#333333') +
  geom_text(text_df, mapping = aes(x = x,
                                   y = y,
                                   label = z),
            hjust = 0,
            color = '#3D3D3D',
            family = 'fira',
            size = 3) +
  scale_x_date(breaks = pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(x = NULL,
       y = 'Construction Cost ($ millions)',
       size = 'Height (in meters)',
       title = 'Ferris Wheels Construction Costs Through the Years',
       subtitle = 'Construction costs have been rising since 2008.',
       caption = 'Visualization: **@luisfreii** | Source: **@Emil_Hvitfeldt**') +
  coord_cartesian(clip = 'off') +
  theme(legend.position = 'top',
        axis.text = element_text(family = 'fira',
                                 size = 9,
                                 color = '#3D3D3D'),
        axis.title = element_text(family = 'fira',
                                  size = 9,
                                  color = '#3D3D3D'),
        plot.title = element_text(family = 'fira',
                                  size = 12,
                                  face = 'bold',
                                  color = '#1F1F1F'),
        plot.subtitle = element_text(family = 'fira',
                                     size = 9,
                                     color = '#3D3D3D'),
        plot.caption = ggtext::element_markdown(family = 'fira',
                                                size = 6,
                                                color = '#3D3D3D'),
        legend.text = element_text(family = 'fira',
                                   size = 9,
                                   color = '#3D3D3D'),
        panel.background = element_rect('#F5F5F5'),
        plot.background = element_rect('#F5F5F5'),
        legend.background = element_rect('#F5F5F5'),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        plot.margin = margin(25, 15, 10, 15))

#Code to save the plots
# ggsave('W32_Ferris_wheels.png',
#        width = 20,
#        heigh = 11,
#        units = 'cm',
#        dpi = 300)
# 
# ggsave('W32_Ferris_wheels.svg',
#        width = 20,
#        heigh = 11,
#        units = 'cm')
