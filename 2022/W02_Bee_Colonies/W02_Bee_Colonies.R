#Load packages
library(tidyverse)
library(showtext)
library(scales)

#Get the data
tuesdata <- tidytuesdayR::tt_load('2022-01-11')
colony <- tuesdata$colony

#Get font
font_add_google('IBM Plex Sans', family = 'ibm')

## Automatically use showtext to render text for future devices
showtext_auto()
showtext_opts(dpi = 300)

#Get to work
colony %>% 
  filter(!is.na(colony_added),
         !is.na(colony_lost)) %>% 
  group_by(year) %>% 
  summarise(colonies_added = sum(colony_added),
            colonies_lost = sum(colony_lost)) %>% 
  mutate(fill_cond = case_when(year == 2019 | year == 2021 ~ 'highlight',
                                TRUE ~ 'normal')) %>% 
  ggplot() +
  geom_col(aes(year, colonies_added, fill = fill_cond), show.legend = FALSE) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(breaks = breaks_pretty(n = 7)) +
  scale_fill_manual(values = c('#deb841', '#6d6a75')) +
  labs(title = 'Number of Honey Bees colonies added in the US from 2015 to 2021',
       subtitle = 'The number of colonies added remained stable until 2019. That year saw\na significant collapse of colonies, a similar pattern repeated in 2021.',
       y = 'Colonies Added',
       x = NULL,
       caption = 'Visualization: **@luisfreii** | Source: **United States Department of Agriculture**') +
  theme(axis.text = element_text(family = 'ibm',
                                 size = 9),
        axis.title = element_text(family = 'ibm',
                                  size = 9),
        plot.title = element_text(family = 'ibm',
                             size = 12),
        plot.subtitle = element_text(family = 'ibm',
                                     size = 9),
        plot.caption = ggtext::element_markdown(family = 'ibm',
                                    size = 6),
        panel.background = element_rect('#F5F5F5'),
        plot.background = element_rect('#F5F5F5'),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        plot.margin = margin(25, 15, 10, 15))

#Code to save the plot
# ggsave('W02_Bee_colonies.png',
#        width = 18,
#        heigh = 10,
#        units = 'cm',
#        dpi = 300,
#        type = 'cairo')

# ggsave('W02_Bee_colonies.svg',
#        width = 18,
#        heigh = 10,
#        units = 'cm',
#        dpi = 300)



