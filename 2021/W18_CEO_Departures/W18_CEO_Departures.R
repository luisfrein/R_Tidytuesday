#Load packages
library(tidyverse)
library(extrafont)
library(ggtext)
library(glue)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 18)

departures <- tuesdata$departures

#Count departures by code
departures %>% 
  filter(departure_code %in% 1:6) %>% 
  mutate(is_voluntary = case_when(departure_code %in% 1:4 ~ FALSE,
                                  TRUE ~ TRUE)) %>%
  group_by(departure_code, is_voluntary) %>% 
  count(departure_code) -> departures_count

#Departures by reason
departures_count %>% 
  mutate(departure_reason = case_when(departure_code == 1 ~ glue("<span style='color:#333333;font-size:12px;'>Death</span><br><span style='color:#525252;'>{n}</span>"),
                                      departure_code == 2 ~ glue("<span style='color:#333333;font-size:12px;'>Illness</span><br><span style='color:#525252;'>{n}</span>"),
                                      departure_code == 3 ~ glue("<span style='color:#333333;font-size:12px;'>Dismissed for job<br>perfomance</span><br><span style='color:#525252;'>{n}</span>"),
                                      departure_code == 4 ~ glue("<span style='color:#333333;font-size:12px;'>Dismissed for legal<br>violations or concerns</span><br><span style='color:#525252;'>{n}</span>"),
                                      departure_code == 5 ~ glue("<span style='color:#333333;font-size:12px;'>Retired</span><br><span style='color:#525252;'>{n}</span>"),
                                      departure_code == 6 ~ glue("<span style='color:#333333;font-size:12px;'>New opportunity</span><br><span style='color:#525252;'>{n}</span>"))) -> departures_count


  ggplot(departures_count,
         aes(n, fct_reorder(departure_reason, n), fill = is_voluntary)) +
  geom_col() +
  scale_fill_manual(values = c('#D34E24', '#044B7F')) +
  labs(title = 'For what reason do CEOs depart firms?',
       subtitle = "Turns out most CEOs depart firms for <span style='color:#044B7F;'>**voluntary**</span> reasons,<br>as oppose to <span style='color:#D34E24;'>**involuntary**</span> reasons.",
       caption = '**Visualization:** @luisfreii | **Source:** Gentry et al. (2021) | **Icon:** Freepik',
       y = NULL,
       x = NULL) +
  coord_cartesian(expand = FALSE) +
  theme(axis.text.y = element_markdown(family = 'Fira Sans',
                                       size = 8),
        plot.title = element_text(family = 'Roboto',
                                  size = 13,
                                  color = '#333333'),
        plot.subtitle = element_markdown(family = 'Fira Sans',
                                         color = '#333333',
                                         margin = margin(b = .6, unit = 'cm'),
                                         size = 10),
        plot.caption = element_markdown(family = 'Fira Sans',
                                        color = '#525252',
                                        size = 8),
        legend.position = 'none',
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect('#F5F5F5'),
        panel.background = element_rect('#F5F5F5'),
        panel.grid = element_blank(),
        plot.margin = margin(25, 15, 10, 15))

#Code to save the plot. I saved it to svg first because I get better antialiasing in Inkscape. The image was added in Inkscape too.
  # ggsave('W18.svg',
  #        width = 13,
  #        height = 16,
  #        units = 'cm',
  #        dpi = 320)
  
  # ggsave('W18.png',
  #        width = 13,
  #        height = 16,
  #        units = 'cm',
  #        dpi = 320)
  