#Load packages
library(tidyverse)
library(extrafont)
library(ggtext)
library(Cairo)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

freed_slaves_wide <- tuesdata$freed_slaves 

#Long format data used to make the plot later
freed_slaves_wide %>% 
  mutate(Slave = case_when(Year == 1800 ~ Slave + 1,
                           TRUE ~ Slave)) %>% 
  pivot_longer(2:3, 
               names_to = "status",
               values_to = "percentage") -> freed_slaves_long
  
#Percentage labels
freed_slaves_wide %>% 
  mutate(position = case_when(Year == 1800 ~ 91.25,
                              Free != 100 ~ Slave + 2,
                              TRUE ~ 91),
         Free = paste0(Free, "%")) -> text_label

#Create df with values for the segments.
text_label %>% 
  filter(!Year %in% c(1790, 1870)) %>% 
  mutate(y = position - 3.25,
         yend = 100) -> h_segments

#Colors
dubois_colors <- c("#278251", "black")
background <- "#DFD4C7"

#Plot
freed_slaves_long %>% 
  mutate(percentage = case_when(status == "Slave" ~ percentage - 5,
                                status == "Free" ~ percentage + 5),
         percentage = case_when(percentage == -5 ~ 0,
                                percentage == 105 ~ 100,
                                TRUE ~ percentage)) %>% 
#The mutate call alters the percentages values in order to allow for more space in which to put annotations. Without it the space is too narrow. The real percentage values are stated by the text_label.
  ggplot() +
  geom_area(aes(Year, percentage, 
                fill = status)) +
  geom_text(text_label,
            mapping = aes(Year, position - 5,
                          label = Free),
            fontface = "bold") +
  geom_text(text_label,
            mapping = aes(Year, 102.75,
                          label = Year),
            fontface = "bold",
            family = "B52-ULCW00-ULC",
            size = 5) +
  annotate('text',
           x = 1830,
           y = 95,
           label = "FREE - LIBRE",
           size = 7,
           fontface = "bold",
           family = "B52-ULCW00-ULC") +
  annotate('text',
           x = 1830,
           y = 60,
           label = "SLAVES",
           size = 7,
           fontface = "bold",
           color = background,
           family = "B52-ULCW00-ULC") +
  annotate('text',
           x = 1830,
           y = 56,
           label = "ESCLAVES",
           size = 7,
           fontface = "bold",
           color = background,
           family = "B52-ULCW00-ULC") +
  geom_segment(h_segments,
               mapping = aes(x = Year,
                             xend = Year,
                             y = y,
                             yend = yend)) +
  geom_segment(aes(x = 1830,
                   xend = 1830,
                   y = 92,
                   yend = 97),
               color = "#278251") +
  scale_fill_manual(values = dubois_colors) +
  labs(y = NULL, x = NULL,
       title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES .\n\nPROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÈRIQUE .",
       subtitle = "DONE BY ATLANTA UNIVERSITY .",
       caption = "\n\nVISUALIZATION: **@LUISFREII** | SOURCE: **#DUBOISCHALLENGE**") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(background),
        plot.background = element_rect(background),
        legend.position = "none",
        plot.title = element_text(size = 16, 
                                  hjust = 0.5,
                                  family = "B52-ULCW00-ULC"),
        plot.subtitle = element_text(size = 11, 
                                     hjust = 0.5, 
                                     margin = margin(1, 0, 2, 0, unit = "cm"),
                                     family = "B52-ULCW00-ULC"),
        plot.caption = element_markdown(size = 8,
                                        hjust = 0.5),
        panel.grid = element_blank()
        
  )

#Code to save the plot
# ggsave("dubois.png",
#        width = 19.5,
#        height = 25,
#        units = "cm",
#        dpi = 320,
#        type = "cairo-png")

