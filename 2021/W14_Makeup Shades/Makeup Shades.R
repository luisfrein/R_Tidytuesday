#Load packages
library(tidyverse)
library(extrafont)
library(patchwork)
library(ggdist)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 14)

allShades <- tuesdata$allShades

#Vector with colors and font family
c(
  text = "#F9874E",
  bkg = "#081E1B",
  dotinterval = "#742727",
  title = "Abril Fatface",
  body = "Fira Sans Medium"
) -> colores

#Color Wheel
tibble(
  y = 1,
  x = 1:12,
  hue1 = c("#E00600", 
           "#E06600",  
           "#E09500", 
           "#E0BA00", 
           "#E0E000", 
           "#8BD100", 
           "#00B300", 
           "#008686", 
           "#103896", 
           "#32129A", 
           "#630896", 
           "#B40066")
) -> color_wheel

ggplot(color_wheel) +
  geom_rect(aes(ymax = 1:12, 
                ymin = 0:11,
                xmax = 1,
                xmin = 0,
                fill = hue1)) +
  scale_fill_identity() +
  geom_segment(x = 1.2,
               xend = 1.2,
               y = 0,
               yend = 1.5,
               color = colores[["text"]]) +
  geom_segment(x = 1.125,
               xend = 1.275,
               y = 0,
               yend = 0,
               color = colores[["text"]]) +
  geom_segment(x = 1.125,
               xend = 1.275,
               y = 1.5,
               yend = 1.5,
               color = colores[["text"]]) +
  annotate("text",
           x = 1.275,
           y = .75,
           label = " Hue\n 0-45°",
           angle = 50,
           hjust = 0,
           size = 4,
           color = colores[["text"]],
           family = colores[["body"]]) +
  coord_polar(theta = "y", clip = "off") +
  theme_void() -> wheel

#Create sat-light groups
  allShades %>% 
    filter(hue < 45) %>% 
    mutate(groups = case_when(sat <= .5 & lightness <= .5 ~ "Low Saturation and Low Light",
                              sat > .5 & lightness > .5 ~ "High Saturation and High Light",
                              sat <=.5 & lightness > .5 ~ "Low Saturation and High Light",
                              sat > .5 & lightness <= .5 ~ "High Saturation and Low Light")) %>% 
    add_count(groups) %>% 
    mutate(groups = glue::glue("{groups}\n\n{n} Shades")) -> allShades
  
#Plot  
set.seed(1512)
plot <-     
  ggplot(allShades, aes(hue, groups)) +
    geom_jitter(aes(color = hex)) +
    stat_pointinterval(color = colores[["dotinterval"]]) +
    scale_color_identity() +
    labs(y = NULL, x = "Hue",
         title = "How Makeup Brands Make their Shades?\n",
         subtitle = "To get the different shades, brands pick a hue (usually between 0 and 45 degrees)\nand then apply varying degrees of saturation and lightness. Below we can see\nthat shades with high light value are preferred.",
         caption = "Made by @luisfreii | Source: The Pudding Data") +
  theme(panel.background = element_rect(colores[["bkg"]]),
        plot.background = element_rect(colores[["bkg"]]),
        panel.grid = element_blank(),
        plot.title = element_text(color = colores[["text"]],
                                  family = colores[["title"]],
                                  size = 20),
        plot.subtitle = element_text(color = colores[["text"]],
                                     family = colores[["body"]]),
        plot.caption = element_text(color = colores[["text"]],
                                   family = colores[["body"]]),
        axis.text.y = element_text(color = colores[["text"]],
                                 family = colores[["body"]]),
        axis.text.x = element_text(color = colores[["text"]],
                                       family = colores[["body"]]),
        axis.title = element_text(color = colores[["text"]],
                                  family = colores[["body"]]),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.margin = margin(25, 25, 10, 25))

shade <- plot + inset_element(wheel, left = .7, right = .9, top = 1.1, bottom = 1)

#Code to save the plot
# ggsave("shades.svg",
#        width = 22,
#        height = 24,
#        units = "cm")

# ggsave("shades.png",
#        width = 22,
#        height = 24,
#        units = "cm")