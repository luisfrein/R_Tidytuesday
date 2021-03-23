#Load packages
library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

roll_calls <-  tuesdata$roll_calls

#Unnest desc column and remove common words 
roll_calls %>% 
  unnest_tokens(word, descr) %>% 
  anti_join(stop_words) -> roll_calls_words

#Correlation between words
roll_calls_words %>% 
  group_by(word) %>% 
  filter(!is.na(word), n() > 20) %>% 
  pairwise_cor(word, session, sort = TRUE) -> word_cors

#Vector of colors
colores <- 
  c(
    line = "#9D0208",
    point = "#FAA307",
    bkg = "#03071E"
  )

#aRt (?)
set.seed(1512)

word_cors %>%
  filter(correlation > .5, item1 %in% c("nuclear", "weapons", "treaty")) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "dh") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE, color = colores[["line"]]) +
  geom_node_point(color = colores[["point"]], size = 3) +
  coord_polar() +
  theme_void() +
  theme(panel.background = element_rect(fill = colores[["bkg"]]),
        plot.background = element_rect(fill = colores[["bkg"]]))

#Code to save the plot. I went for SVG, and then used InkScape to convert to PNG for better Antialiasing
# ggsave("UN_art.png",
#        width = 22,
#        height = 15,
#        dpi = 500,
#        units = "cm",
#        type = "cairo-png")

# ggsave("UN_art.svg",
#        width = 22,
#        height = 15,
#        dpi = 500,
#        units = "cm")
