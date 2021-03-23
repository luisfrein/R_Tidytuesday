#Load packages
library(tidyverse)
library(tidytext)
library(widyr)
library(extrafont)
library(igraph)
library(ggraph)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

unvotes <- tuesdata$unvotes
roll_calls <-  tuesdata$roll_calls

#Explore!
unvotes %>% 
  filter(str_detect(country, "Venezuela")) -> ven_votes

ven_votes %>% 
  left_join(roll_calls) -> ven_votes

ven_votes %>% 
  filter(!vote == "abstain") %>% 
  unnest_tokens(word, short) %>% 
  anti_join(stop_words) -> ven_votes

ven_votes %>% 
  filter(!is.na(word)) %>% 
  group_by(vote) %>% 
  count(word, sort = TRUE) 

#Word correlation
roll_calls %>% 
  unnest_tokens(word, short) %>% 
  anti_join(stop_words) -> roll_calls_words

roll_calls_words %>% 
  pairwise_count(word, session, sort = TRUE) -> word_pairs

roll_calls_words %>% 
  group_by(word) %>% 
  filter(!is.na(word), n() > 20) %>% 
  pairwise_cor(word, session, sort = TRUE) -> word_cors

set.seed(1512)

word_cors %>%
  filter(correlation > .73) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
