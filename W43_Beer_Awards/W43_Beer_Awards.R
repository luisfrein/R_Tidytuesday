#Load packages
library(tidyverse)
library(extrafont)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 43)
beer_awards <- tuesdata$beer_awards

#Bar plot displaying the top 10 cities
top_city <- beer_awards %>% 
  group_by(state, city) %>% 
  count(medal, sort = TRUE) %>% 
  head(16) %>% 
  summarise(across(n, sum)) %>%
  ggplot(aes(reorder(city, -n), n, label = state)) +
  geom_col(fill = '#fca311') +
  coord_flip() +
  annotate('text', x = 1, y = 10, label = 'COLORADO', family = "DejaVu Sans Mono", fontface = 'bold', color = '#14213d', hjust = 0, size = 5) +
  annotate('text', x = 2, y = 10, label = 'WISCONSIN', family = "DejaVu Sans Mono", fontface = 'bold', color = '#14213d', hjust = 0, size = 5) +
  annotate('text', x = 3, y = 10, label = 'OREGON', family = "DejaVu Sans Mono", fontface = 'bold', color = '#14213d', hjust = 0, size = 5) +
  annotate('text', x = 4, y = 10, label = 'CALIFORNIA', family = "DejaVu Sans Mono", fontface = 'bold', color = '#14213d', hjust = 0, size = 5) +
  annotate('text', x = 5, y = 10, label = 'SEATTLE', family = "DejaVu Sans Mono", fontface = 'bold', color = '#14213d', hjust = 0, size = 5) +
  annotate('text', x = 6, y = 10, label = 'OREGON', family = "DejaVu Sans Mono", fontface = 'bold', color = '#14213d', hjust = 0, size = 5) +
  annotate('text', x = 7, y = 10, label = 'ILLINOIS', family = "DejaVu Sans Mono", fontface = 'bold', color = '#14213d', hjust = 0, size = 5) +
  annotate('text', x = 8, y = 10, label = 'COLORADO', family = "DejaVu Sans Mono", fontface = 'bold', color = '#14213d', hjust = 0, size = 5) +
  annotate('text', x = 9, y = 10, label = 'TEXAS', family = "DejaVu Sans Mono", fontface = 'bold', color = '#14213d', hjust = 0, size = 5) +
  annotate('text', x = 10, y = 10, label = 'COLORADO', family = "DejaVu Sans Mono", fontface = 'bold', color = '#14213d', hjust = 0, size = 5) 
  
  
#Some changes to make the plot better 
top_city + labs(title = 'Great American Beer Festival (1987:2020) Medals: Top Ten Cities', subtitle = 'California is the state with the most medals(962), but only has\none city in the top 10 Colorado is just behind California with \n659 medals, but has the most cities(3), in the top 10.', x = NULL, y = NULL, caption = 'Made by @luisfreii | #TidyTuesday Week 43') +
  theme(plot.title = element_text(color = '#fca311',face = 'bold', family = "DejaVu Sans Mono", size = 24),
        plot.subtitle = element_text(color = '#fca311',face = 'bold', family = "DejaVu Sans Mono", size = 15),
        plot.caption = element_text(color = '#fca311',face = 'bold', family = "DejaVu Sans Mono", hjust = 0.5),
        axis.text.y = element_text(color = '#fca311', face = 'bold', family = "DejaVu Sans Mono", size = 12),
        axis.text.x = element_text(color = '#fca311', face = 'bold', family = "DejaVu Sans Mono"),
        panel.background = element_rect('#14213d'),
        plot.background = element_rect('#14213d'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  ) 

ggsave('GABF.png', height = 8, width = 15, dpi = 600)






   
