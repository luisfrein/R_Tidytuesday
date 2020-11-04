#Load the packages
library(tidyverse)
library(extrafont)

#Get the data
ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

#Explore!
#Lets see the price change
price_ikea <- ikea %>% 
  filter(!old_price == 'No old price') %>% #Filters out items without an old price
  mutate(old_price = str_remove(old_price, 'SR')) %>% #Removes the letters 'SR' from the column
  mutate(old_price = as.numeric(str_remove(old_price, ','))) #Removes the comma from the column and turns the column from character to numeric

#Cleveland Dot Plot!
#First arrange the variation to see the highest values
price_ikea %>% 
  group_by(category) %>% 
  mutate(Variation = (price - old_price) / old_price) %>% 
  summarise(Avg_old_price = mean(old_price, na.rm = TRUE) * 0.27, Avg_price = mean(price * 0.27), Avg_Variation = (mean(Variation, na.rm = TRUE) * 100)) %>% 
  arrange(Avg_Variation)

plot <- price_ikea %>% 
  group_by(category) %>% 
  mutate(Variation = (price - old_price) / old_price) %>% 
  summarise(Avg_old_price = mean(old_price, na.rm = TRUE) * 0.27, Avg_price = mean(price * 0.27), Avg_Variation = (mean(Variation, na.rm = TRUE) * 100)) %>% 
  pivot_longer(2:3, names_to = 'price_time', values_to = 'price_value') %>% 
  ggplot() +
  geom_line(aes(price_value, fct_reorder(category, price_value), group = category), color = '#D8DDE8', size = 1) +
  geom_point(aes(price_value, fct_reorder(category, price_value), color = price_time), size = 4) +
  annotate('text', x = 235, y = 4.05, label = '-35%', family = 'Verdana', fontface = 'bold', color = '#FFFFFF', size = 3.5) +
  annotate('text', x = 365, y = 9.05, label = '-25%', family = 'Verdana', fontface = 'bold', color = '#FFFFFF', size = 3.5) +
  annotate('text', x = 508, y = 14.05, label = '-24%', family = 'Verdana', fontface = 'bold', color = '#FFFFFF', size = 3.5) +

  labs(title = "Old IKEA's Prices (USD) VS New Prices", color = NULL, subtitle = "IKEA's furniture prices dropped, on average 20%, across all\ncategories with the highest drop on Trolleys, Outdoor\nfurniture and Room dividers.") +
  scale_color_manual(labels = c('Avg Old Price', 'Avg Current Price'), values = c('#EB4404', '#C6F91F'), limits = c('Avg_old_price', 'Avg_price'), guide = guide_legend(direction = 'horizontal', reverse = TRUE))
  
plot + theme(plot.background = element_rect('#242B3D'),
             panel.background = element_rect('#242B3D'),
             legend.background = element_blank(),
             legend.key = element_blank(),
             legend.position = c(.4, 1),
             axis.title = element_blank(),
             plot.title = element_text(color = '#FFFFFF',face = 'bold', family = "Verdana", size = 24),
             plot.subtitle = element_text(color = '#FFFFFF',face = 'italic', family = "Verdana", size = 14, margin = margin(b = 25)),
             axis.text.y = element_text(color = '#FFFFFF', face = 'bold', family = "Verdana", size = 10),
             axis.text.x = element_text(color = '#FFFFFF', face = 'bold', family = "Verdana"),
             legend.text = element_text(color = '#FFFFFF', face = 'bold', family = "Verdana"),
             panel.grid.major = element_line(colour = "#353F5A"),
             panel.grid.minor = element_line(colour = "#353F5A"),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
             )




  