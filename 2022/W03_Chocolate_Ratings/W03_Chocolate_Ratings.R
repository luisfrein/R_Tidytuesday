#Load packages
library(tidyverse)
library(showtext)
library(ggtext)

#Get the data
tuesdata <- tidytuesdayR::tt_load('2022-01-18')

chocolate <- tuesdata$chocolate

#Get the font for the plot
font_add_google('Source Sans Pro', family = 'source')

#Automatically use showtext to render text for future devices
showtext_auto()
showtext_opts(dpi = 300)

#Get to work
#Total number of chocolates made with the beans of specific countries
chocolate %>% 
  count(country_of_bean_origin, sort = TRUE) %>% 
  rename(total_chocolates = n) -> total_chocolates

#Number of chocolates where beans of origin match company location  
chocolate %>% 
  group_by(country_of_bean_origin) %>% 
  count(company_location, sort = TRUE) %>% 
  rename(chocolates_made_in_country = n) %>% 
  filter(country_of_bean_origin == company_location) %>% 
  #In this next step we left join the total_chocolates df with the match chocolates one
  left_join(total_chocolates) %>% 
  select(1, 3:4) %>% 
  pivot_longer(2:3, 
               names_to = 'total_or_in_country', 
               values_to = 'chocolates_count') -> matched_chocolates
  
matched_chocolates %>% 
  ggplot() +
  geom_line(aes(chocolates_count, 
                fct_reorder(country_of_bean_origin, chocolates_count)),
            color = '#525252', 
            size = 1)+
  geom_point(aes(chocolates_count, 
                 fct_reorder(country_of_bean_origin, chocolates_count), 
                 color = total_or_in_country),
             size = 4,
             show.legend = FALSE) +
  scale_x_continuous(position = 'top',
                     breaks = seq(0, 250, 75)) +
  scale_color_manual(values = c('#b07d62', '#774936')) +
  labs(y = NULL, 
       x = 'Number of Chocolate Bars',
       caption = 'Visualization: **@luisfreii** | Source: **Flavors of Cacao**',
       title = 'Chocolate Bars Made in House',
       subtitle = "The number of chocolate bars <span style='color:#b07d62;'>**made in-house**</span> vs the <span style='color:#774936;'>**total amount**</span><br>of chocolate bars made with the country's cocoa beans. Companies<br>around the globe use Venezuela's cocoa beans to make more than<br>200 chocolate bars, but only 31 of those were made in-house.<br>") +
  theme(axis.ticks = element_blank(),
        panel.background = element_rect('#F5F5F5'),
        plot.background = element_rect('#F5F5F5'),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = 'darkgrey'),
        axis.title = element_text(hjust = 0,
                                  size = 9,
                                  color = '#2A1A13'), 
        axis.text = element_text(size = 9,
                                 color = '#2A1A13'),
        plot.title = element_text(family = 'source',
                                  size = 17,
                                  color = '#2A1A13'),
        plot.subtitle = element_markdown(family = 'source',
                                         size = 11,
                                         color = '#2A1A13'),
        plot.caption = element_markdown(family = 'source',
                                    size = 10,
                                    color = '#2A1A13'),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        plot.margin = margin(25, 15, 10, 15))

#Save the plot
# ggsave("W03_Chocolate_Ratings.png",
#        width = 15,
#        height = 20,
#        units = "cm",
#        dpi = 320)

# ggsave("W03_Chocolate_Ratings.svg",
#        width = 15,
#        height = 20,
#        units = "cm",
#        dpi = 320)
  


  
