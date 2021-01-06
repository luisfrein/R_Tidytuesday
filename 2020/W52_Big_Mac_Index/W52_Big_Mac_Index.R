#Load packages
library(tidyverse)
library(lubridate)
library(extrafont)
library(Cairo)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 52)

big_mac <- tuesdata$`big-mac`

#Explore!
big_mac <- big_mac %>% 
  mutate(dollar_price == round(dollar_price, 2))

#Create the firts half
half_2010 <- big_mac %>% 
  mutate(year = year(date), month = month(date)) %>% 
  filter(year %in% c(2010, 2020), month == 07, name %in% c("Argentina", "Switzerland", "Canada", "United States", "South Africa", "South Korea", "Thailand", "Euro area","China")) %>% 
  slice(1:9)

#Calculate percentage
half_2010 <- half_2010 %>% 
  mutate(raw_index = ((dollar_price * 100) / 3.73) - 100)

half_2010[8, 23] <- 0  #Assing 0 to USA index

#Create second half and calculate percentage
half_2020 <- big_mac %>% 
  mutate(year = year(date), month = month(date)) %>% 
  filter(year == 2020, month == 07, name %in% c("Argentina", "Switzerland", "Canada", "United States", "South Africa", "South Korea", "Thailand", "Euro area","China")) %>% 
  mutate(raw_index = ((dollar_price * 100) / 5.71) - 100)

#Bind both halves
big_mac2 <- bind_rows(half_2010, half_2020)

#Select the columns needed and make the data wider
big_mac2 <- big_mac2 %>% 
  select(4, 21, 23) %>% 
  pivot_wider(names_from = year, values_from = raw_index)

#Plot labels
left_label <- paste(big_mac2$name, round(big_mac2$`2010`, 2) ,sep = ", ")
right_label <- paste(big_mac2$name, round(big_mac2$`2020`, 2) ,sep = ", ")

#Plot colors
plot_colors <- c("#B0413E", "#02C3BD", "#F49E4C", "#697A21", "#8C429E", "#AE6F8F", "#5C52CB", "#08A045", "#0000E0")

#Create plot
plot <- big_mac2 %>% 
  ggplot() +
  geom_segment(aes(x = 1, xend = 2, y = `2010`, yend = `2020`), size = .75, color = plot_colors) + 
  geom_vline(xintercept = 1, linetype = "dashed", size = .1) + 
  geom_vline(xintercept = 2, linetype = "dashed", size = .1) +
  xlim(.5, 2.5) + ylim(-70, 80) +
  geom_text(label = left_label, y = big_mac2$`2010`, x = rep(1, NROW(big_mac2)), hjust = 1.1, size = 3.8, family = "Arial Narrow") +
  geom_text(label = right_label, y = big_mac2$`2020`, x = rep(2, NROW(df)), hjust = -0.1, size = 3.8, family = "Arial Narrow") + 
  geom_text(label = "2010", x = 1, y = 1.1 * (max(big_mac2$`2010`, big_mac2$`2020`)), hjust = 1.2, size = 6, family = "Arial Narrow") + 
  geom_text(label = "2020", x = 2, y = 1.1 * (max(big_mac2$`2010`, big_mac2$`2020`)), hjust = -0.1, size = 6, family = "Arial Narrow")  

#Finishing touches
plot + 
  labs(y = "Raw Big Mac Index", x = NULL,
       title = "Burgernomics from 2010 to 2020",
       subtitle = "How much currencies were under-or over-valued\nrelative to the US dollar, %, from 2010 to 2020.\n",
       caption = "Made by @luisfreii | Data: The Economist") +
  theme(panel.background = element_blank(), 
          plot.title = element_text(face = "bold", family = "Arial Narrow", size = 14),
          plot.subtitle = element_text(family = "Arial Narrow", size = 11),
          plot.caption = element_text(face = "bold", family = "Arial Narrow", size = 10, hjust = 0.5),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))  

#Code to save the plot
# ggsave("Burgernomics.png",
#        height = 22,
#        width = 17,
#        units = "cm",
#        dpi = 500,
#        type = "cairo-png")

