#Loade packages
library(tidyverse)
library(Cairo)
library(extrafont)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 2)

transit_cost <- tuesdata$transit_cost

#Explore
#Useful code that I didn't use in this script
#Goes through each column and filters out NAs

# transit_cost <- transit_cost %>% 
#   filter(across(everything(), ~!is.na(.))) %>%  
#   mutate(real_cost = as.numeric(real_cost))

#Check for NAs in every column. Got this trick from Sebastian Sauer Stats Blog: https://sebastiansauer.github.io/sum-isna/
map(transit_cost, ~sum(is.na(.)))

#Plot
transit_cost %>% 
  filter(!is.na(ppp_rate), cost_km_millions < 1000) %>% 
  #case_when works similarly to an ifelse. It checks a condition and assings a value depending on that condition
  mutate(ppp_groups = case_when(
    ppp_rate <= 1 ~ "0 to 1",
    ppp_rate <= 2 ~ "1 to 2",
    ppp_rate <= 3 ~ "2 to 3",
    ppp_rate <= 4 ~ "3 to 4",
    ppp_rate <= 5 ~ "4 to 5"
  )) %>% 
  ggplot(aes(ppp_groups, cost_km_millions)) +
  geom_jitter(size = 1.5, alpha = .6, width = .3, color = "#EF7B45") +
  geom_boxplot(aes(ppp_groups, cost_km_millions),
               color = "#D04D11", fill = "#F5F5F5",
               outlier.color = "#EF7B45", outlier.alpha = .8) +
  #Got this useful code from Neal Grantham's code. His twitter: @nsgrantham. Link to code: https://www.nsgrantham.com/transit-costs
  scale_y_continuous(labels = function(x) ifelse(x > 0, paste0("$", x, "M"), paste0("$", x)), breaks = seq(0, 1000, by = 200)) +
  labs(title = "The Transit Cost Project and PPP rates",
       subtitle = '\nThe  Transit Cost  Project created a database that spans\nmore  than  50 countries and totals more than 11,000 km\nof urban rail built since the late 1990s. \"Purchasing power\nparity (PPP) is a popular metric used by macroeconomic\nanalysts  that  compares   different  countries  currencies\nthrough a \'basket of goods\' approach.\" (INVESTOPEDIA\nSTAFF, 2020).

A  lower  PPP  rate  means  that  the  currency  has  less\nvalue,   in   other  words  you  would  need  more  of  that\ncurrency   compared  to   a  higher  PPP country  to  buy\nthe same product. Thus having greater cost.',
       caption = ",'\nMade by @luisfreii | Data: TransitCost.com",
       x = "\nPurchasing Power Parity (PPP)", y = NULL
       ) +
  theme(panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid = element_blank(),
        axis.text = element_text(family = "Arial Narrow", face = "bold", size = 10, color = "#391505"),
        axis.title = element_text(family = "Arial Narrow", face = "bold", size = 12, color = "#391505"),
        plot.title = element_text(family = "Arial Narrow", face = "bold", size = 16, color = "#391505"),
        plot.subtitle = element_text(family = "Arial Narrow", face = "italic", color = "#391505"),
        plot.caption = element_text(family = "Arial Narrow", hjust = .5, face = "bold", size = 10, color = "#391505")
  )

ggsave("Transit.png",
       width = 14,
       height = 20,
       units = "cm",
       dpi = 300,
       type = "cairo-png"
       )

  
