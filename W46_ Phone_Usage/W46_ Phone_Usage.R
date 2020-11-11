#Load packages
library(tidyverse)
library(extrafont)
library(patchwork)

# Get the Data
mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')

#Explore!
# Filter to get only Venezuela
mobile_vzla <- mobile %>% 
  filter(entity == "Venezuela")

#Fill GDP per cap NAs
mobile_vzla[26, 5] <- 16257
mobile_vzla[27, 5] <- 13159
mobile_vzla[28, 5] <- 12323

#GDP plot
gdp_plot <- mobile_vzla %>% 
  ggplot() +
  geom_line(aes(year, gdp_per_cap), color = "#E04943", size = 1.5) +
  scale_x_continuous(breaks = seq(1992, 2016, by = 3)) +
  labs(x = NULL, subtitle = "Venezuela is a country that heavily relies on oil exports and in 2014 the oil barrel\nprice started to drop, this drop would continue throught the years and take a big\nhit on Venezuela's GDP. With less money people will prioritize essential goods,\ninstead of mobile subscriptions.", y = "GDP Per Capita") +
  theme(plot.background = element_rect("#FFFFFF"),
        panel.background = element_rect("#FFFFFF"),
        panel.grid.major = element_line(color = "#CCCCCC"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#474747", face = 'bold', family = "Verdana"),
        axis.title.y = element_text(color = "#474747", face = 'bold', family = "Verdana"),
        plot.subtitle = element_text(color = "#474747", face = "bold.italic", size = 9, family = "Verdana")
  )

#Fill population NAs
mobile_vzla[25, 4] <- 30040000
mobile_vzla[26, 4] <- 30080000
mobile_vzla[27, 4] <- 29850000
mobile_vzla[28, 4] <- 29400000

#Population plot
pop_plot <- mobile_vzla %>% 
  mutate(total_pop = total_pop / 1000000) %>% 
  ggplot() +
  geom_line(aes(year, total_pop), color = "#E04943", size = 1.5) +
  scale_x_continuous(breaks = seq(1992, 2016, by = 3)) +
  labs(subtitle = "The country has been aflicted for some time with serious socioeconomic and political\nproblems. In response to that mass migrations started in the country around 2014\nand are still ongoing. Less people in the country means fewer subscriptions.", x = NULL, y = "Total Population (Millions)") +
  theme(plot.background = element_rect("#FFFFFF"),
        panel.background = element_rect("#FFFFFF"),
        panel.grid.major = element_line(color = "#CCCCCC"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#474747", face = 'bold', family = "Verdana"),
        axis.title.y = element_text(color = "#474747", face = 'bold', family = "Verdana"),
        plot.subtitle = element_text(color = "#474747", face = "bold.italic", size = 9, family = "Verdana")
  )

#Filter south american countries to compare with Venezuela
mobile_americas <- mobile %>% 
  filter(continent == "Americas", !is.na(gdp_per_cap), !is.na(mobile_subs), !entity == "Venezuela", entity %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Peru", "Uruguay", "Paraguay")) %>%
  group_by(year) %>% 
  summarise(sum_gdp = sum(gdp_per_cap), sum_subs = sum(mobile_subs)) %>% 
  mutate(gdp_per_cap = sum_gdp / 10, mobile_subs = sum_subs / 10, entity = "Americas") %>% 
  select(-(2:3)) 

mobile_vzla2 <- mobile_vzla %>% 
  select(-2, -4) 

mobile_Amr_vs_vzla <- bind_rows(mobile_americas, mobile_vzla2)

#South America VS Venezuela plot
mob_sus_plot <- mobile_Amr_vs_vzla %>% 
  ggplot(aes(year, mobile_subs, color = entity)) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(1992, 2016, by = 3)) +
  annotate("text", x = 2006, y = 106.5, label = "South America", fontface = "bold", size = 5, color = '#333333') +
  annotate("text", x = 2005, y = 100, label = "Venezuela", fontface = "bold", size = 5, color = '#E04943') +
  scale_color_manual(labels = c('South America', 'Venezuela'), values = c('#333333', '#E04943'), guide = guide_legend(title = NULL, direction = 'horizontal')) +
  labs(x = NULL, y = "Mobile subscriptions (per 100 people)") +
  theme(plot.background = element_rect("#FFFFFF"),
        panel.background = element_rect("#FFFFFF"),
        panel.grid.major = element_line(color = "#CCCCCC"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#474747", face = 'bold', family = "Verdana"),
        axis.title.y = element_text(color = "#474747", face = 'bold', family = "Verdana"),
        legend.position = "none"
  )

#Combine plots with patchwork
patchwork <- mob_sus_plot | (gdp_plot / pop_plot) 

patchwork +
  plot_annotation(title = "A Little Story of Venezuela's mobile subscriptions", subtitle = "The number of mobile subscriptions in Venezuela was once above the region average, but then\nsomething happened, the number of subscriptions stalled and then dropped below the region\naverage.The data gives us two possible causes for this drop.", 
                  caption = "Made by @luisfreii | Data: Our World In Data",
                  theme = theme(plot.title =  element_text(color = "#E04943", face = 'bold',    family = "Verdana", size = 25), 
                                plot.subtitle = element_text(color = "#474747", face = 'bold.italic', family = "Verdana") ,
                                plot.caption = element_text(color = "#474747", face = 'bold.italic', family = "Verdana", hjust = 0.5, vjust = 0.1), 
                                plot.background = element_rect("#FFFFFF")
                  )) & theme(plot.margin = margin(t = .25, b = .25, unit = "in")) 

ggsave("venezuela_story.png", height = 8, width = 14, dpi = 500)
