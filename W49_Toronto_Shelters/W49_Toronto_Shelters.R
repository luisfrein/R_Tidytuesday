#Load packages
library(tidyverse)
library(waffle)
library(patchwork)
library(ggtext)
library(extrafont)
library(Cairo)
#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 49)

shelters <- tuesdata$shelters

#Explore!
#Extract year, month and unite it in a new column.
shelters <- shelters %>% 
  mutate(year = lubridate::year(occupancy_date),
         month  = lubridate::month(occupancy_date)) %>%
  unite("date", year:month , sep = "/", remove = FALSE) 

#Monthly occupancy plot
mo_plot <- shelters %>% 
  group_by(date) %>% 
  summarise(month_occupancy = sum(occupancy) / 1000) %>% 
  mutate(date = lubridate::parse_date_time(date, "ym")) %>% 
  ggplot(aes(date, month_occupancy)) +
  geom_point(size = 2, color = "#371206") +
  geom_smooth(se = FALSE, color = "#F72C25", size = 1.5) +   
  labs(x = NULL, y = "Monthly Occupancy (Thousands)\n", subtitle = "**<span style='color:#F72C25;font-size:60px;'>50%</span><br>increase in monthly shelter occupancy<br>from Jan 2017 to Dec 2019.**") +
    theme(plot.subtitle = element_markdown(color = "#292929"),
          plot.background = element_rect("#EBEBEB"),
          panel.background = element_blank(),
          axis.title = element_text(color = "#292929", face = "bold"),
          axis.text = element_text(color = "#292929", face = "bold"),
          panel.grid = element_blank()
          )

#Waffle plot of sectors
ws_plot <- shelters %>% 
  group_by(sector) %>% 
  summarise(sum = n_distinct(shelter_name)) %>% 
  arrange(sum) %>% 
  ggplot(aes(fill = sector, values = sum)) +
  geom_waffle(n_rows = 9, size = 0.33, colour = "white", flip = FALSE, show.legend = FALSE) +
  scale_fill_manual(limits = c("Families", "Youth", "Co-ed", "Women", "Men"),
                    values = c("#FD8235", "#82869B", "#F7B32B", "#365181", "#F72C25")) +
  scale_y_continuous(breaks = seq(0, 9, by = 3)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  labs(title = "**Number of Shelters by Group**", 
       subtitle = "**Shelters in Toronto are sub-categorized by groups, these are:<br>
                  <span style='color:#FD8235;'>Families</span>,
                  <span style='color:#82869B;'>Youth</span>,
                  <span style='color:#F7B32B;'>Co-ed</span>,
                  <span style='color:#365181;'>Women</span> and
                  <span style='color:#F72C25;'>Men</span>.**") +
  theme(plot.title = element_markdown(color = "#292929"), 
        plot.subtitle = element_markdown(color = "#292929"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_rect("#EBEBEB"),
        panel.background = element_blank(),
        axis.title = element_text(color = "#292929", face = "bold"),
        axis.text = element_text(color = "#292929", face = "bold"))

#Circular bar plot of occupancy rate.
rate_plot <- shelters %>% 
  filter(!is.na(capacity)) %>% 
  group_by(month, year) %>% 
  summarise(n_distinct(shelter_name), year_occupancy = sum(occupancy), year_capacity = sum(capacity),rate = year_occupancy / year_capacity) %>% 
  ggplot(aes(month, rate, fill = rate)) + 
  geom_bar(stat = "identity") +
  ylim(-1, 1) +
  coord_polar() +
  scale_fill_gradient(low = "#ffb950", high = "#a50104") +
  scale_x_discrete(breaks = 1:12, 
                   limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  facet_wrap(~year, ncol = 3) +
  labs(x = NULL, y = NULL, 
       fill = "Occupancy Rate", 
       subtitle = "Monthly occupancy rate of shelters from
       <span style='color:#ffb950;'>low</span> to
       <span style='color:#a50104;'>high</span>.<br>
       From 2017 to 2019 monthly occupancy stayed above<br>90%.") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect("#EBEBEB"),
        panel.background = element_blank(),
        axis.title = element_text(color = "#292929", face = "bold"),
        axis.text = element_text(color = "#292929", face = "bold"),
        plot.subtitle = element_markdown(face = "bold", color = "#292929"),
        strip.text.x = element_text(face = "bold", color = "#292929"),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines")
        )
        

#Combine plots using patchwork.
def_plot <-  ws_plot / rate_plot / mo_plot

#Finishing touches.
def_plot +
  plot_annotation(title = "Toronto Shelters: an Infographic",
                  subtitle = "As of 2019 there were 62 shelters in Toronto.",
                  caption = "Made by @luisfreii | Data: open.toronto.ca via {opendatatoronto}", 
                  theme = theme(plot.title = element_text(color = "#292929", size = 22, face = "bold"),
                                plot.subtitle = element_text(color = "#292929", face = "bold.italic"),
                                plot.caption = element_text(color = "#292929", face = "bold", hjust = 0.5),
                                plot.background = element_rect("#EBEBEB"),
                                panel.background = element_rect("#EBEBEB")
                                ))

#Function to save the plot
 # ggsave("Shelters.png",
 #        width = 21,
 #        height = 29.7,
 #        units = "cm",
 #        dpi = 500,
 #        type = "cairo-png")
   

