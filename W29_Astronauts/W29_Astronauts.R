#Load packages
library(tidyverse)
library(extrafont)
library(ggtext)
#Get the data
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

#Plot!
plot <- astronauts %>% 
  group_by(name) %>% 
  slice(which.max(total_eva_hrs)) %>% 
  arrange(desc(total_eva_hrs)) %>% 
  head(15) %>% 
  ggplot(aes(total_eva_hrs, fct_reorder(name, total_eva_hrs))) +
  geom_point(aes(color = sex, fill = sex), pch = 21, stroke = 1.5, size = 5) +
  scale_color_manual(labels = c("Male", "Female"), values = c("#ECC761", "#8A8EF9"), limits = c("male", "female")) +
  scale_fill_manual(labels = c("Male", "Female"), values = c("#FD6F03", "#EBECFE"), limits = c("male", "female")) +
  labs(title = "**Out in the Vacumn of Space**", subtitle = "This 15 astronauts,
       <span style='color:#FD6F03;'>male</span> and
       <span style='color:#8a8ef9;'>female</span>,
       spent the most time in</span><br>ExtravehicularActivity (activity done outside a spacecrat).", caption = "Made by @luisfreii | Data:  Mariya Stavnichuk and Tatsuya Corlett", x = "Total Extravehicular Hours", y = NULL)

plot + theme(plot.title = element_markdown(color = "white", family = "Nasalization Rg"),
             plot.subtitle = element_markdown(color = "white", family = "Nasalization Rg"),
             plot.caption = element_text(color = "white", hjust = 0.5, family = "Nasalization Rg"),
             plot.background = element_rect("#07090E"),
             panel.background = element_blank(),
             axis.title = element_text(color = "white", family = "Nasalization Rg"),
             axis.text = element_text(color = "white", family = "Nasalization Rg"),
             panel.grid.minor = element_blank(),
             panel.grid.major = element_line(color = "#222222"),
             legend.position = "none"
             )

#ggsave("astronauts.png", width = 18, height = 17, units = "cm", dpi = 500)


  
