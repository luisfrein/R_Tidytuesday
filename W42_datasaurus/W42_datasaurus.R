#Load package
library(tidyverse)
#Read the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 42)
datasaurus <- tuesdata$datasaurus

#Filter the shape for dino, star and circle, then plot it. 
datasaurus_plot <- datasaurus %>% 
  filter(dataset %in% c("dino", "star", "circle")) %>% 
  ggplot() +
  geom_point(aes(x, y), color = "white") +
  geom_hline(aes(yintercept = mean(y)), linetype = "dashed", color = "#fddb3a", size = 1.25) +
  geom_vline(aes(xintercept = sd(y)), linetype = "dashed", color = "#fddb3a",
size = 1.25) +
  annotate("text", x = 87, y = 85, label = "Mean.y = 47.83\nSD.y = 26.84", color = "#fddb3a", fontface = 2) +
  facet_wrap(~dataset) + labs(title = "Datasaurus", subtitle = "Different Graphs can have the Same Stats", x = NULL, y = NULL)

#Adjust theme elements
datasaurus_plot + theme(plot.background = element_rect("#292929"),
                        panel.background = element_rect("#414141"),
                        panel.grid =  element_blank(),
                        axis.title = element_text(color = "#cfcfcf"),
                        axis.line = element_line(color = "#cfcfcf"),
                        axis.text = element_text(color = "#cfcfcf"),
                        strip.background = element_blank(),
                        strip.text = element_blank(),
                        plot.title = element_text(color = "#cfcfcf", hjust = 0.5),
                        plot.subtitle = element_text(color = "#cfcfcf", hjust = 0.5),
                        panel.spacing = unit(2, "lines")
                        )

