#Load packages
library(tidyverse)
library(extrafont)
library(ggstream)
library(scales)
library(ggtext)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 15)

brazil_loss <- tuesdata$brazil_loss

#Explore!
brazil_loss_long <- 
  brazil_loss %>% 
  pivot_longer(4:14, names_to = "types", values_to = "loss")

#Determine total_loss by cause
brazil_loss_long %>% 
  group_by(types) %>% 
  summarise(total_loss = sum(loss)) %>% 
  arrange(-total_loss)


#Create tibble with labels
tibble(year = 2013.1,
       loss = c(1000000, 590000, 340000, 200000, 70000),
       types = c("Pasture", "Small Scale Clearing", "Commercial Crops", "Fire", "Selective Logging"),
       colores = c("#0091AD", "#2E6F95", "#5C4D7D", "#892B64", "#B7094C")) -> labels

#tibble for geom_richtext
tibble(x = 2010.75, 
       y = 3690000,
       text = "<span style='color:#252031;font-size:30px;'>Deforestation in Brazil</span><br><span style='color:#5C4D7D;'>Leading causes of loss<br>of forest in the country.</span>"
  ) -> title

brazil_loss_long %>% 
  filter(types %in% c("pasture", "small_scale_clearing", "commercial_crops", "fire", "selective_logging")) %>% 
  mutate(types = factor(types, levels = c("pasture", "small_scale_clearing", "commercial_crops", "fire", "selective_logging"))) -> brazil_loss_long  

ggplot() +
  geom_stream(brazil_loss_long, 
              mapping = aes(year, loss, fill = types),
              extra_span = .1,
              type = "ridge",
              show.legend = FALSE) +
  scale_y_continuous(labels = label_number_si()) +
  geom_text(labels, 
            mapping = aes(year, loss, 
                          label = types, 
                          color = colores), 
            hjust = 0,
            vjust = 1,
            family = "IBM Plex Sans") +
  geom_richtext(title, mapping = aes(x = x, y = y, label = text),
                hjust = .5,
                family = "IBM Plex Sans",
                color = NA,
                fill = NA,
                size = 5) +
  scale_x_continuous(breaks = seq(2001, 2015, 3), limits = c(2001, 2015)) +
  scale_fill_manual(values = c("#0091AD", "#2E6F95", "#5C4D7D", "#892B64", "#B7094C")) +
  scale_color_identity() +
  labs(x = NULL, y = "Hectares of Forest Lost",
       caption = "Made by @luisfreii | Source: Our World In Data") +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme(plot.margin = margin(25, 25, 20, 25),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = ggtext::element_markdown(color = "#291600",
                                                family = "IBM Plex Sans"),
        axis.text = element_text(family = "IBM Plex Sans"))

#Code to save the plot
# ggsave("6.Experimental.png",
#        width = 25,
#        height = 15,
#        units = "cm",
#        dpi = 320)

# ggsave("6.Experimental.svg",
#        width = 25,
#        height = 15,
#        units = "cm",
#        dpi = 320)
