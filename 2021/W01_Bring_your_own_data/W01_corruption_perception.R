#Load packages
library(tidyverse)
library(here)
library(extrafont)
library(rnaturalearth)
library(ggtext)

#Get the data
corruption_perception <- readxl::read_xlsx(here("2021", "W01_Bring_your_own_data", "CPI2020_GlobalTablesTS_210125.xlsx"), skip = 2)

world <- ne_countries(scale = "medium", returnclass = "sf")

#Explore!
#Rename column
corruption_perception <- 
corruption_perception %>% 
  rename("cpi_score" = `CPI score 2020`)

#Join CPI data with world data
fill_data <- left_join(world, corruption_perception, by = c("iso_a3" = "ISO3"))

#Plot
fill_data %>% 
  filter(!iso_a3 == "ATA") %>% 
  ggplot() +
  geom_sf(aes(fill = cpi_score), color = "white", size = .1) +
  scale_fill_gradient(low = "#25133E", high = "#faae7b", 
                      limits = c(0, 100), breaks = c(20, 40, 60, 80, 100),
                      labels = c("0-19<br>**Highly Corrupt**", "20-39", "40-59", "60-79", "80-100<br>**Very Clean**")) +
  theme_void() +
  labs(title = "Corruption Perception Index 2020",
       subtitle = "The Corruption Perceptions Index (CPI), is an index<br>published annually byTransparency International.<br><br>The index ranks countries by their perceived levels<br>of <span style='color:#25133E;'>**corruption**</span>. Transparency International defines<br><span style='color:#25133E;'>**corruption**</span> as 'the abuse of entrusted power for<br>private gain'.",
       caption = "<br><br>Source: **Transparency International** | Visualization: **@luisfreii**",
       fill = "**Score**") +
  guides(fill = guide_legend(title.position = "top",
                             label.position = "bottom")) +
  theme(legend.position = "bottom",
        plot.title = element_markdown(family = "Tw Cen MT", hjust = .5, face = "bold", size = 18),
        plot.subtitle = element_markdown(family = "Tw Cen MT", hjust = .5),
        plot.caption = element_markdown(family = "Tw Cen MT", hjust = .5),
        legend.text = element_markdown(family = "Tw Cen MT"),
        legend.title = element_markdown(family = "Tw Cen MT", size = 13)
        )

#Save the plot
# ggsave("cpi_map.png",
#        width = 22,
#        height = 18,
#        units = "cm",
#        dpi = 320)
