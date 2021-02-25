#Load packages
library(tidyverse)
library(extrafont)
library(scales)
library(ggtext)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 9)

earn <- tuesdata$earn
#Explore!
#vector with colors for the plot
c(
  "background" = "#FDFFFC",
  "men" = "#39406a",
  "women" = "#c42021",
  "text" = "#3D3D3D",
  "family" = "Verdana",
  "line" = "#7f7979"
) -> colores

earn %>% 
  filter(!sex == "Both Sexes") %>% 
  group_by(sex, year) %>% 
  summarise(median = median(median_weekly_earn)) %>% 
  pivot_wider(names_from = sex, values_from = median) %>% 
  mutate(dif = Men - Women) -> earn4plot

#Plot
  ggplot(earn4plot) +
  geom_line(aes(year, Men), 
            color = colores[["men"]], size = 1.25) +
  geom_line(aes(year, Women), 
            color = colores[["women"]], size = 1.25) +
  geom_ribbon(aes(x = year, ymin = Women, ymax = Men), 
              fill = "darkgrey", alpha = 0.75) +
  annotate("text", x = 2009.35, y = 710, 
           label = paste0("Δ", " $", earn4plot[1, 4]),
           color = colores[["text"]],
           family = colores[["family"]]) +
  annotate("text", x = 2020.5, y = 950, 
           label = paste0("Δ", " $", earn4plot[11, 4]),
           color = colores[["text"]],
           family = colores[["family"]]) +  
  geom_segment(aes(y = 638, yend = 768, x = 2009.9, xend = 2009.9), 
               color = colores[["line"]]) +
  geom_segment(aes(x = 2009.85, xend = 2009.95, y = 768, yend = 768), 
               color = colores[["line"]]) +
  geom_segment(aes(x = 2009.85, xend = 2009.95, y = 638, yend = 638), 
                 color = colores[["line"]]) +
  geom_segment(aes(y = 865, yend = 1038, x = 2020.1, xend = 2020.1), 
                 color = colores[["line"]]) +
  geom_segment(aes(x = 2020.05, xend = 2020.15, y = 1038, yend = 1038), 
                 color = colores[["line"]]) +
  geom_segment(aes(x = 2020.05, xend = 2020.15, y = 865, yend = 865), 
                 color = colores[["line"]]) +
  scale_x_continuous(breaks = pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5), 
                     labels = label_number(prefix = "$")) +
  labs(x = NULL, y = "Median Weekly Earning (Current Prices)\n",
       title = "**Median Weekly Earning in the US**",
       subtitle = glue::glue("The gap between median weekly earnings between <span style='color:{colores[['men']]};'>**men**</span> and<br><span style='color:{colores[['women']]};'>**women**</span> has widened from $129 in 2010 to $173 in 2020."),
       caption = "<br>Visualization: **@luisfreii** | Source: **U.S. BUREAU OF LABOR STATISTICS**") +
  theme(panel.background = element_rect(colores[["background"]]),
        plot.background = element_rect(colores[["background"]]),
        panel.grid.major.y = element_line(color = colores[["line"]]),
        plot.title = element_markdown(family = colores[["family"]], size = 14, color = colores[["text"]]),
        plot.subtitle = element_markdown(family = colores[["family"]], color = colores[["text"]]),
        plot.caption = element_markdown(family = colores[["family"]], color = colores[["text"]]),
        axis.title = element_text(family = colores[["family"]]),
        axis.text = element_text(family = colores[["family"]]),
        axis.ticks.y = element_blank())
  
#Code to save plot
  # ggsave("median_earning.png",
  #        width = 24.5,
  #        height = 14,
  #        dpi = 320,
  #        units = "cm",
  #        type = "cairo-png")
  

