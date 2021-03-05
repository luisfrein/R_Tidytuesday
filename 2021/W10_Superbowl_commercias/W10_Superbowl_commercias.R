#Load packages
library(tidyverse)
library(extrafont)
library(ggtext)
library(ggfittext)
library(magick)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 10)

youtube <- tuesdata$youtube

#Got it from svgrepo.com (https://www.svgrepo.com/svg/3514/american-football) I added it later with InkScape
#icon <- image_read_svg("https://www.svgrepo.com/download/3514/american-football.svg")

#Vector with colors and font family
c(
  "bars" = "#d62828",
  "big text" = "#0A0A0A",
  "small text" = "#666666",
  "family" = "JetBrains Mono"
) -> colores

#Plot
youtube %>%
  arrange(-comment_count) %>% 
  mutate() %>% 
  slice(1:7) %>% 
  mutate(view_count = format(view_count, big.mark = ","),
         title = fct_recode(title, "Budweiser 2017 Super Bowl Commercial | 'Born The Hard Way'" = "Budweiser 2017 Super Bowl Commercial | “Born The Hard Way”",
                            "'Super Bowl Babies Choir' feat. Seal | Music Video" = "“Super Bowl Babies Choir” feat. Seal | Music Video",
                            "Budweiser Wassup Commercial" = "budweiser wassup commercial"),
        title = glue::glue("<span style='color:{colores[['big text']]};font-size:12px'>**{title}**</span><br>
                            <span style='color:{colores[['small text']]};'>Published: {year}</span><br>
                            <span style='color:{colores[['small text']]};'>Views: {view_count}</span>")) %>% 
  ggplot(aes(comment_count, fct_reorder(title, comment_count), label = format(comment_count, big.mark = ","))) +
  geom_col(fill = bar_col) +
  geom_bar_text(family = "JetBrains Mono") +
  labs(title = glue::glue("<span style='color:{colores[['big text']]};'>Superbowl Commercials With the Most Comments</span>"),
       subtitle = glue::glue("<span style='color:{colores[['small text']]};'>These commercials have sparked the most conversation on YouTube.</span>"),
       caption = "Visualization **@luisfreii** | Source: **FiveThirtyEight**") +
  theme_minimal() +
  theme(axis.text.y = element_markdown(family = colores[['family']]),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_markdown(family = colores[['family']], size = 16),
        plot.subtitle = element_markdown(family = colores[['family']]),
        plot.caption = element_markdown(family = colores[['family']]))


#Code to save the plot
# ggsave("bowl_ads.svg",
#        width = 26.5,
#        height = 15.5,
#        units = "cm",
#        dpi = 320)
  