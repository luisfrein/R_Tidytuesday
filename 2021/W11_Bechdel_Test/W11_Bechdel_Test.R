#Load packages
library(tidyverse)
library(extrafont)
library(waffle)
library(ggtext)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)

movies <- tuesdata$movies

#Vector of colors and font family
c(
  "fail" = "#7FA6AD",
  "pass" = "#AE665A",
  "background" = "#F5F6EE",
  "text" = "#454536",
  "title_fam" = "IBM Plex Sans SemiBold",
  "text_fam" = "IBM Plex Sans"
) -> colores

movies %>%
  filter(!is.na(genre)) %>% 
  mutate(genre = str_extract(genre, "\\w+"), #Gets the first word of the string.
         genre = fct_lump_min(genre, 50)) %>% 
  count(genre, binary) -> movies_df

  ggplot(movies_df, aes(fill = binary, values = n)) +
    geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE, show.legend = FALSE) +
    facet_wrap(~fct_reorder(genre, -n), nrow = 1, strip.position = "bottom") +
    scale_x_discrete() + 
    scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                       expand = c(0,0)) +
    scale_fill_manual(values = c(colores[["fail"]], colores[["pass"]])) +
    coord_equal() +
    labs(
      title = "The Bechdel Test and Movie Genres",
      subtitle = glue::glue("<br>The **Bechdel Test** is a test popularized by Alison Bechdel.<br>It follows three criteria:<br><br>1. It has to have at least two women in it.<br>2. Who talk to each other.<br>3. About something besides a man.<br><br>Here's the amount of movies that <span style='color:{colores[['pass']]};'>**passed**</span> or <span style='color:{colores[['fail']]};'>**failed**</span> the<br>Bechdel Test by genre.<br>"),
      caption = "Made by **@luisfreii** | Source: **FiveThirtyEight**",
      x = NULL,
      y = "Number of movies\n"
    ) +
    theme_minimal(base_family = "Roboto") + 
    theme(plot.background = element_rect(color = colores[["background"]],
                                         fill = colores[["background"]]),
          panel.background = element_rect(color = colores[["background"]],
                                          fill = colores[["background"]]),
          axis.ticks.y = element_line(),
          plot.title = element_text(color = colores[['text']], 
                                    family = colores[['title_fam']],
                                    size = 18),
          plot.subtitle = element_markdown(color = colores[['text']],
                                           family = colores[['text_fam']]
                                           ),
          plot.caption = element_markdown(color = colores[['text']],
                                          family = colores[['text_fam']]),
          axis.title = element_text(color = colores[['text']],
                                    family = colores[['text_fam']]),
          strip.text = element_text(color = colores[['text']],
                                    family = colores[['text_fam']]),
          axis.text = element_text(color = colores[['text']],
                                   family = colores[['text_fam']]),
          panel.grid = element_blank()
          )

#Code to save the plot png or svg. Png produces white borders, they can be easily remove with magick::image_trim(). Svg borders can be remove using InkScape.
  # ggsave("Bechdel1.png",
  #        width = 24.5,
  #        height = 14.5,
  #        unit = "cm",
  #        dpi = 320,
  #        type = "cairo-png")
   
  # ggsave("Bechdel1.svg",
  #        width = 24.5,
  #        height = 14.5,
  #        unit = "cm",
  #        dpi = 320)


