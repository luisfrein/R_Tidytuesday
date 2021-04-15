#Load packages
library(tidyverse)
library(extrafont)
library(geofacet)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 16)

post_offices_raw <- tuesdata$post_offices

#Count open offices per state
post_offices_raw %>%
  filter(is.na(discontinued)) %>%
  group_by(state) %>%
  count(state) -> post_offices

#Function to create a tile map
#Function comes from: https://medium.com/@NickDoesData/visualizing-geographic-data-in-r-fb2e0f5b59c5
create_gradient_state_tile_map <- function(state, value, title, caption, legend_title, low_color = '#1EBAC2', high_color = "#FF6C47", state_grid = 'us_state_grid2') {
  
  df <- as_tibble(data.frame(state, value))
  
  fig <- df %>% 
    mutate(x = 1) %>% # size of the bar being plotted. All bars should be same size to make perfect squares
    mutate(label_y = .5) %>%  # this location of state labels
    mutate(label_x = 1) %>% 
    ggplot() +
    geom_bar(mapping = aes(x = x, fill = value))  +
    facet_geo(~ state, grid = state_grid) +
    scale_fill_gradient(low = low_color,
                        high = high_color,
                        breaks = seq(400, 1600, 400),
                        labels = c('400', '800', '1,200', '1,600')) + # creates shading pattern
    ggtitle(title) +
    labs(caption = caption) +
    theme_classic() + # theme classic removes many ggplot defaults (grey background, etc)
    theme(plot.title = element_text(size = 38,
                                    family = 'IBM Plex Sans',
                                    color = "#3D3D3D"), # format the plot
          plot.title.position = 'plot',
          plot.caption = ggtext::element_markdown(color = "#3D3D3D", 
                                                  size = 20,
                                                  family = 'IBM Plex Sans'),
          plot.caption.position = 'plot',
          plot.margin = unit(c(1,1,1,1), "cm"),
          legend.text = element_text(size = 18,
                                     family = 'IBM Plex Sans'),
          legend.title = element_text(size = 22,
                                      family = 'IBM Plex Sans',
                                      color = "#3D3D3D"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          strip.text.x = element_blank(),
          axis.line = element_blank()) +
    geom_text(aes(x = label_x, y = label_y, label = state), 
              color = "white",
              size = 22,
              family = 'IBM Plex Sans') +
    guides(fill = guide_colorbar(title.position = "top",
                                 title = 'Count',
                                 title.hjust = .5, 
                                 barwidth = unit(1, 'lines'), 
                                 barheight = unit(30, 'lines')))
    
  
  return(fig)
}

#Create a tile map
tile_map <- create_gradient_state_tile_map(post_offices$state, post_offices$n, title = 'US Post Offices by State \n', legend_title = "Count", caption = 'Graphic: **@luisfreii** | Source: **Cameron Blevins and Richard W. Helbock (2021)**')

tile_map

#Code to save the plot
# ggsave('W16_US_Post_Offices.svg',
#        width = 24,
#        height = 15,
#        dpi = 320)
# 
# ggsave('W16_US_Post_Offices.png',
#        width = 24,
#        height = 15,
#        dpi = 320)
