#Load packages
library(tidyverse)
library(Cairo)
library(extrafont)
library(packcircles)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 3)

artwork <- tuesdata$artwork

#Explore!
artwork_count <- artwork %>%
  filter(!is.na(medium)) %>% 
  mutate(medium = str_extract(medium, "^([^ ]+)"), #Matches the first word of the string before an space
         medium = str_replace(medium, ",", " "), #Replaces the comma with an space
         medium = fct_recode(medium, "Engraving\non\nPaper" = "Line", "Engraving\non\nPaper" = "Engraving", "Pen/Ink" = "Pen", "Pen/Ink" = "Ink"), #Change factor names
         medium = str_trim(medium)) %>% # trims whitespace
  count(medium, sort = TRUE) %>% 
  head(15)

#Got all the instructions to make this type of plot from here: https://www.r-graph-gallery.com/305-basic-circle-packing-with-one-level.html

# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
  packing <-  circleProgressiveLayout(artwork_count$n, sizetype = "area")
  
#Adds space between the bubbles  
  packing$radius <- 0.95*packing$radius
  
  artwork_count <- bind_cols(artwork_count, packing)
  
  # The next step is to go from one center + a radius to the coordinates of a circle that
  # is drawn by a multitude of straight lines.  
  vertices <- circleLayoutVertices(packing, npoints = 50)
  
  vertices$value <- rep(artwork_count$n, each = 51)
  
#Create color scale
  vertices <- vertices %>% 
    mutate(bubble_color = case_when(value < 1000 ~ "#ADBEFF",
                                    value < 2000 ~ "#5C7CFF",
                                    value < 10000 ~ "#0031F5",
                                    value > 10000 ~ "#000C3D"))
  
#Add count to medium column
  artwork_count <- artwork_count %>% 
    mutate(medium = paste0(medium, "\n", n)) 
  
#Plot
  ggplot() +
  geom_polygon(data = vertices, aes(x, y, group = id, fill = bubble_color), color = "#EBEFFF", alpha = 0.6) +
  scale_fill_identity() +
  geom_text(data = artwork_count, aes(x, y, size = n, label = medium), family = "Lato", color = "#000414", fontface = "bold") +
  scale_size_continuous(range = c(2,6)) + #Controls font size
  labs(title = "The 15 Most Common Art Mediums Found in the Tate Art Museum\n",
         subtitle = 'In art, "medium" can refer to an artistic material. For example, graphite, oil and ink.\nOr it can refer to a type of art. For example, photographs, litographs and paintings.\nHere are the 15 most common mediums from around 70,000 artworks own or\njointly ownned by Tate and the National Galleries of Scotland.',
         caption = "Made by @luisfreii | Data: Tate Art Museum") + 
  theme(legend.position = "none",
          panel.background = element_rect("#EBEFFF"),
          plot.background = element_rect("#EBEFFF"),
          plot.title = element_text(family = "Lato", face = "bold", color = "#000414", size = 14),
          plot.subtitle = element_text(family = "Lato", face = "italic", color = "#000414"),
          plot.caption = element_text(family = "Lato", face = "bold", color = "#000414", hjust = 0, size = 10),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
          ) +
  coord_equal()
  
  #Only run these lines of codes if you want to save the plot in your device  
#Code to save the plot
  # ggsave("art_med.png",
  #        width = 17.2,
  #        height = 19.6,
  #        dpi = 320,
  #        units = "cm",
  #        type = "cairo")

#Trim images white borders. Got this from Jake Kaupp. Twitter: @jakekaupp
  # magick::image_read("art_med.png") %>% 
  #   magick::image_trim() %>% 
  #   magick::image_write("art_med.png")

