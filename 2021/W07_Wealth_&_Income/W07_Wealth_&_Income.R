#Load packages
library(tidyverse)
library(extrafont)
library(ggtext)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 7)


income_time <- tuesdata$income_time

#Explore
#Inspiration and code to create the plot comes from Charlie Gallagher. Twitter: @CharlieGallaghr. Code: https://github.com/charlie-gallagher/tidy-tuesday/blob/master/hbcu/hbcu.R

#Collapse 50th and 10th percentile into 1 factor named 90% of People and rename 90th into 10% of People.
income_time %>% 
  mutate(percentile = fct_collapse(percentile, '90% of People' = c('50th', '10th'),
                                               '10% of People' = '90th')) %>%
  group_by(year, percentile) %>% 
  summarise(n = sum(income_family)) %>% 
  mutate(percentage = n / sum(n)) -> income_time_percentage

#Calculate percentages and create arguments for geom_rect.
income_time_percentage %>% 
  filter(year %in% c(1963, 2016)) %>% 
  mutate(xmin = case_when(year == 1963 ~ 1957.25,
                          TRUE ~ 2016.75),
         ymin = case_when(percentile == '10% of People' ~ percentage,
           TRUE ~ 0),
         ymax = case_when(ymin == 0 ~ lead(percentage),
                          TRUE ~ 1),
         year = case_when(year == 2016 ~ 2021.75,
                          TRUE ~ 1962.25)
         ) -> rects


#Horizontal lines that separate the two categories.
plot_lines <- 
  tibble(
    x = c(1957.25, filter(income_time_percentage, percentile == '10% of People')$year, 2021.75),
    y = c(0.612, filter(income_time_percentage, percentile == '10% of People')$percentage, 0.697)
  )

#Plot text
central_text <- tibble(
  x = c("90% of Families",
        "10% of Families"),
  y = c(0.87, 0.34),
  z = 1990
)

income_time_percentage %>%  
  filter(year %in% c(1963, 2016)) %>% 
  select(percentage) %>% 
  mutate(year = case_when(year == 1963 ~ 1959.75,
                          TRUE ~ 2019.25),
         y = central_text$y,
         percentage = paste0(
           round(percentage, digits = 3) * 100, "%"
         )) -> bars_text

extra_text <- 
  tibble(
  z = c(1963, 2016, "Familial Income Percentage"),
  x = c(1959.75, 2019.25, 1990),
  y = 1.02
)

#Plot colors
text_color <- "#354646"
background <- "#FFFDEB"
fill_colors <- c("#354545", "#32C356")
rect_colors <- c("#21823A", "#232E2E")

#Plot
  ggplot(income_time_percentage) +
    geom_rect(data = rects, aes(xmin = xmin,
                                xmax = year,
                                ymin = ymin,
                                ymax = ymax),
              fill = rep(rect_colors, 2)) +
    geom_area(aes(year, percentage, 
                fill = percentile), 
            size = 1,
            show.legend = FALSE) +
    geom_line(data = plot_lines, aes(x, y),
              color = background,
              size = 2) +
    geom_text(data = central_text,
            aes(z, y, label = x),
            color = background,
            fontface = "bold",
            family = "Tw Cen MT") +
    geom_text(data = bars_text,
              aes(year, y, label = percentage),
              color = background,
              fontface = "bold",
              family = "Tw Cen MT") +
    geom_text(data = extra_text,
              aes(x, y, label = z),
              color = text_color,
              fontface = "bold",
              family = "Tw Cen MT") +
    scale_fill_manual(values = fill_colors) +
    labs(title = "Income Inequality on the Rise",
         subtitle = "From 1963 to 2016, the familial income of the top 10% families\nin the US has increased by 8.5%. This means that the income of\nthe top families is 2.3 times greater than the rest of the families.",
         caption = "Visualization: **@luisfreii** | Source: **Urban Institute** & **US Census**") +
    theme_void() +
    theme(panel.background = element_rect(color = background,
                                          fill = background),
        plot.background = element_rect(background),
        plot.margin =  margin(15, 60, 15, 60),
        plot.title = element_text(color = "#121717", 
                                  hjust = 0.1,
                                  size = 20,
                                  family = "Tw Cen MT"),
        plot.subtitle = element_text(color = text_color,
                                     margin = margin(2, 0, 30, 0),
                                     hjust = 0.12,
                                     family = "Tw Cen MT"),
        plot.caption = element_markdown(color = text_color,
                                        family = "Tw Cen MT")
        ) 

#Code to save the plot
  # ggsave("Income_area.png",
  #        width = 20,
  #        height = 15,
  #        unit = "cm",
  #        dpi = 320,
  #        type = 'cairo')

      