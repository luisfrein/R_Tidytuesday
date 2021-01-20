#Load packages
library(tidyverse)
library(extrafont)
library(scales)
library(ggtext)
library(glue)
library(rKenyaCensus)
#Get the data
rural_population <- rKenyaCensus::V2_T2.2a
urban_population <- rKenyaCensus::V2_T2.2b

#Explore!
urban_rural_population <- 
  rural_population %>% 
  select(1:2) %>% 
  bind_cols(urban_population$Sex_Total) %>% 
  slice(-1) %>% 
  rename(rural_pop = Sex_Total, urban_pop = ...3) %>% 
  mutate(rural_pop = replace_na(rural_pop, 0), rural_percentage = rural_pop * 100 / (urban_pop + rural_pop), color_cond = case_when(rural_percentage > 50 ~ "#1B5299", TRUE ~ "#E63946"), County = fct_recode(County, "TAITA-TAVETA" = "TAITA/TAVETA"))

#Plot colors
urban <- "#C42021"
rural <- "#2B304F"
bkgrd <- "#f5f7fe"
text_color <- "#525252"

#Cleveland dot plot of urban/rural population
urban_rural_population %>% 
  mutate(color_cond = case_when(rural_percentage < 50 ~ glue("{urban}"), 
                                TRUE ~ glue("{rural}")),
         County = case_when(rural_percentage < 50 ~ glue("<span style='color:{urban};'>{County}</span>"), 
                            TRUE ~ glue("<span style='color:{rural};'>{County}</span>")),
         Total = urban_pop + rural_pop) %>% 
  pivot_longer(2:3, names_to = "pop_type", values_to = "pop_value") %>% 
  ggplot(aes(pop_value, fct_reorder(County, pop_value), color = pop_type)) +
  geom_line(aes(group = County), color = '#525252', size = 1) +
  geom_point(size = 4, show.legend = FALSE) +
  scale_color_manual(values = c(glue("{rural}"), glue("{urban}"))) +
  scale_x_continuous(labels = label_number(scale = 1 / 1000000, suffix = "M", accuracy = 1), position = "top") +
  labs(title = glue("Kenya's <span style='color:{urban};'>Urban</span> VS <span style='color:{rural};'>Rural</span> Population"),
       subtitle = glue("Kenya's <span style='color:{urban};'>urban</span> and <span style='color:{rural};'>rural</span> population by county. The county's<br>color is determined by the predominant type of population<br>in said county."),
       x = "\nPopulation\n", y = NULL,
       caption = "<br>Data: Kenya Population and Housing Census via {rKenyaCensus}<br>Graphic by @luisfreii") +
  theme(plot.title = element_markdown(face = "bold", color = text_color, family = "Source Sans Pro"),
        plot.subtitle = element_markdown(face = "bold", color = text_color, family = "Source Sans Pro"),
        plot.caption = element_markdown(face = "bold", color = text_color, hjust = 0, family = "Source Sans Pro"),
        axis.title = element_markdown(face = "bold", color = text_color, hjust = 0, family = "Source Sans Pro"),
        axis.text = element_markdown(face = "bold", color = text_color, family = "Source Sans Pro"),
        axis.text.y = element_markdown(face = "bold"),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(bkgrd),
        plot.background = element_rect(bkgrd),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = "darkgrey"))

# ggsave("Kenya_census.png",
#        width = 15,
#        height = 20,
#        units = "cm",
#        dpi = 320)
