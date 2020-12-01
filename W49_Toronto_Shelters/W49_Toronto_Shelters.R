#Load packages
library(tidyverse)
library(waffle)
#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 49)

shelters <- tuesdata$shelters

#Explore!
#Extract year, month and unite it in a new column.
shelters <- shelters %>% 
  mutate(year = lubridate::year(occupancy_date),
         month  = lubridate::month(occupancy_date)) %>%
  unite("date", year:month , sep = "/", remove = FALSE)

#Monthly occupancy
shelters %>% 
  group_by(shelter_city, date) %>% 
  summarise(month_occupancy = sum(occupancy)) %>% 
  ggplot(aes(date, month_occupancy, color = shelter_city)) +
  geom_point()


shelters %>% 
  group_by(sector) %>% 
  summarise(sum = n_distinct(shelter_name)) %>% 
  arrange(sum) %>% 
  ggplot(aes(fill = sector, values = sum)) +
  geom_waffle(n_rows = 13, size = 0.33, colour = "white", flip = TRUE) +
  scale_fill_manual(limits = c("Families", "Youth", "Co-ed", "Women", "Men"),
    values = c("#ED6A5A", "#F3C969", "#7DDE99", "#909CC2", "#AA9D9F"))
