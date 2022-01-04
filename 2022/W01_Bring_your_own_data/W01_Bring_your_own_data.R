#Load libraries
library(tidyverse)
library(extrafont)
library(scales)
library(ggforce)
library(glue)

#Import data
books_read_raw <- read.csv("Books_read.csv") %>% 
  janitor::clean_names()

#Total number of words read
books_read_raw %>% 
  filter(year_read == 2021) %>% 
  mutate(total_words = sum(word_count),
         total_words = format(total_words, big.mark = ",")) -> books_read_raw

#Filter
books_read_raw %>% 
  filter(year_read == 2021) %>% 
  group_by(month_finished_read) %>% 
  summarise(avg_words = mean(word_count)) -> books_avg_words

books_read_raw %>% 
  filter(year_read == 2021) %>% 
  group_by(month_finished_read) %>% 
  count(month_finished_read) -> books_count

#Left join both dfs
books_avg_words %>% 
  left_join(books_count) %>% 
  mutate(month_finished_read = str_c(month_finished_read, '-01-21'),
         month_finished_read = lubridate::mdy(month_finished_read)) %>% 
  add_row(month_finished_read = as.Date("2021-10-01"),
          avg_words = 0,
          n = 0) %>% 
  add_row(month_finished_read = as.Date("2021-11-01"),
          avg_words = 0,
          n = 0) -> books_4plot


#Tibble with annotations
tibble(
  month = as.Date(c('2021-06-01', '2021-12-01', '2021-02-01', '2021-09-01')),
  y = c(2, 1, 3, 1),
  labels = c('Reread The Meditations', "Got out of my\n reader's block", 'Started reading The\nStormlight Archive Series', "Reader's block\nstarted")
) -> annotations

books_4plot %>% 
  ggplot(aes(month_finished_read, n)) +
  geom_line(size = 1.25,
            color = "#211A1D") +
  geom_point(aes(size = avg_words),
             pch = 21,
             stroke = 1.5,
             fill = '#F5F5F5',
             color = "#211A1D") +
  geom_mark_circle(annotations,
                   mapping = aes(month, y, group = month, description = labels),
                   color = NA, 
                   label.fill = NA,
                   label.fontsize = c(0, 8),
                   label.family = c('Fira Sans', 'Fira Sans'),
                   con.colour = "#211A1D") +
  scale_size(range = c(0, 7),
             labels = label_comma()) +
  coord_cartesian(clip = 'off') +
  scale_x_date(labels = label_date_short(), 
               breaks = breaks_width('1 month')) +
  labs(x = NULL, y = 'Number of Books Read',
       size = 'Avg Number of Words Read',
       title = 'My Reading Timeline (2021)',
       subtitle = glue('Read 19 books in 2021, for a total of {books_read_raw[1, 14]} words.'),
       caption = 'Visualization: **@luisfreii** | Made up Data') +
  guides(size = guide_legend(title.position = "top",
                             title.hjust = .5)) +
  theme(panel.background = element_rect('#F5F5F5'),
        plot.background = element_rect('#F5F5F5'),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'top',
        legend.background = element_blank(),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        plot.title = element_text(family = 'IBM Plex Sans',
                                  size = 20,
                                  color = "#211A1D"),
        plot.subtitle = element_text(family = 'Fira Sans',
                                     color = "#525252"),
        plot.caption = ggtext::element_markdown(family = 'Fira Sans',
                                                size = 8,
                                                color = "#525252"),
        axis.title = element_text(family = 'Fira Sans',
                                  color = "#525252"),
        axis.text = element_text(family = 'Fira Sans',
                                 color = "#525252"),
        legend.text = element_text(size = 9,
                                   family = "Fira Sans",
                                   color = "#525252"),
        legend.title = element_text(size = 10,
                                    family = "Fira Sans",
                                    color = "#525252"),
        plot.margin = margin(25, 15, 10, 15))

#Code to save the plot
ggsave('D24.Monochrome.svg',
       width = 25,
       height = 15,
       units = 'cm',
       dpi = 320)

ggsave('D24.Monochrome.png',
       width = 24,
       height = 15,
       units = 'cm',
       dpi = 320)
