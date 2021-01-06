#Load the packages
library(tidyverse)
library(Cairo)
library(extrafont)
library(magick)
library(ggtext)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 50)

women <- tuesdata$women

#Explore!
sa_women <- women %>% 
  filter(country %in% c("Brazil", "Argentina", "Venezuela", "Colombia", "Peru", "Ecuador")) 



evelina <- image_read("https://github.com/Zetluis/R_Tidytuesday/raw/master/images/W50_Women_of_2020/evelina.png") 

carolina <- image_read("https://github.com/Zetluis/R_Tidytuesday/raw/master/images/W50_Women_of_2020/carolina.png")

claudia <- image_read("https://github.com/Zetluis/R_Tidytuesday/raw/master/images/W50_Women_of_2020/claudia.png")

nemonte <- image_read("https://github.com/Zetluis/R_Tidytuesday/raw/master/images/W50_Women_of_2020/nemonte.png")

cibele <- image_read("https://github.com/Zetluis/R_Tidytuesday/raw/master/images/W50_Women_of_2020/cibele.png")

susana <- image_read("https://github.com/Zetluis/R_Tidytuesday/raw/master/images/W50_Women_of_2020/susana.png")

ruth <- image_read("https://github.com/Zetluis/R_Tidytuesday/raw/master/images/W50_Women_of_2020/ruth.png")

lea <- image_read("https://github.com/Zetluis/R_Tidytuesday/raw/master/images/W50_Women_of_2020/lea.png")


descriptions <- tibble(
  label = c(
    "**Evelina was born into a context of<br>
  vulnerability, but that did not pre-<br>
  vent her from becoming a football<br>
  coach and manager. She founded<br>
  the Argentinian Women's Football<br>
  Association at the age of 27.**",
    "**Carolina Castro is the first woman<br>
  to reach a governing position  at<br>
  the  Argentine  Industrial  Union<br>
  (UIA)  in  its  130-year history. Her<br>
  activism  has  contributed  to ad-<br>
  vancing the gender-equality agen-<br>
  da across party lines in a country<br>
  where public debate is highly po-<br>
  larised.**",
    "**Claudia López is the first female<br>
  mayor of Bogotá, Colombia’s<br>
  capital and the largest city in<br>
  the country.**",
    "**Nemonte Nenquimo is an in-<br>
  digenous  Waorani  woman<br>
  committed to defending her<br>
  ancestral territory, culture<br>
  and way of life in the Ama-<br>
  zon rainforest.**",
    "**Cibele is a retired  headteacher<br>
  who pioneered the teaching  of<br>
  racial equality to primary school<br>
  children in São Paulo.**",
    "**Susana is a humanitarian worker<br>
  who has spent 22 years assisting<br>
  in emergencies around the world.<br>
  She helped Cáritas de Venezuela<br>
  to establish  a tool that showed,<br>
  in real  time, the impact  of the<br>
  crisis on children. Susana also<br>
  founded a network of centres<br>
  providing nutritional support<br>
  for children living in slums.**",
    "**Ruth Shady has a doctorate in<br>
  archaeology and anthropology<br>
  and is vice-dean of research at<br>
  the Faculty of Social Sciences<br>
  of  the  National  University  of<br>
  San Marcos. She is director of<br>
  research at the Caral archaeol-<br>
  ogical  site, considered  to be<br>
  the oldest CIV. in the Ameri-<br>cas.**",
    "**Not many models can say their first<br>
  ever job was for Givenchy, but that’s<br>
  the case for Lea T. She’s been in the<br>
  business for more than 10 years, and<br>
  has graced the pages of high-profile<br>
  publications, including Marie Claire,<br>
  Grazia and Vogue.**"
    ),
  x = c(0, 1.9, 0, 1.9, 1.9, 0, 0, 1.9),
  y = c(-22, 18, -3.5, .5, -13.5, 11, 34, 40),
  hjust = c(0, 0, 0, 0, 0, 0, 0, 0),
  vjust = c(0, 0, 0, 0, 0, 0, 0, 0)
)

names <- tibble(
  label = c("**Evelina Cabrera<br><span style='color:#D16010;'>Leadership</span><br>Football manager<br>Argentina**",
            "**Carolina Castro<br><span style='color:#D16010;'>Leadership</span><br>Union Leader<br>Argentina**",
            "**Claudia López<br><span style='color:#D16010;'>Leadership</span><br>Mayor<br>Colombia**",
            "**Nemonte Nenquimo<br><span style='color:#D16010;'>Leadership</span><br>Waorani leader<br>Ecuador**",
            "**Cibele Racy<br><span style='color:#267D39;'>Identity</span><br>Teacher<br>Brazil**",
            "**Susana Raffalli<br><span style='color:#33B3D7;'>Knowledge</span><br>Nutritionist<br>Venezuela**",
            "**Ruth Shady<br><span style='color:#33B3D7;'>Knowledge</span><br>Archaeologist<br>Perú**",
            "**Leandra (Lea T)<br><span style='color:#267D39;'>Identity</span><br>Transgender model<br>Brazil**"
  ),
  x = c(0.4, 2.34, 0.4, 2.34, 2.34, 0.4, 0.4, 2.34),
  y = c(-11, 33, 3.5, 10.5, -6.5, 26, 49, 51.5),
  hjust = c(0, 0, 0, 0, 0, 0, 0, 0),
  vjust = c(0, 0, 0, 0, 0, 0, 0, 0)
)

center_label <- tibble(
  label = c("**S<br>A<br><br>
              W<br>O<br>M<br>E<br>N<br><br>
              O<br>F<br><br>
              2<br>0<br>2<br>0**"
  ),
  x = c(1.5),
  y = c(20),
  hjust = c(0.5),
  vjust = c(0.5)
)

plot <- sa_women %>% 
  ggplot() +
  xlim(0, 3) +
  ylim(-23, 57) +
  geom_richtext(data = descriptions, aes(x, y, label = label, hjust = hjust, vjust = vjust), fill = NA, label.color = NA) +
  geom_richtext(data = names, aes(x, y, label = label, hjust = hjust, vjust = vjust), fill = NA, label.color = NA) +
  geom_richtext(data = center_label, aes(x, y, label = label, hjust = hjust, vjust = vjust), fill = NA, label.color = NA, size = 13, family = "Arial Rounded MT Bold") +
  annotation_raster(evelina, xmin = 0, xmax = .4, ymin = -10.5, ymax = -5) +
  annotation_raster(carolina, xmin = 1.9, xmax = 2.3, ymin = 33, ymax = 38.5) +
  annotation_raster(claudia, xmin = 0, xmax = .4, ymin = 3.5, ymax = 9) +
  annotation_raster(nemonte, xmin = 1.9, xmax = 2.3, ymin = 10.5, ymax = 16) +
  annotation_raster(susana, xmin = 0, xmax = .4, ymin = 26.5, ymax = 32) +
  annotation_raster(cibele, xmin = 1.9, xmax = 2.3, ymin = -6.5, ymax = -1) +
  annotation_raster(ruth, xmin = 0, xmax = .4, ymin = 50, ymax = 55.5) +
  annotation_raster(lea, xmin = 1.9, xmax = 2.3, ymin = 51.5, ymax = 57) 
  
plot + 
  labs(caption = "**Made by @luisfreii | Data: BBC**") +
  theme(plot.background = element_rect("#FEF0EC"),
        panel.background = element_rect("#FEF0EC"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.caption = element_markdown(hjust = 0.5)
        )

#Function to save the plot
ggsave("women1.png",
       width = 21,
       height = 29.7,
       units = "cm",
       dpi = 500,
       type = "cairo-png")

