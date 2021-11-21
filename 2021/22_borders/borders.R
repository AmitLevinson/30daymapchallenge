library(sf)
library(purrr)
library(viridis)
library(ggplot2)
library(dplyr)
library(ggtext)

# Sys.setlocale("LC_ALL", "Hebrew")

tel_aviv_neighborhoods_path <- list.files(pattern ="Neighbourhoods\\.shp$", recursive = TRUE)
tel_aviv_neighborhoods <- read_sf(tel_aviv_neighborhoods_path) %>% 
  st_transform(crs = 4326)

# Function to count number of bordering neighbourhoods
count_borders <- function (rownumber) {
  tel_aviv_neighborhoods[rownumber,] %>% 
    st_touches(tel_aviv_neighborhoods) %>% 
    unlist() %>% 
    length()
}

ta <- tel_aviv_neighborhoods %>% 
  mutate(
    # Wrap in purrr::possibly as it might result in NA's and throw an error if your sf is messy:(
    n_borders = map_dbl(seq(nrow(.)), possibly(count_borders, NA_real_))
  )

yarkon_arrow <- ta %>% 
  filter(n_borders == max(n_borders)) %>% 
  st_centroid() %>% 
  st_coordinates()


ta %>% 
  ggplot()+
  geom_sf(aes(fill = n_borders), color = "gray5", size = 0.1)+
  annotate(geom = "curve", x = yarkon_arrow[1] -0.003, xend = yarkon_arrow[1] , y = yarkon_arrow[2] - 0.015, yend = yarkon_arrow[2]-0.003,
           curvature =.1,  color = "gray25", size = 0.5, arrow = arrow(length = unit(2, "mm")))+
  geom_richtext(aes(x = yarkon_arrow[1] -0.003, y = yarkon_arrow[2] - 0.0155, label = "The *Yarkon*, bordering<br> 15 other neighbourhoods", hjust = 0, vjust = 1),label.color = NA,   fill = NA, size = 6, family = "Mukta", color = "gray35")+
  labs(title = "Bordering Neighbourhoods in Tel-Aviv",
       subtitle = "Number of neighbourhoods each neighbourhood borders in Tel-Aviv",
       caption = "@Amit_Levinson"
       )+
  scale_fill_viridis(name = "# borders",option="magma")+
  guides(fill = guide_colorbar(barheight = unit(2.2, units = "mm"),
                               barwidth = unit(45, units = "mm"),
                               ticks.colour = "gray25",
                               title.position = "top",
                               label.position = "bottom",
                               title.hjust = 0.5))+
  theme_void()+
  theme(
    text = element_text(family = "Mukta"),
    plot.title = element_text(size =  24, face = "bold", family = "Lora"),
    plot.subtitle = element_text(size =  19),
    plot.caption = element_text(size = 14, color = "gray35", hjust = 1),
    plot.title.position = "plot",
    legend.position = "bottom",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(4,2,4,2,"mm")
  )

ggsave("2021/22_borders/borders.png", height = 11, width = 9)
