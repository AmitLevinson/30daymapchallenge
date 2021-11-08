library(sf)
library(ggmap)
library(ggtext)

tel_aviv_file_name <- list.files(pattern =".Aviv.+\\.shp$", recursive = TRUE)
tel_aviv_file <- read_sf(paste0(tel_aviv_file_name))

# Provide relevant crs to Tel-Aviv
tel_aviv_file <- st_transform(st_set_crs(tel_aviv_file, 2039), 4326)

wave <- read_sf("2021/data/wave/tsunami 5 meter line.shp") %>% 
  st_transform(., crs = 4326) %>% 
  st_intersection(., tel_aviv_file)

telavivggmap <- get_googlemap(center = "tel aviv, israel", zoom = 13,style=c(feature="all",element="labels",visibility="off"))

ggmap(telavivggmap)+
  geom_sf(data = wave, inherit.aes = FALSE, fill = NA, color = "#1f73b7", size =1.25)+
  labs(title = "Area covered by a<span style='color:#1f73b7;'> 5 meter <b>tzunami wave</span></b> in Tel-Aviv",
       caption = "Data: Tel-Aviv Open Data | Amit_Levinson")+
  theme_void()+
  theme(
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_markdown(hjust = 0.5, size = 18, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.caption = element_text(size = 10, color = "gray15")
  )

ggsave("2021/08_blue/wave.png", width = 7, height = 8)

