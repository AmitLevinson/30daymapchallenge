library(sf)
library(dplyr)
library(ggplot2)

# Get Tel Aviv shapefile to extract points for routes
tel_aviv_file_name <- list.files(pattern =".Aviv.+\\.shp$", recursive = TRUE)
tel_aviv_file <- read_sf(paste0(tel_aviv_file_name))

# Provide relevant crs to Tel-Aviv
tel_aviv_file <- st_transform(st_set_crs(tel_aviv_file, 2039), 4326)

biketrail <- read_sf("2021/data/biketrails/Bicycle Routes.shp")


# Calculate length of routes ------------------------------------------------------------

trail_length <- st_transform(biketrail, 2039)

trail_length %>% 
  filter(!is.na(direction)) %>% 
  st_length() %>% 
  sum()

# plot
ggplot(tel_aviv_file)+
  geom_sf(fill = "white", color = "black", size = 0.25)+
  geom_sf(data = biketrail, inherit.aes = FALSE, color = '#50C878')+
  theme_void()+
  coord_sf()+
  labs(title = "Bike Lanes in Tel-Aviv",
       caption = "Data: Tel-Aviv Open Data | Amit_Levinson")+
  theme(
    text = element_text(family = 'Playfair Display'),
    plot.title = element_text(hjust = 0.5, family = 'Playfair Display', face="bold", size = 19),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "gray20"),
    plot.background = element_rect(fill = '#E9E9E9', color = NA),
    panel.background = element_rect(fill = '#E9E9E9', color = NA),
    plot.margin = margin(4,2,2,4,"mm")
  )

ggsave("2021/07_green/bikelanes.png", width = 5, height = 6)
