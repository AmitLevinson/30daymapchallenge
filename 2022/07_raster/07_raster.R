# https://rspatialdata.github.io/elevation.html

library(elevatr)
library(sf)
library(raster)
library(ggplot2)
library(ggtext)

# Get Israel map for extracting raster layer below, you can also use the {rgeoboundaries} instead
# to get boundaries of a country you're interested in.
isr_map <- readRDS(paste0('../data/maps/00_Israel_0_sf.rds'))


# Get elevation data
isrraster <- get_elev_raster(locations = isr_map, z = 10, clip = "locations")

elevation_data <- as.data.frame(isrraster, xy = TRUE)
colnames(elevation_data)[3] <- "elevation"
elevation_data <- elevation_data[complete.cases(elevation_data), ]

ggplot() +
  geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) +
  coord_sf() +
  scale_fill_viridis_c(name = "Elevation (meters)" ,option = 'E', labels = scales::comma) +
  labs(title = "Elevation in Israel", 
       x = "Longitude",
       y = "Latitude",
       caption = 'Data: {elvatar} package & AWS Tiles &#8226; Visualization: Amit Grinson') +
  theme(
    text = element_text(family = "Quicksand", lineheight = 1),
    plot.title = element_text(size = 28, family = "Roboto Slab", face = 'bold', hjust = 1),
    plot.subtitle = element_text(size = 18, lineheight = 1.1),
    plot.caption = element_markdown(hjust = 0, size = 12, color = "gray25"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    # legend.position = 'horizontal',
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    plot.margin = margin(6,2,6,2,"mm"),
    axis.ticks = element_blank()
  )

ggsave('07_raster/elevation_high_res.png', width = 7, height = 11)
