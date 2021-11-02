library(sf)
library(janitor)
library(ggplot2)
library(units)
library(ggvoronoi)
library(dplyr)
library(ggtext)

# Get Israel map, unnest to coordinates as polygons.
isrfull <- read_sf("data/maps/isr-full/israel_borders.shp")
isrfull <- st_transform(isrfull, 4326)
isr_points <- as.data.frame(st_coordinates(isrfull))

# Similarly with the train station data
dat <- read_sf("2021/data/rail_stat_onoff_month/RAIL_STAT_ONOFF_MONTH.shp") %>% 
  clean_names()  

dat <- st_transform(dat, 4326)

dat <- dat %>% 
  st_coordinates(geometry) %>% 
  as.data.frame()

# Create Voronoi data
isr_voronoi <- voronoi_polygon(data=dat,
                                x="X",y="Y",
                                outline=isr_points)

# Converts output of voronoi_polygon to be plotted, but we'll move it to sfc_polygon for area calculation
isr_voronoi <- fortify_voronoi(isr_voronoi)


isr_voronoi_poloygon <- isr_voronoi %>% 
  # Convert the coordinats of polygon to a sfc_polygon
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  group_by(id) %>% 
  # collect them together, keep only distinct point for train
  summarise(geometry = st_combine(geometry),
            train.y = max(point.x),
            train.x = max(point.x)) %>%
  st_cast("POLYGON")

# Calculate area each train station comprises
isr_voronoi_poloygon <- isr_voronoi_poloygon %>% 
  st_transform(.,crs = 2039) %>% 
  mutate(area = st_area(geometry),
         area = as.numeric(set_units(area, km^2))) %>% 
  st_transform(crs = 4326)


ggplot(isr_voronoi_poloygon) +
  geom_sf(aes(fill = area), color = NA) +
  geom_point(data = dat, aes(x = X, y = Y), inherit.aes = FALSE, size = 0.05, alpha = 0.7)+
  # Ignore the limits; played a little with adding white spice but removed for now
  coord_sf(xlim = c(min(isr_voronoi$x), max(isr_voronoi$x)))+
  scale_fill_gradientn("Area (km<sup>2</sup>)", 
                                   colors=viridis::plasma(10), trans = "log", breaks = c(10,100,1000,8000), labels = scales::comma)+
  labs(title = "Nearest Train Station",
       subtitle = "The nearest train station for land areas in Israel.\nAll locations in an area are closer to that train\nstation (dot) than to any other",
       caption = "Data: Ministry of Transport | @Amit_Levinson")+
  theme_minimal()+
  theme(
    text = element_text(family = "Raleway"),
    plot.title = element_text(size = 18, "Playfair Display"),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_markdown(size = 10),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "gray15"),
    legend.text = element_text(size = 8),
    plot.margin = margin(6,2,6,2,"mm")
  )

ggsave("2021/03_polygons/trains.png", width = 8, height = 7)
