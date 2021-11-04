library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(ggtext)
library(data.table)

# Get Israel map, unnest to coordinates as polygons.
isrfull <- read_sf("data/maps/isr-full/israel_borders.shp") %>% 
  st_transform(crs = 2039)

# read data for Novemeber 3
dat <- fread("https://openmobilitydata-data.s3-us-west-1.amazonaws.com/public/feeds/ministry-of-transport-and-road-safety/820/20211103/original/stops.txt")

dat_clean <- dat %>% 
  distinct(stop_lon, stop_lat, stop_id)

# filter to dsitinct stop_id (which it should be)
datsf <- dat_clean %>% 
  select(stop_lon, stop_lat,stop_id, stop_id) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = 2039)

# Make grids
isrfull_grid <- st_make_grid(isrfull,  cellsize = 5000 , square = FALSE)
# Get only intersecting grids
grid_rds <- st_as_sf(st_intersection(isrfull_grid, isrfull))

# Add a unique id
grid_rds <- st_as_sf(grid_rds) %>% 
  mutate(id = 1:nrow(.))


# Join grid and stops
gridpoint_joined <- st_join(grid_rds, datsf, left = TRUE) %>% 
  mutate(stops = ifelse(is.na(stop_id), 0,1)) 

# for some reason I can't aggregate in the geometry object level, so use id instead:  
stops_per_grid <- gridpoint_joined %>%
  st_drop_geometry() %>% 
  group_by(id) %>% 
  summarise(stops = sum(stops)) %>% 
  mutate(stops = na_if(stops, 0))

# Join back to grid object
full_grid_stops <- grid_rds %>% 
  left_join(stops_per_grid) %>% 
  st_transform(crs = 4326)


ggplot(full_grid_stops)+
  geom_sf(aes(fill = stops), size = 0.1, color = "gray15")+
  scale_fill_viridis(name = "Bus stops", trans = "log", breaks = c(1,10,100,500))+
  labs(title = "Bus stops per 5km<sup>^2</sup>",
       caption = "Data: OpenMobilityData (11/3/2021) | Visualization: Amit_Levinson")+
  coord_sf(xlim = c(33.5,36))+
  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    text = element_text(family = "Roboto"),
    plot.title = element_markdown(size = 18, "Merriweather", hjust = 1, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_markdown(size = 10, color = "gray15"),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "gray35"),
    legend.text = element_text(size = 8, hjust = 0, color = "gray15"),
    legend.position=c(0.3,0.75),
    legend.background = element_blank(),
    plot.margin = margin(6,2,6,2,"mm")
  )

ggsave("2021/04_hexagons/stops.png", width= 8, height= 7)
