library(httr)
library(jsonlite)
library(tidyverse)
library(sf)
library(lubridate)
library(ggvoronoi)
library(units)
library(ggtext)
library(snapbox)
library(ggsflabel)

# Load Solid FA font
fa_solid_path <- systemfonts::system_fonts() %>% 
  filter(family == "Font Awesome 6 Free", style == "Solid") %>% 
  pull(path)

systemfonts::register_font(
  name = "Font Awesome 6 Free Solid",
  plain = fa_solid_path
)


# GTFS stops in Rehovot
# res = GET("https://open-bus-stride-api.hasadna.org.il/gtfs_stops/list",
#           query = list(
#             # route_short_name = 14,
#             limit = 20000, date_from = '2022-10-16', date_to = '2022-10-28', city = 'רחובות'))


# stops_in_rehovot = fromJSON(rawToChar(res$content))
# saveRDS(stops_in_rehovot, '03_polygons/stops_in_rehovot.rds')

stops_in_rehovot = readRDS('03_polygons/stops_in_rehovot.rds')

train_stops <- stops_in_rehovot %>% 
  filter(str_detect(name, 'רכבת')) %>% 
  distinct(id) %>% 
  pull(id) %>% 
  paste0(collapse = ',')


rides_passing_through_stop <- function(stop_id) {
  # Take from stops ride stops, info about specific rides:
  res = GET("https://open-bus-stride-api.hasadna.org.il/gtfs_ride_stops/list",
            query = list(
              # route_short_name = 14,
              limit = 10000,gtfs_stop_ids = stop_id))
  
  rides_from_stop = fromJSON(rawToChar(res$content))
  return(rides_from_stop)
}


# Pass in stop and get all rides passing through there
sample_of_rides_for_this_stop <- map_dfr(train_stops, rides_passing_through_stop) %>%
  distinct(shape_dist_traveled, .keep_all = T) %>%
  pull(gtfs_ride_id)



get_ride_info <- function (rides) {
  
  stops_in_ride <- function (ride_id) {
    # Sys.sleep(5)
    res = GET("https://open-bus-stride-api.hasadna.org.il/gtfs_ride_stops/list",
              query = list(
                limit = 1000,
                gtfs_ride_ids = ride_id))
    busroute = fromJSON(rawToChar(res$content))
    return(busroute)
  }
  
  busroute <- stops_in_ride(rides)
  
  get_stop_name <- function(stop_id) {
    res = GET("https://open-bus-stride-api.hasadna.org.il/gtfs_stops/get",
              query = list(
                id = stop_id)
              )
    return(data.frame(fromJSON(rawToChar(res$content))))
  }
  
  stops_info <- map_dfr(busroute$gtfs_stop_id, get_stop_name)
  busroutes_complete <- busroute %>% 
    left_join(stops_info, by = c('gtfs_stop_id' = 'id'))
  
  return(busroutes_complete)
}


# all_rides <- map_dfr(sample_of_rides_for_this_stop, get_ride_info)
# saveRDS(all_rides,file = '03_polygons/all_rides.rds')

all_rides <- readRDS('03_polygons/all_rides.rds')

get_route_id <- function (ride_id) {
  res = GET("https://open-bus-stride-api.hasadna.org.il/gtfs_rides/get",
            query = list(
              id = ride_id)
            )
  route_from_ride = fromJSON(rawToChar(res$content)) 

  return(route_from_ride$gtfs_route_id)
  
}

get_route_info <- function(route_id = NA) {
  res = GET("https://open-bus-stride-api.hasadna.org.il/gtfs_routes/get",
            query = list(
              id = route_id)
            )
  
  route_from_ride = fromJSON(rawToChar(res$content)) %>% 
    do.call(cbind, .) %>% 
    as.data.frame()
  return(route_from_ride)
}


collect_route_from_ride <- function(ride_id) {
  # Sys.sleep(5)
  route_id <-  get_route_id (ride_id)  
  route_info <- get_route_info(route_id)
  return(route_info)
}



# Collect routes and line-refs
ride_ids <- unique(all_rides$gtfs_ride_id)

all_routes <- map_dfr(ride_ids,collect_route_from_ride)

# clean up duplicate stations for plotting.
clean_rides <- all_rides %>% 
  filter(code %in% stops_in_rehovot$code) %>% 
  distinct(lon,lat, .keep_all = T)

bus_stops_sf <- clean_rides %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Rehovot shp
rehovot_path <- list.files('../data/maps/judicial/', recursive = T, pattern = 'gv_Rehovot_2016_plt\\.shp$')
rehovot_sf <- st_read(paste0('../data/maps/judicial/', rehovot_path)) %>% 
  st_transform(crs = 4326)

rehovot_points <- as.data.frame(st_coordinates(rehovot_sf))


# Create Voronoi data
rehovot_voronoi <- voronoi_polygon(data=clean_rides,
                               x="lon",y="lat",
                               outline=rehovot_points)

rehovot_voronoi_poloygon <- rehovot_voronoi %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_set_crs(4326) %>% 
  group_by(id) %>% 
  st_cast("POLYGON")


# Calculate area each nearest bus comprises
rehovot_voronoi_poloygon_km <- rehovot_voronoi_poloygon %>% 
  st_transform(.,crs = 2039) %>% 
  mutate(area = st_area(geometry),
         area = as.numeric(set_units(area, 'km^2'))) %>% 
  st_transform(crs = 4326) %>% 
  # filter extreme areas not near a land
  mutate(area = ifelse(area > 1, NA, area))

train_stop <- bus_stops_sf %>% 
  filter(str_detect(name, 'רכבת')) %>% 
  head(1) %>% 
  mutate(label = 'train')

  ggplot() +
  geom_sf(data = rehovot_sf, fill = NA, lwd  = 0.5, inherit.aes = F)+
  geom_sf(data = rehovot_voronoi_poloygon_km, aes(fill = area), color = NA)+
  geom_sf(data = bus_stops_sf, inherit.aes = FALSE, size = 0.05, alpha = 0.8, color = 'gray85')+
  geom_sf_text(data = train_stop, aes(label = label),
                   inherit.aes = F, family = 'Font Awesome 6 Free Solid', size = 3, show.legend = FALSE, color = 'white')+
  scale_fill_gradientn("Area (km<sup>2</sup>)", 
                       colors=viridis::inferno(10), breaks = c(0.25, 0.5,.75), na.value = 'gray95')+
  labs(title = "One Bus Ride to Rehovot's Train Station",
         subtitle = "Nearest bus stop with a direct route to Rehovot's train station. All locations in an area<br>are closer to that bus stop (dot) than to any other stop. Bus stops were extractred from Israel's GTFS<br>in the past two weeks & sampling routes that reach Rehovot's train station from that stop.<br><span style='color:gray35;'>**Gray**</span> areas are filtered for having mostly open fields.",
       caption = "Data: Israel's GTFS; made accessible by 'Hasadna' | @Amit_Levinson")+
  theme_minimal()+
  theme(
    text = element_text(family = "Noto Sans", lineheight = 1),
    plot.title = element_text(size = 28, "Lora", face = 'bold'),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 16, lineheight = 1.1),
    plot.caption = element_text(hjust = 0.5, size = 12, color = "gray25"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_markdown(size = 10),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    plot.margin = margin(6,2,6,2,"mm")
  )
  
ggsave('03_polygons/bus_stops.png', width = 13, height = 9)


