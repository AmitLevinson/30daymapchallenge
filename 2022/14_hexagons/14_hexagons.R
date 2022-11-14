library(sf)
library(dplyr)
library(ggplot2)
library(googleway)
library(purrr)
library(tidyr)

rehovot_path <- list.files('data/maps/judicial/', recursive = T, pattern = 'gv_Rehovot_2016_plt\\.shp$')

rehovot_sf <- st_read(paste0('data/maps/judicial/', rehovot_path)) %>% 
  # st_transform(crs = 4326) %>% 
  sfheaders::sf_remove_holes()

home <- st_geometry(st_point(c(34.809366, 31.898025))) %>% 
  st_set_crs(4326)

grid_square <- st_make_grid(rehovot_sf, cellsize = 250,square = FALSE)


rehovot_grid <- grid_square[st_intersects(grid_square, rehovot_sf, sparse = F) %>% 
                              as.vector()]  %>% 
  st_transform(crs = 4326) %>% 
  st_as_sf()

hexagons_to_measure <- rehovot_grid[!st_within(home, rehovot_grid, sparse = F) %>% as.vector(),]



hexagons_centroids <-  rehovot_grid %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  select(lat = Y, long = X) 

get_distance_info <- function(lat, long, way) {
  Sys.sleep(1)
  travel_info <- googleway::google_distance(origins = c(31.89803, 34.80937),
                             destinations =  c(lat, long),
                             mode = way,
                             key = keyring::key_get('google-key'))

  dat <- travel_info$rows$elements[[1]] %>% 
    as_tibble() %>% 
    unnest() %>% 
    mutate(mode = way,
           address = travel_info$destination_addresses,
           lat = lat,
           long = long)

  return(dat)
}
# 
# get_distance_info(31.8975, 34.76228, 'transit')


safe_info <- safely(get_distance_info)

# queried 21:00
# driving_info <- pmap_dfr(list(hexagons_centroids$lat, hexagons_centroids$long, 'driving'), safe_info)
# saveRDS(driving_info, '2022/14_hexagons/driving.rds')
driving <- readRDS('2022/14_hexagons/driving.rds')
?google_distance

# biking_info <- pmap_dfr(list(hexagons_centroids$lat, hexagons_centroids$long, 'bicycling'), safe_info)
# saveRDS(biking_info, '2022/14_hexagons/biking.rds')
biking <- readRDS('2022/14_hexagons/biking.rds')

hexagons_distance <- rbind(biking, driving) %>% 
  rename(travel_seconds = value1, travel_m = value) %>% 
  # group_by(lat,long) %>% 
  pivot_wider(id_cols = c('long', 'lat'), 
              values_from = c(travel_seconds,travel_m),names_from = 'mode') %>% 
  mutate(ratio = travel_seconds_bicycling - travel_seconds_driving) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_join(x=  hexagons_to_measure, y = .)

limit <- max(abs(hexagons_distance$ratio)) * c(-1, 1)

ggplot(hexagons_distance) +
  geom_sf(aes(fill = ratio))+
  scale_fill_distiller(palette = 'RdBu', limit =limit )

hexagons_distance %>% 
  arrange(ratio)

pal <- colorNumeric('RdBu',hexagons_distance$ratio)

library(leaflet)
leaflet(hexagons_distance) %>% 
  addTiles() %>% 
  addPolygons(stroke = FALSE, fillOpacity = 0.8,
              fillColor = pal(hexagons_distance$ratio)) %>% 
  addLegend(pal = pal, values = ~ratio)


?colorNumeric
ggplot(aes(fill = ratio)) +
  geom_sf(color = NA)

  rehovot_grid %>% 
    st_as_sf

  rbind(biking, driving) %>% 
    count(address, sort = T)
    rename(travel_seconds = value1, travel_m = value) %>% 
    st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
    
st_as_sf(coordinates = c('$long', '$lat'))

driving_info$result$duration %>% as_tibble()

?google_distance

keyring::key_set('google-key')

test$rows$elements %>% 
  as.data.frame()
  


ggmap::has_google_key()
test <- route(from = home, 
                to = hexagons_centroids[1:3], mode = 'bicycling')





grid_centroid <- tibble(
  grid = st_geometry(rehovot_grid),
  centroid = rehovot_grid %>% st_centroid()
) 


tes <- rbind(home, hexagons_centroids %>% head(1)) %>% 
  as.data.frame() %>% 
  st_as_sf(crs = 4326)

routes <- osrm::osrmTrip(
                loc = tes,
                osrm.profile = 'bike')

install.packages('googleway')

bike_route <- osrm::osrmTable(src = home, dst = hexagons_dist %>% filter(is.na(duration)) %>% st_centroid(),
                osrm.profile = 'bike' )


test_plot <- data.frame(st_geometry(hexagons_to_measure)) %>% 
  mutate(
    duration = bike_route$durations %>% as.vector()
  ) %>% 
  st_as_sf()

hexagons_dist %>% filter(is.na(duration)) %>% st_centroid() %>% 
  ggplot() +
  geom_sf()

dat <- bike_route$destinations %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
  mutate(duration = bike_route$durations %>% as.vector())

hexagons_dist <- st_join(hexagons_to_measure %>% st_as_sf(), dat)
hexagons_dist%>% 
  ggplot() +
  geom_sf(aes(fill = duration))


class(dat)
class()
?st_coordinates()
st_
bike_route$destinations

plot(hexagons_to_measure)
test_plot

ggplot(test_plot) +
  geom_sf(aes(color = duration))

plot(test_plot)

ggplot()

?osrm::osrmTable
routes[[1]][[1]]

routes
plot(routes)

ggplot()+
  geom_sf(hexagons_to_measure,mapping = aes(), fill ='red') 
  geom_sf(home, inherit.aes = F, mapping = aes())

  pharmacy <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"), 
                      quiet = TRUE)
  travel_time <- osrmTable(loc = pharmacy)
  travel_time$durations[1:5,1:5]

