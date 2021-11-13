
# Just some sf objects and mapping ----------------------------------------

library(sf)
library(ggplot2)

# Pre 67 borders
isr <- readRDS("data/maps/00_israel_0_sf.rds")

# Error requesting to set crs from old version
isr <- st_set_crs(isr, st_crs(isr))

ggplot()+
  geom_sf(data = st_as_sf(isr))

# ggsave("2021/messing-around/israelpre67.png")

# Full map
isrfull <- read_sf("data/maps/isr-full/israel_borders.shp")

ggplot(isrfull)+
  geom_sf()

# ggsave("2021/messing-around/israel.png")

# Tel Aviv
tel_aviv_file_name <- list.files(pattern =".Aviv.+\\.shp$", recursive = TRUE)
# I use the file for different projects and reference it several times from a specific folder
tel_aviv_file <- read_sf(paste0(tel_aviv_file_name))


# So Tel-aviv didn't have any crs, but we can see it's in ITM:
tel_aviv_file <- st_set_crs(tel_aviv_file, 2039)

# Now convert to WGS 84:
tel_aviv_file <- st_transform(tel_aviv_file, 4326)


# tmap attempt ------------------------------------------------------------

library(tmap)

t <- qtm(tel_aviv_file, fill = NA, borders = NULL, title = "Just a quick tmap of Tel-Aviv", style = "cobalt")

tmap_save(t,"2021/messing-around/telaviv_tmap.png")

# ggmap -------------------------------------------------------------------
library(ggmap)

telavivggmap <- get_googlemap(center = "tel aviv, israel", zoom = 12)

ggmap(telavivggmap)

# ggsave("2021/messing-around/ggmap.png")

# Again, this time add an sf object
ggmap(telavivggmap)+
  # Adding sf to ggmap, inherift.aes = FALSE
  geom_sf(data = tel_aviv_file, inherit.aes = FALSE, fill = NA)

# ggsave("2021/messing-around/ggmap_polygon.png")


# And again, this time removing labels
telavivggmapclean <- get_googlemap(center = "tel aviv, israel", zoom = 12, style=c(feature="all",element="labels",visibility="off"))

ggmap(telavivggmapclean)+
  # Adding sf to ggmap, inherift.aes = FALSE
  geom_sf(data = tel_aviv_file, inherit.aes = FALSE, fill = NA)+
  theme_void()

# ggsave("2021/messing-around/ggmap_polygon_clean.png")


# Leaflet -----------------------------------------------------------------


library(leaflet)
library(magrittr)
m <- leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = tel_aviv_file)

# mapview::mapshot(m, file = "2021/messing-around/leaflet.png")


# Osm data ----------------------------------------------------------------
library(osmdata)

opq(st_bbox(tel_aviv_file))

dat <- opq(bbox = st_bbox(tel_aviv_file)) %>% 
  # add_osm_feature(key = 'height') %>% 
  add_osm_feature(key = "busway") %>% 
  osmdata_sf() 

q <- opq(st_bbox(tel_aviv_file)) %>%
  add_osm_feature(key = 'highway') %>% 
  osmdata_sf()

ggplot()+
  # geom_sf(data = st_geometry(q$osm_lines), color = "red")+
  geom_sf(data = st_geometry(q$osm_points), alpha = 0.1)

?overline
library(ggplot2)
plot(dat)
available_features()



# stplanr and osrm --------------------------------------------------------
library(stplanr)
library(osrm)

trip <- route(
  from = st_coordinates(st_sample(tel_aviv_file, size = 1)),
  to = st_coordinates(st_sample(tel_aviv_file, size = 1)),
  route_fun = osrmRoute,
  returnclass = "sf",
  osrm.profile = "car"
)

mapview::mapview(trip, color = "cyan")



# Buffer zone -------------------------------------------------------------

# Dizingof center as a point
# dizcenter <- st_transform(st_sfc(st_point(c(34.774225, 32.077915)), crs = 4326), crs = 2039)

# 1 Km radius around it
# dizcenter1km <- st_transform(st_buffer(dizcenter, dist = units::set_units(2, "km")), crs = 4326)


# st_transform(st_buffer(st_transform(st_as_sf(loc, coords = c("longitude", "latitude"), crs = 4326), 2039), dist = set_units(8, "km")), crs = 4326)

# lines intersecting within our buffer
# datcomplete <- st_intersection(tel_aviv_file, dat)

# So Tel-aviv didn't have any crs, but we can see it's in ITM:
tel_aviv_file <- st_set_crs(tel_aviv_file, 2039)

# Now convert to WGS 84:
tel_aviv_file <- st_transform(tel_aviv_file, 4326)


# Extract streets in Tel-Aviv near the Diz center
# streets <- opq(bbox = st_bbox(dizcenter1km)) %>% 
#   add_osm_feature(key = "highway", value = c("motorway","primary",
#                                              "secondary", "tertiary", "residential", 
#                                              "service")) %>% 
#   osmdata_sf()



# Bus routes --------------------------------------------------------------


# Check out more at 
# https://transitfeeds.com/p/ministry-of-transport-and-road-safety/820

routes
routes <- fread("2021/data/stop_times.txt")
setkey(routes, "stop_id")
setkey(dat, "stop_id")

dat[routes, on = "stop_id", nomatch=0][trip_id == "44092626_041121",.(mean(arrival_time)), by = trip_id]


# elevater ----------------------------------------------------------------




isrraster <- get_elev_raster(locations = isrfull, z = 9, clip = "locations")

elevation_data <- as.data.frame(isrraster, xy = TRUE)


colnames(elevation_data)[3] <- "elevation"
# remove rows of data frame with one or more NA's,using complete.cases
elevation_data <- elevation_data[complete.cases(elevation_data), ]

ggplot() +
  geom_point(data = elevation_data, aes(x = x, y = y, color = elevation)) +
  scale_color_gradient(low = "white", high = "black")+
  coord_sf() +
  theme_void()

