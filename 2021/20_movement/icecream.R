library(sf)
library(readr)
library(sf)
library(dplyr)
library(osrm)
library(stplanr)

Sys.setlocale("LC_ALL", "Hebrew")

# Get Tel Aviv shapefile to extract points for routes
tel_aviv_file_name <- list.files(pattern =".Aviv.+\\.shp$", recursive = TRUE)
tel_aviv_file <- read_sf(paste0(tel_aviv_file_name))

# Provide relevant crs to Tel-Aviv
tel_aviv_file <- st_transform(st_set_crs(tel_aviv_file, 2039), 4326)

# Incomplete data (specific locations for this analysis were added manually)
golda <- readxl::read_xlsx("2021/data/golda_locations.xlsx") %>% 
  mutate(id = 1:nrow(.))

golda_sf <- golda %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE) %>% 
  filter(lengths(st_intersects(., tel_aviv_file)) > 0) %>% 
  mutate(latlong = paste0(lat, "+", long))

distance <- st_distance(golda_sf, golda_sf)
distance <- as.matrix(distance) / 1000

colnames(distance) <- golda_sf$id
rownames(distance) <- golda_sf$id

# covert to distance matrix of upper/lower
distance <- as.dist(distance)



# TSP prob & solving ------------------------------------------------------
tsp <- TSP(distance)

# Playing around with different methods
methods <- c(
  "nearest_insertion",
  "farthest_insertion",
  "cheapest_insertion",
  "arbitrary_insertion",
  "nn",
  "repetitive_nn",
  "two_opt"
)

library(purrr)

tours <- methods %>% map(function(method) {
  solve_TSP(tsp, method)
})

# Go with default of two_opt
tour <- solve_TSP(tsp)

# Order of locations in tour
tour_order <- as.integer(tour)

# get order of addresses
addresses <- golda_sf[tour_order,]


# Build route
golda_route <- map_dfr(c(1:20), function (n){
  route(
    from = addresses[n,],
    to = addresses[n + 1,],
    route_fun = osrmRoute,
    returnclass = "sf") %>% 
    mutate(section = n)
})

library(ggplot2)

ice_cream_icon <- makeIcon("https://upload.wikimedia.org/wikipedia/commons/2/2c/Ice-cream-solid.svg", iconWidth = 8, iconHeight = 12)

make_label <- function(street ){
  glue("<div style='text-align:right;'>{street}</div>") %>% 
    HTML()}
library(htmltools)
library(glue)
# Create the labels


ice_cream_labels <- map(golda_sf$street, make_label)



leaflet() %>%
  # addTiles() %>% 
  addProviderTiles("CartoDB.Positron",options = providerTileOptions(minZoom = 12)) %>%
  addMarkers(data = golda_sf, icon = ~ice_cream_icon, label  = ice_cream_labels) %>%
  addPolylines(data = golda_route, weight = 2, color = "#bc360a") %>% 
  addLabelOnlyMarkers(data = golda_route[1,], ~fx, ~fy, label = HTML("Start<br>&#8595;"),
            labelOptions = labelOptions(noHide = T, direction = "top", textOnly = TRUE,
                                        style = list(
                                          "color" = "red",
                                          "text-align" = "center",
                                          "font-size" = "16px",
                                          "border-color" = "rgba(0,0,0,0.5)"))) %>% 
addControl(html = paste(tags$h1(HTML("Visiting every<br>Golda location in TA"), style = "color:black; font-family:Merriweather; font-size: 26pt; padding-left: 8px; line-height: 1.2em;"),
                        tags$div(HTML("Fastest route between all<br>golda ice-cream locations in<br>Tel-Aviv, IL"), style = "color:black; font-family:Segoe UI; font-size: 15pt; padding-left: 8px; margin-top:-20px")), className = "fieldset {
    border: 0;
}",  position = "topleft") %>% 
  addControl(html = tags$div(HTML("Data: openrouteservice &#8226; Amit_Levinson &#8226; <a href="), style = "color:black; font-family:Segoe UI; font-size: 12pt; padding-left: 15px;"), className = "fieldset {border: 0;}", position = "bottomleft")

# Save widget
saveWidget(p, "2021/10_raster/walk_from_dizengof.html")
# Save snapshot
webshot("2021/10_raster/walk_from_dizengof.html", file = "2021/10_raster/walking.png", zoom = 3, vwidth = 900, vheight = 700)


