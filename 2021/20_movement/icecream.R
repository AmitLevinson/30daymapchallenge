library(sf)
library(readr)
library(sf)
library(dplyr)
library(osrm)
library(stplanr)
library(ggplot2)
library(htmltools)
library(glue)
library(htmlwidgets)
library(webshot)
library(purrr)

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
  # Get only what's in Tel-Aviv
  filter(lengths(st_intersects(., tel_aviv_file)) > 0)

# TSP analysis based on Jerry Shannon's piece:
# https://twitter.com/jerry_shannon/status/1455525819066028037/photo/1

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


tours <- methods %>% map(function(method) {
  solve_TSP(tsp, method)
})

# Find the optimal method (Though also I want it to be intuitive for me)
dotchart(sort(c(sapply(tours, tour_length), optimal = 30)),
          xlab = "tour length", xlim = c(0, 35))

  

# Go with insertion algorithm, specifically farthest, read more here:
# https://cran.r-project.org/web/packages/TSP/vignettes/TSP.pdf
tour <- solve_TSP(tsp, method = "farthest_insertion")

# Order of locations in tour
tour_order <- as.integer(tour)

# get order of addresses
addresses <- golda_sf[tour_order,]


# Build route
golda_route <- map_dfr(seq(nrow(addresses)-1), function (n){
  route(
    from = addresses[n,],
    to = addresses[n + 1,],
    route_fun = osrmRoute,
    returnclass = "sf") %>% 
    mutate(section = n)
})

# Add location labels       
make_label <- function(street ){
  glue("<div style='text-align:right;'>{street}</div>") %>% 
    HTML()}

# Map map
p <- leaflet() %>%
  addProviderTiles("CartoDB.Positron",options = providerTileOptions(minZoom = 12)) %>%
  addPolylines(data = golda_route, weight = 2, color = "#080E46") %>% 
  # Add emoji as points
  addLabelOnlyMarkers(data = golda_sf, label = HTML("&#127846;"),
                      labelOptions = labelOptions(noHide = T, textOnly = T,
                                                  "font-size" = "6px")) %>% 
  # Add a start label
  addLabelOnlyMarkers(data = golda_route[1,], ~fx, ~fy, label = HTML("Start<br>&#8595;"),
            labelOptions = labelOptions(noHide = T, direction = "top", textOnly = TRUE, 
                                        style = list(
                                          "color" = "#080E46",
                                          "text-align" = "center",
                                          "font-size" = "10px",
                                          "font-face" = "bold",
                                          "border-color" = "rgba(0,0,0,0.5)"))) %>% 
addControl(html = paste(tags$h1(HTML("Golda locations<br>in Tel-Aviv 🍦"), style = "color:black; font-family:Rockwell; font-size: 26pt; padding-left: 8px; line-height: 1.2em;"),
                        tags$div(HTML("Fastest route between all<br>Golda ice-cream locations<br>in Tel-Aviv."), style = "color:black; font-family:Segoe UI; font-size: 15pt; padding-left: 8px; margin-top:-20px")), className = "fieldset {
    border: 0;
}",  position = "topleft") %>% 
  addControl(html = tags$div(HTML("Data: Golda &#8226; Amit_Levinson &#8226; <a href=https://github.com/AmitLevinson/30daymapchallenge/blob/main/2021/20_movement/icecream.R>Code</a>"), style = "color:black; font-family:Segoe UI; font-size: 12pt; padding-left: 15px;"), className = "fieldset {border: 0;}", position = "bottomleft")


# Save widget
saveWidget(p, "2021/20_movement/ice-cream.html")
# Save snapshot
webshot("2021/20_movement/ice-cream.html", file = "2021/20_movement/ice-cream_farthest.png", zoom = 3, vwidth = 900, vheight = 700, delay = 2)

