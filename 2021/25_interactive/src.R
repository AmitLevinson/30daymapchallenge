library(sf)
library(jsonlite)
library(httr)
library(purrr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(glue)
library(htmlwidgets)
library(webshot)

# Set for Hebrew characters
# Sys.setlocale("LC_ALL", "Hebrew")

# Bus stops from TA api
bus_stopsapiraw <- GET("https://gisn.tel-aviv.gov.il/arcgis/rest/services/IView2/MapServer/956/query?where=1%3D1&outFields=*&f=json") 
bus_apilist <- fromJSON(content(bus_stopsapiraw, "text"), simplifyVector = FALSE)
bus_stops <-   map_dfr(bus_apilist$features, pluck("attributes")) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>% 
  st_transform(crs = 2039) %>% 
  mutate(id = 1:nrow(.))

# Get the API information from Tel-Aviv municipality website for elderly locations
elderly_info <- data.frame(
  apiurl = c(
    "https://gisn.tel-aviv.gov.il/arcgis/rest/services/IView2/MapServer/600/query?where=1%3D1&outFields=*&f=json",
    "https://gisn.tel-aviv.gov.il/arcgis/rest/services/IView2/MapServer/581/query?where=1%3D1&outFields=*&f=json",
    "https://gisn.tel-aviv.gov.il/arcgis/rest/services/IView2/MapServer/855/query?where=1%3D1&outFields=*&f=json"
  ),
  apiname = c("elderly_clubs","geriatric_day_center", "programs_for_elderly" )
)


# Function to extract, clean and transform to sf the api call of elderly program locations
clean_ta_api <- function(apiurl, apiname) {
apiraw <- GET(apiurl) 

apilist <- fromJSON(content(apiraw, "text"), simplifyVector = FALSE)

sf_data <- map_dfr(apilist$features, pluck("geometry")) %>% 
  mutate(
    shem = map_chr(apilist$features, pluck, "attributes", "shem", .default = NA),
    location_name = apiname
  ) %>% 
  st_as_sf(coords = c("x", "y"), crs = 2039)

return(sf_data)
}

# Get the data
elderly <- map2_dfr(elderly_info$apiurl, elderly_info$apiname, clean_ta_api) %>% 
  distinct(shem, location_name, geometry)

# Calculate distances from each elderly center to the nearest bus stop
distances <- st_distance(bus_stops, elderly) %>% 
  as_tibble() 

# Get the minimum distance, i.e. the nearest bus stop
elderly_with_info <- elderly %>% 
  mutate(distance_to_bus = map_dbl(distances, function(x) min(x))) %>% 
  st_transform(crs = 4326)

bus_stops_wgs <- bus_stops %>% 
  st_transform(crs = 4326)

distance_label <- glue("<b>{round(elderly_with_info$distance_to_bus, 1)}m</b>") %>% 
  map(., htmltools::HTML)
  
pal <- colorNumeric(
  palette = RColorBrewer::brewer.pal(9, "Blues"),
  domain = elderly_with_info$distance_to_bus)

open_labels <-  elderly_with_info %>% 
  arrange(-distance_to_bus) %>% 
  slice(c(1,3)) %>% 
  mutate(distance_label_open = glue("<b>{round(distance_to_bus, 1)}m</b>"),
         distance_label_open = lapply(distance_label_open, HTML))


p <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron",options = providerTileOptions(minZoom = 12)) %>%
  addCircles(data = bus_stops_wgs,  weight =1, color = "#696969", radius = 1, opacity = 0.5) %>% 
  addCircleMarkers(data = elderly_with_info, weight = 4, radius = 2, color = ~pal(distance_to_bus), fillColor = ~pal(distance_to_bus), label = distance_label,  opacity = 1, labelOptions = labelOptions(
                                               style = list(
                                                 "font-size" = "14px",
                                                 "border-color" = "rgba(0,0,0,0.5)"))) %>% 
  addLabelOnlyMarkers(data = open_labels, label = open_labels$distance_label_open,
                      labelOptions = labelOptions(noHide = T, direction = "top",  
                                                  style = list(
                                                    "font-size" = "11px",
                                                    "border-color" = "rgba(0,0,0,0.5)"))) %>% 
  addControl(html = paste(tags$h1(HTML("Distance from elderly<br>program locations to<br>the nearest bus stop"), style = "color:black; font-family:Alegreya; font-size: 24pt; padding-left: 8px; line-height: 1.1em;")), className = "fieldset {
    border: 0;
}",  position = "topleft") %>%  
  addLegend(
  title = "Distance (m)",
  data = elderly_with_info, 
  position = "bottomright",
  pal = pal, values = ~ elderly_with_info$distance_to_bus,
  bins = c(100,200,300))


# Save widget
saveWidget(p, "2021/25_interactive/elderly.html")
# Save snapshot
webshot("2021/25_interactive/elderly.html", file = "2021/25_interactive/elderly.png", vwidth = 700, vheight = 800, delay = 2)
