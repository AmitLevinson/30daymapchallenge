
# Code from Alexandra Kapp day 9: https://alexandrakapp.github.io/30daymapchallenge/index.html
# Check it out^^

library(sf)
library(dplyr)
library(osrm)
library(stplanr)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(webshot)

# Get Tel Aviv shapefile to extract points for routes
tel_aviv_file_name <- list.files(pattern =".Aviv.+\\.shp$", recursive = TRUE)
tel_aviv_file <- read_sf(paste0(tel_aviv_file_name))

# Provide relevant crs to Tel-Aviv
tel_aviv_file <- st_transform(st_set_crs(tel_aviv_file, 2039), 4326)

routes <- route(from = st_coordinates(st_sample(tel_aviv_file, size = 1000)),
                to = st_coordinates(st_sample(tel_aviv_file, size = 1000)),
                route_fun = osrmRoute,
                returnclass = "sf")

routes["count"] <- 1

overlapping_segments <- overline(routes, attrib = "count")


p <- leaflet(overlapping_segments) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addPolylines(weight = overlapping_segments$count / 6, color = "white") %>% 
  addControl(html = paste(tags$h1(HTML("Streets traveled<br>in Tel-Aviv"), style = "color:white; font-family:Merriweather; font-size: 28pt; padding-left: 15px; line-height: 1.2em;"),
                          tags$div(HTML("Most commonly traveled streets from a<br>sample of 1000 routes."), style = "color:white; font-family:Segoe UI; font-size: 10pt; padding-left: 15px; margin-top:-20px"))
                          , className = "fieldset {
    border: 0;
}",  position = "topleft") %>% 
  addControl(html = tags$div(HTML("Adapted from Alexandra Kapp &#8226; Data: {stplanr} package &#8226; Amit Levinson"), style = "color:#BEBEBE; font-family:Segoe UI; font-size: 10pt; padding-left: 15px;"), className = "fieldset {border: 0;}", position = "bottomleft")

# Saving a snapshot
saveWidget(p, "2021/02_lines/lines.html")
webshot("2021/02_lines/lines.html", file = "2021/02_lines/lines.png", zoom = 3, vwidth = 900, vheight = 700)
