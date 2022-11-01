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

rehovot_path <- list.files('../data/maps/judicial/', recursive = T, pattern = 'gv_Rehovot_2016_plt\\.shp$')

rehovot_sf <- st_read(paste0('../data/maps/judicial/', rehovot_path)) %>% 
  st_transform(crs = 4326) %>%
  # remove the holes (combine Gibton)
  sfheaders::sf_remove_holes()


routes <- route(from = st_coordinates(st_sample(rehovot_sf, size = 1000)),
                to = st_coordinates(st_sample(rehovot_sf, size = 1000)),
                route_fun = osrmRoute,
                returnclass = "sf")

routes["count"] <- 1

overlapping_segments <- overline(routes, attrib = "count")

library(stplanr)

p <- leaflet(overlapping_segments) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addPolylines(weight = overlapping_segments$count / 6, color = "white") %>% 
  addControl(html = paste(tags$h1(HTML("Major routes <br>in Rehovot, Israel"), style = "color:white; font-family:Merriweather; font-size: 28pt; padding-left: 12px; line-height: 1.2em;"),
                          tags$div(HTML("Most commonly traveled roads<br>from a sample of 1000 routes."), style = "color:white; font-family:Segoe UI; font-size: 15pt; padding-left: 15px; margin-top:-20px"))
             , className = "fieldset {
    border: 0;
}",  position = "topleft") %>% 
  addControl(html = tags$div(HTML("Adapted from Alexandra Kapp &#8226; Data: {stplanr} package &#8226; Amit Levinson"), style = "color:#BEBEBE; font-family:Segoe UI; font-size: 10pt; padding-left: 15px;"), className = "fieldset {border: 0;}", position = "bottomleft")

# Saving a snapshot
saveWidget(p, "02_lines/lines.html")
webshot("02_lines/lines.html", file = "02_lines/lines.png", zoom = 3, vwidth = 900, vheight = 700)
