library(stars)
library(sf)
library(htmltools)
library(leaflet)
library(raster)
# remotes::install_github("GIScience/openrouteservice-r")
library(openrouteservice)
library(htmlwidgets)
library(webshot)

ta_iso <- ors_isochrones(locations = data.frame(x= 34.774225,y  = 32.077915), 
                         profile = "foot-walking", 
                         range = 1800, 
                         interval = 300, 
                         output = "sf") 
  
ta_iso_min <- ta_iso %>% 
  mutate(value = value / 60)


# rasterize
ta_raster <- raster(nrow = 270 , ncols = 450, ext = extent(ta_iso_min[,1:2]))
ta_raster <- rasterize(ta_iso_min[,1:2], ta_raster, field = "value", fun = min)

# Add crs to raster
crs(ta_raster) <- CRS("+init=epsg:4326")

bins <- seq(0,30,5)

# Create color scale
s <- colorRampPalette(c( "#FFFFCC", "#41B6C4"))
palcolors <- s(6)

pal <- colorBin(palcolors, domain = ta_raster, bins = seq(0,30,5), na.color = NA)


p <- leaflet() %>% 
  addTiles() %>% 
  addRasterImage(ta_raster, colors = palcolors, opacity = 0.8) %>% 
  addLegend(pal = pal, values = values(ta_raster), title = "Minutes walking", position = "bottomright", opacity = 0.8) %>% 
  addControl(html = paste(tags$h1(HTML("Walking from<br>Dizengoff square"), style = "color:black; font-family:Merriweather; font-size: 26pt; padding-left: 8px; line-height: 1.2em;"),
                          tags$div(HTML("How far can you reach when<br>walking away from Dizengoff<br>square, Tel-Aviv, IL."), style = "color:black; font-family:Segoe UI; font-size: 15pt; padding-left: 8px; margin-top:-20px")), className = "fieldset {
    border: 0;
}",  position = "topleft") %>% 
  addControl(html = tags$div(HTML("Data: openrouteservice &#8226; Amit_Levinson"), style = "color:black; font-family:Segoe UI; font-size: 12pt; padding-left: 15px;"), className = "fieldset {border: 0;}", position = "bottomleft")

# Save widget
saveWidget(p, "2021/10_raster/walk_from_dizengof.html")
# Save snapshot
webshot("2021/10_raster/walk_from_dizengof.html", file = "2021/10_raster/walking.png", zoom = 3, vwidth = 900, vheight = 700)
