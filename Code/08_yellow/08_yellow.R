library(snapbox)
library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(extrafont)

# List available shape files of Israel's municipality
list.files("×’×‘×•×œ×•×ª ×©×™×¤×•×˜", recursive = TRUE, pattern = "\\.shp$")
# Read Be'er sheva
b7 <- read_sf("×’×‘×•×œ×•×ª ×©×™×¤×•×˜/ï¿½ï¿½ï¿½ï¿½ ï¿½ï¿½ï¿½ï¿½/ï¿½ï¿½ ï¿½ï¿½ï¿½ï¿½/ï¿½ï¿½ï¿½ï¿½ï¿½ ï¿½ï¿½ï¿½ï¿½ (Arc View)/gv_beer_sheva3.7.07.shp")


# Read street light data
# data from: https://www.beer-sheva.muni.il/OpenData/Pages/Data.aspx (seems to be broken now)
b7_lights <- read_csv("Data/07_streetlight.csv")

# Convert lights to sf and add title as a row
b7_lights <- b7_lights %>% 
select(lat, lon) %>% 
  mutate(label = "") %>% 
  add_row(lat = 31.31, lon = 34.76, label = "Light posts in\nBe'er Sheva, IL")

# Convert to sf
lights_sf <- b7_lights %>% 
  st_as_sf(coords = c("lon", "lat"), crs=4326)

# Get boundaries of Be'er Sheva
b7_area <- st_bbox(b7)

# Retrieve mapbox, see link below for Jose Maria's code
map_base <- ggplot()+
  layer_mapbox(b7_area,
               mapbox_api_access_token = keyring::key_get("mapbox"))

# Plot
map_base +
  geom_sf(data = filter(lights_sf, label ==""), fill = NA, color = "#ffff00", size = 0.01, alpha = 0.6)+
  geom_sf_text(data = lights_sf, aes(label = label),color = "#ffff00", size = 7, family = "Roboto Condensed", fontface = "italic")+
  theme_void()
  
# Save:
ggsave("Code/08_yellow/08_yellow.png",width = 13.5,height = 17.5,units = c("cm"), dpi = 400,bg =  "transparent")

# Code I followed:
# https://gist.github.com/JoseEchave/672df001307c7f4344418fc4edb999cc