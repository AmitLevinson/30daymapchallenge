library(ggplot2)
library(dplyr)
library(sf)
library(ggrepel)
library(jsonlite)
library(httr)
library(extrafont)
library(ggtext)

# Polygon data from https://gadm.org/download_country_v3.html
shp <- readRDS("Data/00_Israel_0_sf.rds")

# GET data from data.gov.il
hotels <- GET("https://data.gov.il/api/3/action/datastore_search?resource_id=073b0370-dd72-4fbb-ab0e-bc2f8e85699f&limit=400")
hotels <- fromJSON(rawToChar(hotels$content))
hotels <- hotels$result$records

# Plot
ggplot()+
  geom_sf(data = shp, size = 1, color = "black", fill = "grey95")+
  geom_point(data = hotels, aes(x = X, y = Y, label = Name), color = "#8A2BE2")+
  theme_void()+
  labs(title = "<span style='color:#8A2BE2'>Hotels</span> in Israel", subtitle = "Labeled hotels are over the the green line",
       caption = "#30DayMapChallenge | Day: 03\nData: data.gov.il | @Amit_Levinson")+
  geom_text_repel(data = filter(hotels, Name %in% c("Kalya", "Almog")), mapping = aes(x = X, y= Y, label = Name), point.padding = 0.4 ,box.padding = 1, family = "IBM Plex Sans", color = "gray15")+
  xlim(c(34, 36.5))+
theme(text = element_text(family = "IBM Plex Sans"),
      plot.title = element_markdown(hjust = 0, size = 36, family = "IBM Plex Sans Medium"),
      plot.subtitle = element_text(size = 15),
      plot.caption = element_text(size = 10,family = "IBM Plex Sans Light", hjust = 0, color = "gray15"),
      plot.margin = unit(c(4,4,4,4), "mm"))

ggsave(filename = "Code/03_polygons/03_polygons.png", height = 10, width = 7)
