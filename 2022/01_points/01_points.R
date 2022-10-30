library(readr)
library(sf)
library(dplyr)
library(systemfonts)
library(stringr)
library(snapbox)
library(ggplot2)
library(ggsflabel)

rehovot_path <- list.files('../data/maps/judicial/', recursive = T, pattern = 'gv_Rehovot_2016_plt\\.shp$')
list.files('../data/maps/judicial/', recursive = T, pattern = '.givton.\\.shp$')

fa_solid_path <- system_fonts() %>% 
  filter(family == "Font Awesome 6 Free", style == "Solid") %>% 
  pull(path)

systemfonts::register_font(
  name = "Font Awesome 6 Free Solid",
  plain = fa_solid_path
)


rehovot_sf <- st_read(paste0('../data/maps/judicial/', rehovot_path)) %>% 
  st_transform(crs = 4326)



area <- st_bbox(rehovot_sf,
                crs = 4326)

polling_stations <- read_csv('01_points/polling_station_rehovot.csv') %>%
  filter(lon <= 34.85) %>%
  select(address:lon) %>%
  mutate(is_voting_location = ifelse(lat == 31.89650869, 'Yes', 'no')) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  mutate(label = 'vote-yea')


rehovot_mask <- st_buffer(rehovot_sf, units::as_units(2.6, "km")) %>% 
  st_difference(rehovot_sf)
library(ggfx)

p <- ggplot() +
  (layer_mapbox(area, scale_ratio = 0.5, map_style = mapbox_gallery_frank()))

p +
  geom_sf(data = rehovot_sf, inherit.aes = F,fill = NA, color = 'black', lwd = 1) +
  geom_sf(data = rehovot_mask, inherit.aes = F,fill = 'white', color = NA, alpha = 0.65) +
  geom_sf_text(data = polling_stations, aes(label = label, color = is_voting_location ),
               inherit.aes = F, family = 'Font Awesome 6 Free Solid', size = 3, show.legend = FALSE)+
  # coord_sf(xlim = c(34.76999, 34.85),
           # ylim = c(31.86643, 31.92090))+
  scale_color_manual(values = c('Yes' = 'blue', 'no' = 'gray55'))+
  # xlim(34.7, 34.8)+
  coord_sf()+
  theme_void() +
  theme(
    panel.background =element_rect(fill = 'white', color = NA),
    plot.background =element_rect(fill = 'white', color = NA)
  ) 
  
ggsave('test.png',width = 13, height = 13, dpi = 500)


img = magick::image_read('test.png')
img_cropped = magick::image_trim(img)

img2 = magick::image_crop(img, '5000x3500+750+1250')

magick::image_write(img_cropped, '01_points/map.png')

  ggplot(NULL, aes(0, 0)) +
  geom_text(aes(label = "cat"), size = 50, family = "Font Awesome 6 Free Solid")


lsf.str('package:snapbox')


stylebox::mapbox_light()
rehovot_ggmap <- get_map(location = c(34.80958, 31.89546), zoom = 13)

ggmap(rehovot_ggmap)+
  geom_sf(data = rehovot_sf, inherit.aes = F,fill = NA)

get_map

ggmap(rehovot_ggmap)



remotes::install_github("anthonynorth/snapbox")

