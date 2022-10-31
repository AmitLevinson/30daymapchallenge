library(readr)
library(sf)
library(dplyr)
library(systemfonts)
library(snapbox)
library(ggplot2)
library(ggsflabel)

rehovot_path <- list.files('../data/maps/judicial/', recursive = T, pattern = 'gv_Rehovot_2016_plt\\.shp$')

# taken from June Choe's blog
fa_solid_path <- system_fonts() %>% 
  filter(family == "Font Awesome 6 Free", style == "Solid") %>% 
  pull(path)

systemfonts::register_font(
  name = "Font Awesome 6 Free Solid",
  plain = fa_solid_path
)

rehovot_sf <- st_read(paste0('../data/maps/judicial/', rehovot_path)) %>% 
  st_transform(crs = 4326) # %>% 
  # remove the holes (combine Gibton)
  # sfheaders::sf_remove_holes()


polling_stations <- read_csv('01_points/polling_station_rehovot.csv') %>%
  filter(lon <= 34.85) %>%
  select(address:lon) %>%
  mutate(is_voting_location = ifelse(lat == 31.89650869, 'Yes', 'no')) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  mutate(label = 'vote-yea')

# Great tip to cutout part of the map, I just used it as a mask:
# https://stackoverflow.com/questions/69126928/hide-area-outside-of-area-of-interest-in-ggspatial
rehovot_mask <- st_buffer(rehovot_sf, units::as_units(5.5, "km")) %>% 
  st_difference(rehovot_sf)

# Plot --------------------------------------------------------------------
p <- ggplot() +
  (layer_mapbox(st_bbox(rehovot_sf, crs = 4326) + c(-0.01,-0.01,0.01, 0.01),
                scale_ratio = 0.6, map_style = mapbox_gallery_frank()))

p +
  geom_sf(data = rehovot_sf, inherit.aes = F,fill = NA, color = 'black', lwd = 1) +
  geom_sf(data = rehovot_mask, inherit.aes = F,fill = 'white', color = NA, alpha = 0.65) +
  geom_sf_text(data = polling_stations, aes(label = label, color = is_voting_location ),
               inherit.aes = F, family = 'Font Awesome 6 Free Solid', size = 3, show.legend = FALSE)+
  scale_color_manual(values = c('Yes' = '#00008b', 'no' = 'gray55'))+
  coord_sf()+
  theme_void() +
  theme(
    panel.background =element_rect(fill = 'white', color = NA),
    plot.background =element_rect(fill = 'white', color = NA)
  ) 
  
ggsave('test.png',width = 13, height = 13, dpi = 500)


img = magick::image_read('test.png')
img_cropped = magick::image_trim(img) # cutout extra white space

# img2 = magick::image_crop(img, '5000x3500+750+1250')

magick::image_write(img_cropped, '01_points/map.png')
