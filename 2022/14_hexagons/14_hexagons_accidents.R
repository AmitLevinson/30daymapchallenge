library(sf)
library(dplyr)
library(ggplot2)
library(readr)

rehovot_path <- list.files('data/maps/judicial/', recursive = T, pattern = 'gv_Rehovot_2016_plt\\.shp$')
rehovot_sf <- st_read(paste0('data/maps/judicial/', rehovot_path)) %>% 
  sfheaders::sf_remove_holes()

grid_square <- st_make_grid(rehovot_sf, cellsize = 500,square = FALSE)

rehovot_grid <- grid_square[st_intersects(grid_square, rehovot_sf, sparse = F) %>% 
                              as.vector()]  %>% 
  st_transform(crs = 4326) %>%
  st_as_sf() %>% 
  mutate(id = 1:nrow(.)) %>% 
  rename(geometry = x)

loc <- read_csv('-2019.csv')

loc_clean <- loc %>% 
  filter(!is.na(X) | !is.na(Y)) %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 2039) 

rehovot_accidents <- loc_clean[st_within(loc_clean, rehovot_sf, sparse = F),] %>% 
  st_transform(crs = 4326)

hexagons_with_accidents <-  rehovot_accidents %>% 
  mutate(accident = 1) %>% 
  st_join(x = rehovot_grid, y = .) %>% 
  select(id, geometry, accident) %>% 
  group_by(id) %>% 
  summarise(total = sum(accident, na.rm = T)) %>% 
  mutate(total = ifelse(total == 0, NA, total))

library(ggtext)
ggplot(hexagons_with_accidents) +
  geom_sf(aes(fill = total), color = 'gray45', lwd = 0.25) +
  scale_fill_gradient(name = '# Accidents', low = '#000000', high = '#FF0000',  na.value = 'gray85', 
                      breaks = seq(2,12,4))+
  guides(fill = guide_colorbar(title.position="top", title.hjust = 0.5))+
  labs(title = 'Car Accidents in Rehovot, IL',
       subtitle = 'Number of car accidents per 500m<sup>2</sup> that occurred in 2019',
       caption = 'Data: gov.il &#x2022; Amit Grinson')+
  theme_minimal()+
  theme(
    text = element_text(family = "Public Sans"),
    plot.title = element_text(size = 28,  face = 'bold'),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 18),
    plot.caption = element_markdown(hjust = 1, size = 12, color = "gray35"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10),
    legend.position = 'bottom',
    legend.key.height = unit(4, 'mm'),
    panel.grid = element_blank(),
    plot.margin = margin(6,2,6,2,"mm"),
    plot.background = element_rect(fill = 'white', color = NA),
    panel.background = element_rect(fill = 'white', color = NA)
  )

ggsave('2022/14_hexagons/accidents.png', width = 11, height = 8)

