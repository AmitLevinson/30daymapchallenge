library(ggplot2)
library(sf)
library(raster)
library(dplyr)
library(patchwork)

tel_aviv_file_name <- list.files(path = "~/isr_maps", pattern =".Aviv.+\\.shp$", recursive = TRUE)
# I use the file for different projects and reference it several times from a specific folder
tel_aviv_file <- read_sf(paste0("C:/Users/amitl/R_code/isr_maps/", tel_aviv_file_name))

# Convert to raster
r <- raster(tel_aviv_file,res =10)
ras_ta <- rasterize(tel_aviv_file, r, field = 1)


draw_plot <- function(ras_ta, direction) {


# Turn raster object to data.frame
ta_tiles <- ras_ta %>% 
  as.data.frame(xy = TRUE) %>%
  filter(!is.na(layer)) %>% 
  arrange({{direction}}) %>% 
  # Here we split the x-y coordinates according to % of each (non/)vaccinated group
  mutate(id = 1:nrow(.)) %>% 
  mutate(point_to_labels = as.numeric(cut(id, breaks = quantile(id,probs = 0:10/10), labels = seq(10, 100, 10), include.lowest = TRUE)),
         point_to_labels = point_to_labels * 10,
         fillcolor = ifelse( (point_to_labels / 10) %% 2 == 1, "#5E7893", "#2F4169"))


p <- ggplot(ta_tiles)+
  # The tiles that fill the map
  geom_sf(data = tel_aviv_file, inherit.aes = FALSE, fill = NA, color = 'gray55')+
  geom_tile(aes(x = x,y = y,fill = fillcolor, color = fillcolor), size =.3, show.legend = FALSE, alpha = 0.3)+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "white", color =NA)
  )+
  scale_fill_identity() +
  scale_color_identity()

return (p)
}

p_hor <- draw_plot(ras_ta, direction = y)
p_ver <- draw_plot(ras_ta, direction = x)

p_hor + p_ver+
plot_annotation(
  title = "Tel Aviv Split to Ten Equal Areas\n",
  caption = "@Amit_Levinson",
  theme = theme(
    text = element_text(family = "Lora"),
    plot.title = element_text(size = 24),
    plot.caption = element_text(size = 11, color = "gray25", hjust = 0.5),
    plot.margin = margin(5,3,5,3,"mm")
  ))

ggsave("2021/17_land/ta_area.png", width= 13, height = 8)
