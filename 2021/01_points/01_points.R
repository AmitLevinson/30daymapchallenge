library(sf)
library(ggplot2)
library(geojsonio)
library(ggmap)
library(dplyr)
library(stringr)
library(RColorBrewer)

dat <- st_as_sf(geojson_read("https://opendata.arcgis.com/datasets/feec119124874c70994fa13a365bebf9_0.geojson", what = "sp"))

Sys.setlocale("LC_ALL", "Hebrew")


clean_data <- dat %>% 
  mutate(
    # Extract years
    TreeEstimateAge = str_extract(TreeEstimateAge, "\\d+(-)?\\d*"),
    TreeEstimateAge = ifelse(TreeEstimateAge == "60-100", "60", TreeEstimateAge),
    TreeAge = factor(TreeEstimateAge, levels = c(NA, "10-20", "20-40", "40-60", "60", "100"), labels = c("10-20","20-40", "40-60", "60+", "100+"))
  )

telavivggmap <- get_googlemap(center = "tel aviv, israel", zoom = 13,style=c(feature="all",element="labels",visibility="off"))


ggmap(telavivggmap)+
  geom_sf(data = clean_data, aes(color = TreeAge), inherit.aes = FALSE, size = 1.5)+
  scale_color_brewer(palette = "YlOrBr", na.value="grey", name = "Tree age (Years)")+
  guides(color = guide_legend(nrow = 1, byrow = TRUE,override.aes = list(size=3)))+
  labs(title = "Historical and preserved trees across Tel-Aviv",
       subtitle = "",
       caption = "Data: Israel's Ministry of Agriculture | Visualization: @Amit_Levinson")+
  theme(
    text = element_text(family = "Segoe UI"),
    plot.title = element_text(size = 22, family = "Alegreya", hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 8),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.key.size = unit(3,"mm"),
    legend.key.width = unit(5,"mm"),
    legend.title = element_text(size = 11, color = "gray10"),
    legend.text = element_text(size = 10, color = "gray25"),
    plot.caption = element_text(hjust = 0.5, color = "gray45", size = 10),
    plot.margin = margin(6,2,6,2,"mm"),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.key=element_blank()
  )

ggsave("2021/01_points/trees.png",height = 8, width = 7, dpi = 500)
