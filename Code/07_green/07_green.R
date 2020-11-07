library(readr)
library(dplyr)
library(ggplot2)
library(osmdata)
library(sf)
library(pdftools)
library(ggtext)
library(extrafont)


sf_trees <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

# Get coordinates
sf_coordinates <- getbb("San Francisco USA")

# Get data from osm
sf_roads_raw <- sf_coordinates %>%
  opq() %>%
  add_osm_feature("highway") %>%
  osmdata_sf()

label_df <- data.frame(
  x = -122.44,
  y = 37.82,
  label = "<span style='color:#919c4c'>**Trees**</span><span style='color:grey35'> of San Francisco</span>")

plot_pdf <- ggplot()+
  geom_sf(data = sf_roads_raw$osm_lines,
          color = "grey35",
          inherit.aes = FALSE,
          size = .6,
          alpha = .4)+
  geom_point(data = sf_trees, aes(x = longitude, y = latitude), color = "#919c4c", size = 0.05)+
  coord_sf(xlim  = c(-122.51490, -122.35698),
           ylim = c(37.70809, 37.83240),
           expand = FALSE)+
  geom_richtext(data = label_df,aes(x =x , y = y, label = label), inherit.aes = FALSE, fill = NA, label.color =NA, label.padding = unit(c(0,0,0,0), "pt"), size = 24, family = "Kaushan Script")+
  geom_text(aes(x = -122.358, y = 37.711, label = "#30DayMapChallenge Day 07\nSource: DataSF | @Amit_Levinson"), size = 6, hjust = 1, family = "Kaushan Script", color = "grey25")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "#f8eed1"),
    panel.border = element_blank(),
    plot.title = element_markdown(size = 32),
    plot.margin = unit(c(4,4,4,4), "mm")
  )

# See Cédric Scherer on saving as pdf:
# https://github.com/Z3tt/TidyTuesday/blob/master/R/2020_05_TreesSF.Rmd
ggsave(plot = plot_pdf, filename = "Code/07_green/07_green.pdf", width = 19, height = 18.94, device = cairo_pdf)

pdf_convert(pdf = "Code/07_green/07_green.pdf", 
            filenames = "Code/07_green/07_green.png",
            format = "png")
