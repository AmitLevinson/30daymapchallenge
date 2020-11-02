library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)
library(pdftools)

# This gets us the long and lat min and max suitable for Tel-Aviv.
# See link at the bottom of the script for a blog to create your own.
tv_coordinates <- getbb("Tel Aviv Israel")

# Start by getting the lines and point for the main streets
streets <- tv_coordinates %>% 
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("motorway","primary",
                            "secondary", "tertiary")) %>% 
  osmdata_sf()

small_streets <- tv_coordinates %>% 
  opq() %>% 
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "unclassified", "service", "footway")) %>% 
  osmdata_sf()

# PDF output --------------------------------------------------------------

# I deliberately add it to an object since the direct outputting is extremely slow.
# I then render it to pdf and convert that to png which is much faster:
plot_pdf <- ggplot()+
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .6,
          alpha = .8)+
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .6)+
  coord_sf(xlim = c(34.73900 , 34.85400),
           ylim = c(32.01650, 32.14750),
           expand = FALSE)+
  theme_void()+
  geom_text(aes(x= 34.80200, y = 32.02020, label = "Tel-Aviv, IL"),
            color = "grey15", family = "IBM Plex Sans Light", size = 14)+
  labs(caption = "\n#30daymapchallenge | Day: 02\nData: OSM | @Amit_Levinson")+
  theme(
    plot.caption = element_text(family = "IBM Plex Sans Light", size = 11, color = "gray25", face = "italic"),
    plot.caption.position =  "plot",
    plot.margin = unit(c(2,4,2,2), "mm"))

# Used pdf method from Cédric Scherer's tidytuesday outputs
ggsave(plot = plot_pdf, filename = "Code/02_lines/02_lines2.pdf", width = 10, height = 18, device = cairo_pdf)

pdf_convert(pdf = "Code/02_lines/02_lines.pdf", 
            filenames = "Code/02_lines/02_lines.png",
            format = "png", dpi = 400)

#Thanks to Chris' blog post on introducing us the plot:
#https://ggplot2tutor.com/streetmaps/streetmaps/