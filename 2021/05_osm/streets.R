library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
junebug::font_hoist("Alegreya") # Hoist Alegreya to enjoy its various weights


# Data collections & setup ------------------------------------------------

# Wrap the street collection info into a function:

large_features <- c("motorway","primary","secondary", "tertiary")
small_features <- c("residential", "living_street", "unclassified", "service", "footway")

get_osm_data <- function (city, features) {
  getbb(city)%>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = features) %>% 
    osmdata_sf()
  
}


jlm_large <- get_osm_data("Jerusalem, IL", features = large_features)
jlm_small <- get_osm_data("Jerusalem, IL", features = small_features)

ny_large <- get_osm_data("Manhattan, NY", features = large_features)
ny_small <- get_osm_data("Manhattan, NY", features = small_features)



# Plot --------------------------------------------------------------------

theme_set(theme_void())


generate_plot <- function(large, small, title, glowsigma, glowexpand) {
  ggplot(data = small$osm_lines)+
    geom_sf(color = "gray15", size = 0.2)+
    with_outer_glow(
      geom_sf(data =large$osm_lines, inherit.aes = FALSE), 
      colour = "yellow",
      sigma = glowsigma,
      expand = glowexpand
    )+

    labs(title = title) +
    theme (
      plot.title = element_text(size = 25, hjust = 0.5, family = "Alegreya Bold"),
      # For some reason I add this to solve FaceBook issues :(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}


# Combine -----------------------------------------------------------------

pny <- generate_plot(ny_large, ny_small, title = "Manhattan", glowsigma = 0.5, glowexpand = 1)
pjlm <- generate_plot(jlm_large, jlm_small, title = "Jerusalem", glowsigma = 0.75, glowexpand = 1.5)

combined <- pny + pjlm +
  plot_annotation(
                  caption = "Data: OSM | Amit_Levinson",
                  theme = theme(
                    plot.caption = element_text(size = 11, family = "Alegreya", color = "gray25", hjust = 0),
                    plot.margin = margin(4,0,4,0,"mm")
                  ))

ggsave("2021/05_osm/street.png" ,plot = combined, width = 13, height = 8)  
