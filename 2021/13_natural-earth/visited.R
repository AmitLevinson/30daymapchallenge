
## Code credited to gist from below and the d6berlin R package!
## Also thanks to Georgis for pointing it out @geokaramanis

library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(patchwork)

# Process -----------------------------------------------------------------

## code to preserve orthpgraphic view from this gist:
## https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1
## via this issue: https://github.com/r-spatial/sf/issues/1050


  ## Load country data
  mini_world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") 
  
  
  ## Define the orthographic projection ........................................
  lat <- 20
  lon <- 40
  
  ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon,
                  ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')
  
  ## Define the polygon to split what lies within and without your projection ..
  circle <-
    suppressMessages(
      sf::st_point(x = c(0, 0)) %>%
        sf::st_buffer(dist = 6371000) %>%
        sf::st_sfc(crs = ortho)
    )
  ## Project this polygon in lat-lon ...........................................
  circle_longlat <-
    circle %>%
    sf::st_transform(crs = 4326)
  
  ## You must decompose it into a string with ordered longitudes
  ## Then complete the polygon definition to cover the hemisphere ..............
  if(lat != 0) {
    circle_longlat <- sf::st_boundary(circle_longlat)
    
    circle_coords <- sf::st_coordinates(circle_longlat)[, c(1,2)]
    circle_coords <- circle_coords[order(circle_coords[, 1]),]
    circle_coords <- circle_coords[!duplicated(circle_coords),]
    
    ## Rebuild line ............................................................
    circle_longlat <-
      sf::st_linestring(circle_coords) %>%
      sf::st_sfc(crs = 4326)
    
    if(lat > 0) {
      rectangle <- list(rbind(circle_coords,
                              c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                              c(X = 180, Y = 90),
                              c(X = -180, Y = 90),
                              c(X = -180, circle_coords[1, 'Y']),
                              circle_coords[1, c('X','Y')])) %>%
        sf::st_polygon() %>%
        sf::st_sfc(crs = 4326)
    } else {
      rectangle <- list(rbind(circle_coords,
                              c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                              c(X = 180, Y = -90),
                              c(X = -180, Y = -90),
                              c(X = -180, circle_coords[1, 'Y']),
                              circle_coords[1, c('X','Y')])) %>%
        sf::st_polygon() %>%
        sf::st_sfc(crs = 4326)
    }
    
    circle_longlat <- suppressMessages(sf::st_union(
      sf::st_make_valid(circle_longlat),
      sf::st_make_valid(rectangle))
    )
  }
  
  ## A small negative buffer is necessary to avoid polygons still disappearing
  ## in a few pathological cases ...............................................
  ## Comment Cédric: Doesn't work with -.09 anymore, returns empty object.
  ##                 But works also without the buffer, so using 0 here to
  ##                 return the same type of object.
  visible <- suppressMessages(suppressWarnings(
    sf::st_intersection(sf::st_make_valid(mini_world),
                        sf::st_buffer(circle_longlat, 0)) %>%
      sf::st_transform(crs = ortho)
  ))
  
  ## Get reason why polygons are broken ........................................
  broken_reason <- sf::st_is_valid(visible, reason = TRUE)
  
  ## First fix NA's by decomposing them ........................................
  na_visible <- visible[is.na(broken_reason),]
  visible <- visible[!is.na(broken_reason),]
  
  ## Open and close polygons ...................................................
  na_visible <- sf::st_cast(na_visible, 'MULTILINESTRING') %>%
    sf::st_cast('LINESTRING', do_split=TRUE)
  na_visible <- na_visible %>%
    dplyr::mutate(npts = mapview::npts(geometry, by_feature = TRUE))
  
  ## Exclude polygons with less than 4 points ..................................
  na_visible <- na_visible %>%
    dplyr::filter(npts >= 4) %>%
    dplyr::select(-npts) %>%
    sf::st_cast('POLYGON')
  
  ## Fix other broken polygons .................................................
  broken <- which(!sf::st_is_valid(visible))
  for(land in broken) {
    result = suppressWarnings(tryCatch({
      visible[land,] <-
        sf::st_make_valid(visible[land,]) %>%
        sf::st_collection_extract()
    }, error = function(e) {
      visible[land,] <<- sf::st_buffer(visible[land,], 0)
    }))
  }
  
  ## Bind together the two tables ..............................................
  visible <- suppressMessages(rbind(visible, na_visible))
  


# ## Inset line -----------------------------------------------------------


israel_center <- filter(visible, name_long  == "Israel") %>% 
  st_centroid() %>%
  st_geometry() %>% 
  st_coordinates()
  
line_loc <- rbind(israel_center, israel_center + c(4e6, 4e6))

inset_line <- st_sfc(st_linestring(line_loc), crs = ortho)


# # Create globe as ggplot ....................................................
p_globe <-  ggplot2::ggplot()+
    ggplot2::geom_sf(data = circle, fill = "white") +
    ggplot2::geom_sf(data = circle,  alpha = .5) +
    ggplot2::geom_sf(data = sf::st_collection_extract(visible),
                     fill = "gray45", color = NA) +
    ggplot2::geom_sf(data = filter(visible, name_long  == "Israel"), fill = "#4B0082", color = NA) +
    ggplot2::geom_sf(data = circle, color = "grey60", fill = NA, size = .5) +
    geom_sf(data = inset_line, color = "gray75", size = 0.3, linetype = "dashed")+
    ggplot2::coord_sf(crs = ortho) +
    # labs(
    #   title = "Countries I visited in the past two years"
    # ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.grid.major = element_blank()
      )



# Inset Israel ------------------------------------------------------------

circle <- st_point(x = c(35, 31)) %>%
  st_buffer(dist = 7) %>%
  st_sfc(crs = ortho)

israel_zoomed <- st_intersection(circle, st_set_crs(mini_world, ortho))

p_small <- ggplot() +
  geom_sf(data = circle, color = "gray45",fill = "white")+
  geom_sf(data = israel_zoomed, fill = "gray55")+
  # Highlight Israel
  geom_sf(data = israel_zoomed[5], fill = "#4B0082", color = NA)+
  coord_sf(xlim = c(27,43), ylim = c(22, 38))+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "transparent", color = NA)
  )


p_globe + inset_element(p_small, left = 0.6, bottom = 0.7, right = 0.8, top = 1)+
  plot_annotation(
    title = "Countries I visited in the past year",
    caption = "@Amit_Levinson",
    theme = theme(
      title = element_text(size = 18, family = "Playfair Display", face = "bold", hjust = 0),
      plot.caption = element_text(size = 8, family = "Playfair Display", color = "gray35", hjust = 0.5),
      plot.margin = margin(4,0,4,0,"mm")
    ))
  
ggsave("2021/13_natural-earrth/visited.png", width = 7, height = 8)
