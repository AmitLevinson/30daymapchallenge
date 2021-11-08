library(elevatr)
library(rayshader)
library(sf)

# Get Israel map for extracting raster layer below, you can also use the {rgeoboundaries} instead
# to get boundaries of a country you're interested in.
isrfull <- read_sf("data/maps/isr-full/israel_borders.shp")
isrfull <- st_transform(isrfull, 4326)

# Get elevation data
isrraster <- get_elev_raster(locations = isrfull, z = 9, clip = "locations")

# Convert to matrix for rayshader plotting
isrrayshader = raster_to_matrix(isrraster)

isrrayshader %>% 
  sphere_shade(texture = "bw") %>% 
  plot_3d(isrrayshader, zscale = 10, fov = 0, theta = 0, zoom = 0.9, phi = 45, windowsize = c(1000, 800))

render_snapshot("2021/09_monochrome/israelbw",
                title_text = "Israel's\nElevation",
                title_offset = c(60,50),
                title_color = "black",
                title_size = 40,
                title_font = "PT Serif",
                vignette = 0.1)
