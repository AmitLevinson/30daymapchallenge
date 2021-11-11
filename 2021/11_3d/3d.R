library(rayshader) 
library(rayvista)
# devtools::install_github("h-a-graham/rayvista", dependencies=TRUE)
library(rayrender)

lat <- 30.609379
long <-  34.884477


ramon <- plot_3d_vista(lat = lat, long = long, radius= 9000, overlay_detail = 14,
                    cache_dir = '2021/11_3d/cache',theta=25, phi=25,
                    windowsize = 800)


render_snapshot("2021/11_3d/ramon",
                title_text = "Ramon Crater",
                title_offset = c(60,50),
                title_color = "black",
                title_size = 40,
                title_font = "Playfair Display",
                vignette = 0.1,
                clear = TRUE)
