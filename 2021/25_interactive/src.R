library(sf)
library(jsonlite)
library(httr)
library(purrr)
Sys.setlocale("LC_ALL", "Hebrew")

# Get the API information from Tel-Aviv municipality website
elderly_info <- data.frame(
  apiurl = c(
    "https://gisn.tel-aviv.gov.il/arcgis/rest/services/IView2/MapServer/600/query?where=1%3D1&outFields=*&f=json",
    "https://gisn.tel-aviv.gov.il/arcgis/rest/services/IView2/MapServer/581/query?where=1%3D1&outFields=*&f=json",
    "https://gisn.tel-aviv.gov.il/arcgis/rest/services/IView2/MapServer/855/query?where=1%3D1&outFields=*&f=json"
  ),
  apiname = c("elderly_clubs","geriatric_day_center", "programs_for_elderly" )
)


# Function to extract, clean and transform to sf the api call
clean_ta_api <- function(apiurl, apiname) {
apiraw <- GET(apiurl) 

apilist <- fromJSON(content(apiraw, "text"), simplifyVector = FALSE)

sf_data <- map_dfr(apilist$features, pluck("geometry")) %>% 
  mutate(
    shem = map_chr(apilist$features, pluck, "attributes", "shem", .default = NA),
    location_name = apiname
  ) %>% 
  st_as_sf(coords = c("x", "y"), crs = 2039)


return(sf_data)
}


# Generate data
elderly <- map2_dfr(elderly_info$apiurl, elderly_info$apiname, clean_ta_api)


# Appendix -- cool chuck & pluck!
library(purrr)

lst <- list(foo = list("f", "o" ,"o"), bar = list("b", "a", "r"), foobar = list(1,2))

map(lst, pluck, 3)
