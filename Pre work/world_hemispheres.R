# dependencies ####
## packages ####
library(dplyr)
library(ggplot2)
library(sf)

## functions ####

get_coords <- function(.data, names = c("x","y"), crs = 4269){
  
  # global bindings
  geometry = NULL
  
  # ensure .data is an sf object
  if ("sf" %in% class(.data) == FALSE){
    stop("An sf object must be used with 'gw_get_coords()'.")
  }
  
  # store coordinates
  coords <- sf::st_crs(.data)$epsg
  
  if (is.na(coords) == TRUE){
    coords <- 0
  }
  
  # reproject
  if (coords != crs){
    .data <- sf::st_transform(.data, crs = crs)
  }
  
  # create coordinate columns
  ret <- do.call(rbind,sf::st_geometry(.data))
  ret <- dplyr::as_tibble(ret, .name_repair = 'unique')
  
  # ensure two columns are returned
  stopifnot(length(names) == ncol(ret))
  
  # name columns with coordinate data
  ret <- stats::setNames(ret, names)
  
  # combine coordinate data with source data
  out <- cbind(.data, ret) # %>%
  out <- dplyr::select(out, -geometry, dplyr::everything())
  
  # return output
  return(out)
  
}

# load data ####
# url - https://hub.arcgis.com/datasets/2b93b06dc0dc4e809d3c8db5cb96ba69_0/explore?location=16.169184%2C-75.935135%2C2.93
world <- st_read("~/Downloads/world_hemisphere/World_Countries_Generalized.geojson") %>%
  select(-SHAPE_Length, -SHAPE_Area)

# geoprocess ####
## calculate centroids ####
### centroid calculation
world_centroid <- st_centroid(world) %>%
  get_coords(crs = 4326) %>% # extract geometry col to x and y cols
  select(FID, x, y)

### unit test FID to make sure it uniquely IDs rows
all(unique(world_centroid$FID)) == TRUE
  
## identify hemispheres ####
world_centroid <- world_centroid %>%
  mutate(hemi = case_when(
    y > 23.5 ~ "northern",
    y < -23.5 ~ "southern",
    y < 23.5 & y > -23.5 ~ "tropics"
  )) %>%
  select(FID, hemi)

st_geometry(world_centroid) <- NULL

## combine with polygons ####
world <- left_join(world, world_centroid, by = "FID")

# map ####
# world <- st_transform(world, crs = 'ESRI:54030')
# ggplot(data = world, mapping = aes(fill = hemi)) +
#   geom_sf()

world2 <- world
st_geometry(world2) <- NULL

world2$iso_a3 <- countrycode(
  sourcevar = world2$COUNTRY,
  origin = "country.name",
  destination = "iso3c",
  warn = TRUE,
  nomatch = NA,
  custom_dict = NULL,
  custom_match = NULL,
  origin_regex = T
)

world2$iso_a3[world2$COUNTRY=="Saint Martin"]<-"MAF"
world2$iso_a3[world2$COUNTRY=="Bonaire"]<-"BES"
world2$iso_a3[world2$COUNTRY=="Saba"]<-"BES"
world2$iso_a3[world2$COUNTRY=="Saint Eustatius"]<-"BES"
world2$iso_a3[world2$COUNTRY=="Juan De Nova Island"]<-"ATF"
world2$iso_a3[world2$COUNTRY=="Glorioso Islands"]<-"ATF"
world2$iso_a3[world2$COUNTRY=="Micronesia"]<-"FSM"
world2$iso_a3[world2$COUNTRY=="Canarias"]<-"IC"



write.csv(world2[,c("iso_a3", "hemi")], "~/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/flu_maps/Pre work/hemi.csv", na="", row.names=F)




