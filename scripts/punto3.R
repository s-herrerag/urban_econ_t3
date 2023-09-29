###################
# Punto 3: Taller 3
###################

# Libraries ---------------------------------------------------------------
library(pacman)
p_load(osmdata, tidyverse, sf, fixest, units)

# Prices: Load data for Bogota and Medellin ---------------------------------------

dataTaller2 <- readRDS("data/punto3/dataTaller2.Rds") #crs not available, assuming WGS84
dataTaller2_sf <- dataTaller2 %>%
  st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
  st_transform(crs=3116) #Longitudinal CRS for Bogotá

#For knowing which properties are in Medellin or Bogota, use data from census
mgn_2021_mpio <- st_read("data/punto3/MGN/MGN_MPIO_POLITICO") %>%
  st_transform(crs = 3116) %>%
  dplyr::select(c("MPIO_CNMBR", "MPIO_CDPMP", "geometry")) %>%
  rename(c("COD_MPIO"="MPIO_CDPMP"))

#Spatial join
dataTaller2_sf <- dataTaller2_sf %>%
  st_join(mgn_2021_mpio, .predicate=st_intersects, left = T)

#Create two dfs with prices
prices_bog <- dataTaller2_sf %>%
  filter(COD_MPIO=="11001")

prices_med <-  dataTaller2_sf %>%
  filter(COD_MPIO=="05001")

# Read OSM Data: Parks and public squares -----------------------------------------------------------

#First, select bounding boxes for Bog and Med
bbox_bog <- mgn_2021_mpio %>%
  filter(COD_MPIO=="11001") %>%
  st_transform(crs = 4326) %>% #Return to 4326 since that is OSM CRS!
  st_bbox()

bbox_med <- mgn_2021_mpio %>%
  filter(COD_MPIO=="05001") %>%
  st_transform(crs = 4326) %>% 
  st_bbox()

## Now select appropriate amenities

#Bogota
parks_bog <- opq(bbox_bog) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()
parks_bog_polygons <- parks_bog$osm_polygons %>%
  dplyr::select(c(osm_id, geometry)) %>%
  st_transform(crs = 3116) 

parks_bog_points <- st_centroid(parks_bog_polygons)

squares_bog <- opq(bbox_bog) %>%
  add_osm_feature(key = "place", value = "square") %>%
  osmdata_sf()
squares_bog_polygons <- squares_bog$osm_polygons %>%
  dplyr::select(c(osm_id, geometry)) %>%
  st_transform(crs = 3116) 

squares_bog_points <- st_centroid(squares_bog_polygons)

#Medellin
parks_med <- opq(bbox_med) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()
parks_med_polygons <- parks_med$osm_polygons %>%
  dplyr::select(c(osm_id, geometry)) %>%
  st_transform(crs = 3116) 

parks_med_points <- st_centroid(parks_med_polygons)

squares_med <- opq(bbox_med) %>%
  add_osm_feature(key = "place", value = "square") %>%
  osmdata_sf()
squares_med_polygons <- squares_med$osm_polygons %>%
  dplyr::select(c(osm_id, geometry)) %>%
  st_transform(crs = 3116)

squares_med_points <- st_centroid(squares_med_polygons)

# Distances to parks and squares ---------------------------------------------------------------
nearest_amenity <- function(x,y) {
  ###
  #Find the nearest point of y to each point in x. In this case, x should be the dataset of properties and prices and 
  #y should be the dataset of parks/squares. 
  ###
  
  #Indices of nearest amenity
  indices_nearest <- st_nearest_feature(x, y)
  data_nearest <- y[indices_nearest,]
  
  #Calculate distance
  res <- st_distance(x, data_nearest, by_element = TRUE)
  
  #Return dataset with distances
  res
}

#Bog
prices_bog$dist_park <- nearest_amenity(prices_bog, parks_bog_points) 
prices_bog$dist_square <- nearest_amenity(prices_bog, squares_bog_points)

prices_bog <- prices_bog %>%
  mutate(near_park=ifelse(dist_park<=set_units(200, "meters"), 1, 0),
         near_square=ifelse(dist_square<=set_units(200, "meters"), 1, 0)) %>%
  mutate(near_open_space = ifelse(near_park==1 | near_square==1, 1, 0))

#Med
prices_med$dist_park <- nearest_amenity(prices_med, parks_med_points) 
prices_med$dist_square <- nearest_amenity(prices_med, squares_med_points)

prices_med <- prices_med %>%
  mutate(near_park=ifelse(dist_park<=set_units(200, "meters"), 1, 0),
         near_square=ifelse(dist_square<=set_units(200, "meters"), 1, 0)) %>%
  mutate(near_open_space = ifelse(near_park==1 | near_square==1, 1, 0))


# Other amenities (control) -----------------------------------------------

retrieve_amenities <- function(bbox, key, value, type="polygons") {
  
  amenity_osm <- opq(bbox) %>%
    add_osm_feature(key = key, value = value) %>%
    osmdata_sf()
  
  if (type=="points"){
    amenity_osm_points <-amenity_osm$osm_points 
  }

  if (type=="polygons"){
    amenity_osm_points <-amenity_osm$osm_polygons 
  }  
  
  
  amenity_osm_points <- amenity_osm_points %>%
    st_transform(crs = 3116) %>%
    st_centroid() 
  
  amenity_osm_points
}

##Bog 
hospitals_bog_points <- retrieve_amenities(bbox_bog, "amenity", "hospital")
schools_bog_points <- retrieve_amenities(bbox_bog, "amenity", "school")
bus_bog_points <- retrieve_amenities(bbox_bog, "amenity", "bus_station", "points")

#Add to df
prices_bog$dist_hospital <- nearest_amenity(prices_bog, hospitals_bog_points) 
prices_bog$dist_school <- nearest_amenity(prices_bog,  hospitals_bog_points)
prices_bog$dist_bus <- nearest_amenity(prices_bog, bus_bog_points)


##Med 
hospitals_med_points <- retrieve_amenities(bbox_med, "healthcare", "hospital")
schools_med_points <- retrieve_amenities(bbox_med, "amenity", "school")
bus_med_points <- retrieve_amenities(bbox_med, "amenity", "bus_station", "points")

#Add to df
prices_med$dist_hospital <- nearest_amenity(prices_med, hospitals_med_points) 
prices_med$dist_school <- nearest_amenity(prices_med,  hospitals_med_points)
prices_med$dist_bus <- nearest_amenity(prices_med, bus_med_points)




