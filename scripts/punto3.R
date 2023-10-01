###################
# Punto 3: Taller 3
###################

# Libraries ---------------------------------------------------------------
library(pacman)
p_load(osmdata, tidyverse, sf, fixest, units, modelsummary, conleyreg)

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
  st_join(mgn_2021_mpio, .predicate=st_intersects, left = T) %>%
  mutate(logprice=log(price)) %>%#Use logs of price
  filter(logprice!=Inf) %>%
  filter(logprice !=-Inf) %>%
  drop_na(logprice)
  
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
police_bog_points <- retrieve_amenities(bbox_bog, "amenity", "police")

#Add to df
prices_bog$dist_hospital <- nearest_amenity(prices_bog, hospitals_bog_points) 
prices_bog$dist_school <- nearest_amenity(prices_bog,  schools_bog_points)
prices_bog$dist_bus <- nearest_amenity(prices_bog, bus_bog_points)
prices_bog$dist_police <- nearest_amenity(prices_bog, police_bog_points)

##Med 
hospitals_med_points <- retrieve_amenities(bbox_med, "healthcare", "hospital")
schools_med_points <- retrieve_amenities(bbox_med, "amenity", "school")
bus_med_points <- retrieve_amenities(bbox_med, "amenity", "bus_station", "points")
police_med_points <- retrieve_amenities(bbox_med, "amenity", "police")

#Add to df
prices_med$dist_hospital <- nearest_amenity(prices_med, hospitals_med_points) 
prices_med$dist_school <- nearest_amenity(prices_med,  schools_med_points)
prices_med$dist_bus <- nearest_amenity(prices_med, bus_med_points)
prices_med$dist_police <- nearest_amenity(prices_med, police_med_points)


### Lastly, add UPL and comunas
upl_bog <- st_read("data/punto3/unidadplaneamientolocal.gpkg") %>%
  st_transform(crs=3116) %>%
  dplyr::select(CODIGO_UPL, SHAPE)

comunas_med <- st_read("data/punto3/comunas_medellin") %>%
  st_transform(crs=3116) %>%
  filter(grepl("Comuna", IDENTIFICA)) %>%
  dplyr::select(IDENTIFICA, geometry)

prices_bog <- prices_bog %>%
  st_join(upl_bog, join= st_nearest_feature)

prices_bog <- prices_bog %>%
  rename("spatial_group_id"="CODIGO_UPL")
  
prices_med <- st_join(prices_med, comunas_med, join= st_nearest_feature)

prices_med <- prices_med %>%
  rename("spatial_group_id"="IDENTIFICA")


# Estimates ---------------------------------------------------------------

### Split rental vs sale
arriendo_bog <- prices_bog %>%
  filter(operation=="Alquiler") %>%
  mutate() 
venta_bog <- prices_bog %>%
  filter(operation=="Venta")

arriendo_med <- prices_med %>%
  filter(operation=="Alquiler")
venta_med <- prices_med %>%
  filter(operation=="Venta")

##### OLS #####
#Standard errors are clustered

ols_form <- formula(logprice~near_open_space + near_open_space:dist_police +  dist_hospital + dist_school + dist_police +
                      bedrooms + bathrooms + surface_covered | spatial_group_id)

bog_ols_venta <- feols(ols_form,
                       data = venta_bog,
                       cluster = ~spatial_group_id)

bog_ols_arriendo <- feols(ols_form,
                          data = arriendo_bog,
                          cluster = ~spatial_group_id)

med_ols_venta <- feols(ols_form,
                       data = venta_med,
                       cluster = ~spatial_group_id)

med_ols_arriendo <- feols(ols_form,
                          data = arriendo_med,
                          cluster = ~spatial_group_id)

##Export results with modelsummary

##Outputs from the models
coefs <- c("near_open_space" = "Menos de 200m a parque [0=No, 1=Sí]",
           "near_open_space:dist_police" = "Menos de 200m a parque * Distancia a estación de policía",
           "dist_police" = "Distancia a estación de policía",
           "dist_school" = "Distancia a colegio", 
           "dist_hospital" = "Distancia a hospital", 
           "bedrooms" = "N Habitaciones",
           "bathrooms" = "N baños",
           "surface_covered" = "Área de la propiedad",
           "(Intercept)" = "Constante")
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared", "R2",            2,
  "adj.r.squared", "Adj. R2",   2)

models_ols<-list()
models_ols[["Precio de venta (log) en Bogotá"]] <- bog_ols_venta
models_ols[["Precio de arriendo (log) en Bogotá"]] <- bog_ols_arriendo
models_ols[["Precio de venta (log) en Medellín"]] <- med_ols_venta
models_ols[["Precio de arriendo (log) en Medellín"]] <- med_ols_arriendo

modelsummary(models_ols,
             fmt=fmt_decimal(digits = 6),
             stars = c("*"=0.1, "**"=0.05, "***"=0.001),
             coef_map = coefs,
             gof_map = gm,
             output = "tables/modelos_ols(clustered)_p3.tex")

##### Conley regs #####
#Uses the same formula and should yield the same estimates, only changes std. errors
bog_conley_venta <- conleyreg(ols_form,
                       data = venta_bog,
                       dist_cutoff = 3,
                       crs = st_crs(3116),
                       st_distance = T,
                       dist_which = "Euclidean")

bog_conley_arriendo <- conleyreg(ols_form,
                              data = arriendo_bog,
                              dist_cutoff = 3,
                              crs = st_crs(3116),
                              st_distance = T)

med_conley_venta <- conleyreg(ols_form,
                            data = venta_med,
                            dist_cutoff = 3,
                            crs = st_crs(3116),
                            st_distance = T)

med_conley_arriendo <- conleyreg(ols_form,
                              data = arriendo_med,
                              dist_cutoff = 1000,
                              crs = st_crs(3116),
                              st_distance = T,
                              dist_which = "Euclidean")



summary(med_conley_arriendo)

modelsummary(med_conley_arriendo,
             output = "tables/test_conley.tex")

##### SFD #####

ggplot(venta_bog)+
  geom_histogram(aes(x=logprice))


ggplot(venta_bog)+
  geom_point(aes(x=dist_park, y=price))













