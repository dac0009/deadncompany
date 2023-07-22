# Completed app 

# 1. load required packages
library(tidyverse)
library(sf)
library(leaflet)
##########################################################################################
# 2. Define color palletes 
color_palette <- c("#46ACC8", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#550307",
                   "#A65628", "#F781BF", "#999999", "#66C2A5", "#273046","#FD6467", 
                   "#A6D854", "#FFD92F", "#0B775E")

opening_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
                     "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#FDBF6F", 
                     "#B3B3CC", "#CC79A7", "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                     "#66A61E", "#E6AB02", "#A6761D", "#666666", "#1B78B8", "#E31A1C", 
                     "#FD8D3C", "#7C7C7C")

encore_palette <- c("#FF0000", "#FF7F00", "#00FF00", "#00FFFF", "#0000FF", 
                    "#8B00FF", "#FF00FF", "#FF1493", "#FF4500", "#FFD700", 
                    "#32CD32", "#00CED1", "#4169E1", "#800080", "#FFC0CB", 
                    "#00FF7F", "#FFA500", "#FFFF00", "#8A2BE2", "#00BFFF", 
                    "#FF69B4", "#7FFF00", "#FF6347", "#FFA07A", "#8FBC8F")
##########################################################################################
# 3. Clean Raw Data
states <- st_read("states/states.shp") %>%
  st_transform(crs = 4326)

cities <- st_read("USA_Cities/US_Cities.shp") %>%
  st_transform(crs = 4326)

missing_points <- st_read("missing_points/bethel_wheatland.shp") %>%
  st_transform(crs = 4326)

george <- cities %>%
  filter(NAME == "George")
george$FID_1 <- 9999

bristow <- cities %>%
  filter(NAME == "Bristow")
bristow$FID_1 <- 9998

burgettstown <- cities %>%
  filter(NAME == "Burgettstown")
burgettstown$FID_1 <- 9997

clarkston <- cities %>%
  filter(NAME == "Clarkston")
clarkston$FID_1 <- 9996

darien <- cities %>%
  filter(NAME == "Darien Center")
darien$FID_1 <- 9995

troy <- cities %>%
  filter(NAME == "East Troy")
troy$FID_1 <- 9994

Foxborough <- cities %>%
  filter(NAME == "Foxborough")
Foxborough$FID_1 <- 9993

hollywood <- cities %>%
  filter(NAME == "Hollywood")
hollywood$FID_1[2] <- 9992
hollywood <- hollywood %>%
  filter(!ST == "FL")

mansfield <- cities %>%
  filter(NAME == "Mansfield")

mansfield <- mansfield %>%
  filter(FID_1 == 9991)

morrison <- cities %>%
  filter(NAME == "Morrison")
morrison$FID_1 <- 9990

found_places <- bind_rows(bristow, burgettstown, clarkston, darien, 
                          Foxborough, george, hollywood, mansfield, morrison, 
                          troy, missing_points)

found_places <- found_places %>%
  filter(!ST == "FL")

found_places$FID_1[11] <- 9988

cities <- bind_rows(cities, found_places) %>%
  filter(!FID_1 == 0)

cities <- cities %>%
  filter(!FID_1 == 9988)

bethel <- missing_points %>%
  filter(NAME == "Bethel")
bethel$FID_1 <- 9988

cities <- bind_rows(cities, bethel)
##########################################################################################
# 4. 2016 map
tour16 <- read.csv("summer_tours/summer2016.csv")

summer16_cities <- cities %>%
  filter(FID_1 == 2264|FID_1 == 3511|FID_1 == 2382|FID_1 == 301|
           FID_1 == 3405|FID_1 == 2215|FID_1 == 9998|FID_1 == 2174|
           FID_1 == 726|FID_1 == 661|FID_1 == 9996|FID_1 == 9994|
           FID_1 == 9997|FID_1 == 1437|FID_1 == 2913|FID_1 == 9999|
           FID_1 == 1165|FID_1 == 1052|FID_1 == 9989|FID_1 == 452)
summer16_cities$FID_1[15:19] <- 0

summer16_cities <- summer16_cities %>%
  filter(!FID_1 == 0)

tour16$Show_num <- as.factor(tour16$Show_num)
levels(tour16$Show_num)

setlists16 <- tour16 %>%
  group_by(Show_num) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

tour16_with_geometry <- left_join(tour16, summer16_cities[, c("FID_1", "geometry")], by = "FID_1")

# Extract latitude and longitude from geometry column
tour16_with_geometry$latitude <- st_coordinates(tour16_with_geometry$geometry)[, 2]
tour16_with_geometry$longitude <- st_coordinates(tour16_with_geometry$geometry)[, 1]

tour16_with_geometry <- tour16_with_geometry[complete.cases(tour16_with_geometry$latitude, tour16_with_geometry$longitude), ]

# Convert tour18_with_geometry to sf object
tour16_with_geometry_sf <- st_as_sf(tour16_with_geometry, coords = c("longitude", "latitude"), crs = 4326)

# Transform the sf object to WGS84
tour16_with_geometry_sf <- st_transform(tour16_with_geometry_sf, crs = 4326)

# Group the songs by FID_1 and concatenate them into a single string
setlist16 <- tour16_with_geometry_sf %>%
  group_by(FID_1) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

# Merge the setlist data with the tour23_with_geometry_sf
tour16_with_setlist <- st_join(tour16_with_geometry_sf, setlist16)

# Missing cities; Boston, Portland, George, Wheatland
# Boston
missing_bos16 <- setlist16 %>%
  filter(FID_1 == 1437)
missing_bos16_df <- as.data.frame(missing_bos16)
tour16_with_setlist <- tour16_with_setlist %>%
  dplyr::left_join(missing_bos16_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Portland
missing_por16 <- setlist16 %>%
  filter(FID_1 == 2913)
missing_por16_df <- as.data.frame(missing_por16)
tour16_with_setlist <- tour16_with_setlist %>%
  dplyr::left_join(missing_por16_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# George
missing_geo16 <- setlist16 %>%
  filter(FID_1 == 9999)
missing_geo16_df <- as.data.frame(missing_geo16)
tour16_with_setlist <- tour16_with_setlist %>%
  dplyr::left_join(missing_geo16_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Wheatland
missing_whe16 <- setlist16 %>%
  filter(FID_1 == 9989)
missing_whe16_df <- as.data.frame(missing_whe16)
tour16_with_setlist <- tour16_with_setlist %>%
  dplyr::left_join(missing_whe16_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)

tour16_with_setlist$latitude <- st_coordinates(tour16_with_setlist$geometry)[, 2]
tour16_with_setlist$longitude <- st_coordinates(tour16_with_setlist$geometry)[, 1]

map16 <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "ESRI") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "CartoDB") %>%
  addPolygons(data = states, color = "grey", fill = FALSE, weight = 2,
              highlight = highlightOptions(weight = 3, color = "white", bringToFront = FALSE), popup = FALSE) %>%
  addCircleMarkers(data = tour16_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~setlist, group = "Tour16",
                   color = color_palette[as.numeric(factor(tour16_with_setlist$Location))]) %>%
  setView(lat = 39, lng = -100, zoom = 4) %>%
  addLayersControl(
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    options = layersControlOptions(collapsed = FALSE))

encore_songs16 <- tour16_with_setlist[tour16_with_setlist$Set.Type == "Encore", ]
encore_songs16 <- sf::st_drop_geometry(encore_songs16)

encore_agg16 <- encore_songs16 %>%
  group_by(Location) %>%
  summarize(encore_setlist = paste(Song, collapse = "<br>"))

# Merge aggregated encore songs with tour16_with_setlist
tour16_with_setlist <- left_join(tour16_with_setlist, encore_agg16, by = "Location")

map16 <- map16 %>%
  addCircleMarkers(data = tour16_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~encore_setlist, group = "Encore Songs",
                   color = encore_palette)

# Add layers control
map16 <- map16 %>%
  addLayersControl(
    overlayGroups = c("Tour16", "Encore Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )

opening_songs16 <- tour16_with_setlist[tour16_with_setlist$Song.Order == 1, ]
opening_songs16 <- sf::st_drop_geometry(opening_songs16)

opening_agg16 <- opening_songs16 %>%
  group_by(Location) %>%
  summarize(opening_setlist = paste(Song, collapse = "<br>"))

tour16_with_setlist <- left_join(tour16_with_setlist, opening_agg16, by = "Location")

map16 <- map16 %>%
  addCircleMarkers(data = tour16_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~opening_setlist, group = "Opening Songs",
                   color = opening_palette)
map16 <- map16 %>%
  addLayersControl(
    overlayGroups = c("Tour16", "Encore Songs", "Opening Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )


map16


##########################################################################################
# 5. 2017 map
tour17 <- read.csv("summer_tours/summer2017.csv")

summer17_cities <- cities %>%
  filter(FID_1 == 3364|FID_1 == 910|FID_1 == 9992|FID_1 == 452|FID_1 == 3830|
           FID_1 == 661|FID_1 == 1879|FID_1 == 9997|FID_1 == 1437|FID_1 == 2215|
           FID_1 == 9998|FID_1 == 2174|FID_1 == 3405|FID_1 == 2390|FID_1 == 63)


summer17_cities$FID_1[13:15] <- 0
summer17_cities <- summer17_cities %>%
  filter(!FID_1 == 0)

tour17$Show_num <- as.factor(tour17$Show_num)
levels(tour17$Show_num)

setlists17 <- tour17 %>%
  group_by(Show_num) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

tour17_with_geometry <- left_join(tour17, summer17_cities[, c("FID_1", "geometry")], by = "FID_1")

# Extract latitude and longitude from geometry column
tour17_with_geometry$latitude <- st_coordinates(tour17_with_geometry$geometry)[, 2]
tour17_with_geometry$longitude <- st_coordinates(tour17_with_geometry$geometry)[, 1]

tour17_with_geometry <- tour17_with_geometry[complete.cases(tour17_with_geometry$latitude, tour17_with_geometry$longitude), ]

# Convert tour18_with_geometry to sf object
tour17_with_geometry_sf <- st_as_sf(tour17_with_geometry, coords = c("longitude", "latitude"), crs = 4326)

# Transform the sf object to WGS84
tour17_with_geometry_sf <- st_transform(tour17_with_geometry_sf, crs = 4326)

# Group the songs by FID_1 and concatenate them into a single string
setlist17 <- tour17_with_geometry_sf %>%
  group_by(FID_1) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

# Merge the setlist data with the tour23_with_geometry_sf
tour17_with_setlist <- st_join(tour17_with_geometry_sf, setlist17)

# Missing shows; Boston & Chicago
# Chicago
missing_chi17 <- setlist17 %>%
  filter(FID_1 == 63)
missing_chi17_df <- as.data.frame(missing_chi17)
tour17_with_setlist <- tour17_with_setlist %>%
  dplyr::left_join(missing_chi17_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Boston
missing_bos17 <- setlist17 %>%
  filter(FID_1 == 1437)
missing_bos17_df <- as.data.frame(missing_bos17)
tour17_with_setlist <- tour17_with_setlist %>%
  dplyr::left_join(missing_bos17_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
############################################
tour17_with_setlist$latitude <- st_coordinates(tour17_with_setlist$geometry)[, 2]
tour17_with_setlist$longitude <- st_coordinates(tour17_with_setlist$geometry)[, 1]

# Define a custom color palette with 15 distinct colors

map17 <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "ESRI") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "CartoDB") %>%
  addPolygons(data = states, color = "grey", fill = FALSE, weight = 2,
              highlight = highlightOptions(weight = 3, color = "white", bringToFront = FALSE), popup = FALSE) %>%
  addCircleMarkers(data = tour17_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~setlist, group = "Tour17",
                   color = color_palette[as.numeric(factor(tour17_with_setlist$Location))]) %>%
  setView(lat = 39, lng = -100, zoom = 4) %>%
  addLayersControl(
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    options = layersControlOptions(collapsed = FALSE))

encore_songs17 <- tour17_with_setlist[tour17_with_setlist$Set.Type == "Encore", ]
encore_songs17 <- sf::st_drop_geometry(encore_songs17)

encore_agg17 <- encore_songs17 %>%
  group_by(Location) %>%
  summarize(encore_setlist = paste(Song, collapse = "<br>"))

# Merge aggregated encore songs with tour17_with_setlist
tour17_with_setlist <- left_join(tour17_with_setlist, encore_agg17, by = "Location")


map17 <- map17 %>%
  addCircleMarkers(data = tour17_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~encore_setlist, group = "Encore Songs",
                   color = encore_palette)

# Add layers control
map17 <- map17 %>%
  addLayersControl(
    overlayGroups = c("Tour17", "Encore Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )

opening_songs17 <- tour17_with_setlist[tour17_with_setlist$Song.Order == 1, ]
opening_songs17 <- sf::st_drop_geometry(opening_songs17)

opening_agg17 <- opening_songs17 %>%
  group_by(Location) %>%
  summarize(opening_setlist = paste(Song, collapse = "<br>"))

tour17_with_setlist <- left_join(tour17_with_setlist, opening_agg17, by = "Location")

map17 <- map17 %>%
  addCircleMarkers(data = tour17_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~opening_setlist, group = "Opening Songs",
                   color = opening_palette)
map17 <- map17 %>%
  addLayersControl(
    overlayGroups = c("Tour17", "Encore Songs", "Opening Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )


map17

##########################################################################################
# 6. 2018 map
tour18 <- read.csv("summer_tours/summer2018.csv")

summer18_cities <- cities %>%
  filter(FID_1 == 9991|FID_1 == 3405|FID_1 == 2382|FID_1 == 301|FID_1 == 1879|FID_1 == 2318|FID_1 == 2215|FID_1 == 726|
           FID_1 == 2174|FID_1 == 9995|FID_1 == 2390|FID_1 == 9994|FID_1 == 9999|FID_1 == 2887|FID_1 == 452|FID_1 == 1052|
           FID_1 == 415|FID_1 == 2037|FID_1 == 661)

summer18_cities$FID_1[c(17,18,16,19)] <- 0

summer18_cities <- summer18_cities %>%
  filter(!FID_1 == 0)

tour18$Show_num <- as.factor(tour18$Show_num)
levels(tour18$Show_num)

setlists18 <- tour18 %>%
  group_by(Show_num) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

tour18_with_geometry <- left_join(tour18, summer18_cities[, c("FID_1", "geometry")], by = "FID_1")

# Extract latitude and longitude from geometry column
tour18_with_geometry$latitude <- st_coordinates(tour18_with_geometry$geometry)[, 2]
tour18_with_geometry$longitude <- st_coordinates(tour18_with_geometry$geometry)[, 1]

tour18_with_geometry <- tour18_with_geometry[complete.cases(tour18_with_geometry$latitude, tour18_with_geometry$longitude), ]

# Convert tour18_with_geometry to sf object
tour18_with_geometry_sf <- st_as_sf(tour18_with_geometry, coords = c("longitude", "latitude"), crs = 4326)

# Transform the sf object to WGS84
tour18_with_geometry_sf <- st_transform(tour18_with_geometry_sf, crs = 4326)

# Group the songs by FID_1 and concatenate them into a single string
setlist18 <- tour18_with_geometry_sf %>%
  group_by(FID_1) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

# Merge the setlist data with the tour23_with_geometry_sf
tour18_with_setlist <- st_join(tour18_with_geometry_sf, setlist18)

# Missing shows; Mansfield, Raleigh, George
# George
missing_geo18 <- setlist18 %>%
  filter(FID_1 == 9999)
missing_geo18_df <- as.data.frame(missing_geo18)
tour18_with_setlist <- tour18_with_setlist %>%
  dplyr::left_join(missing_geo18_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Mansfield
missing_man18 <- setlist18 %>%
  filter(FID_1 == 9991)
missing_man18_df <- as.data.frame(missing_man18)
tour18_with_setlist <- tour18_with_setlist %>%
  dplyr::left_join(missing_man18_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Raeliegh
missing_nc18 <- setlist18 %>%
  filter(FID_1 == 2318)
missing_nc18_df <- as.data.frame(missing_nc18)
tour18_with_setlist <- tour18_with_setlist %>%
  dplyr::left_join(missing_nc18_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)

tour18_with_setlist$latitude <- st_coordinates(tour18_with_setlist$geometry)[, 2]
tour18_with_setlist$longitude <- st_coordinates(tour18_with_setlist$geometry)[, 1]


map18 <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "ESRI") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "CartoDB") %>%
  addPolygons(data = states, color = "grey", fill = FALSE, weight = 2,
              highlight = highlightOptions(weight = 3, color = "white", bringToFront = FALSE), popup = FALSE) %>%
  addCircleMarkers(data = tour18_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~setlist, group = "Tour18",
                   color = color_palette[as.numeric(factor(tour18_with_setlist$Location))]) %>%
  setView(lat = 39, lng = -100, zoom = 4) %>%
  addLayersControl(
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    options = layersControlOptions(collapsed = FALSE))

encore_songs18 <- tour18_with_setlist[tour18_with_setlist$Set.Type == "Encore", ]
encore_songs18 <- sf::st_drop_geometry(encore_songs18)

encore_agg18 <- encore_songs18 %>%
  group_by(Location) %>%
  summarize(encore_setlist = paste(Song, collapse = "<br>"))

# Merge aggregated encore songs with tour23_with_setlist
tour18_with_setlist <- left_join(tour18_with_setlist, encore_agg18, by = "Location")

map18 <- map18 %>%
  addCircleMarkers(data = tour18_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~encore_setlist, group = "Encore Songs",
                   color = encore_palette)

# Add layers control
map18 <- map18 %>%
  addLayersControl(
    overlayGroups = c("Tour18", "Encore Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )

opening_songs18 <- tour18_with_setlist[tour18_with_setlist$Song.Order == 1, ]
opening_songs18 <- sf::st_drop_geometry(opening_songs18)

opening_agg18 <- opening_songs18 %>%
  group_by(Location) %>%
  summarize(opening_setlist = paste(Song, collapse = "<br>"))

tour18_with_setlist <- left_join(tour18_with_setlist, opening_agg18, by = "Location")

map18 <- map18 %>%
  addCircleMarkers(data = tour18_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~opening_setlist, group = "Opening Songs",
                   color = opening_palette)
map18 <- map18 %>%
  addLayersControl(
    overlayGroups = c("Tour18", "Encore Songs", "Opening Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )


map18

##########################################################################################
# 7. 2019 map
tour19 <- read.csv("summer_tours/summer2019.csv")
# Clean data
summer19_cities <- cities %>%
  filter(FID_1 == 452|FID_1 == 9992|FID_1 == 9999|FID_1 == 301|FID_1 == 63|
           FID_1 == 2215|FID_1 == 3405|FID_1 == 9993|FID_1 == 2174|FID_1 == 9998|
           FID_1 == 2264|FID_1 == 3592|FID_1 == 3592|FID_1 == 661 |FID_1 == 1879)

summer19_cities$FID_1[11:14] <- 0 
summer19_cities <- summer19_cities %>%
  filter(!FID_1 == 0)

tour19$Show_num <- as.factor(tour19$Show_num)
levels(tour19$Show_num)

setlists19 <- tour19 %>%
  group_by(Show_num) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

tour19_with_geometry <- left_join(tour19, summer19_cities[, c("FID_1", "geometry")], by = "FID_1")

# Extract latitude and longitude from geometry column
tour19_with_geometry$latitude <- st_coordinates(tour19_with_geometry$geometry)[, 2]
tour19_with_geometry$longitude <- st_coordinates(tour19_with_geometry$geometry)[, 1]

tour19_with_geometry <- tour19_with_geometry[complete.cases(tour19_with_geometry$latitude, tour19_with_geometry$longitude), ]

# Convert tour22_with_geometry to sf object
tour19_with_geometry_sf <- st_as_sf(tour19_with_geometry, coords = c("longitude", "latitude"), crs = 4326)

# Transform the sf object to WGS84
tour19_with_geometry_sf <- st_transform(tour19_with_geometry_sf, crs = 4326)

# Group the songs by FID_1 and concatenate them into a single string
setlist19 <- tour19_with_geometry_sf %>%
  group_by(FID_1) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

# Merge the setlist data with the tour23_with_geometry_sf
tour19_with_setlist <- st_join(tour19_with_geometry_sf, setlist19)

# Missing shows; George, Chicago, 
# Chicago
missing_chi19 <- setlist19 %>%
  filter(FID_1 == 63)
missing_chi19_df <- as.data.frame(missing_chi19)
tour19_with_setlist <- tour19_with_setlist %>%
  dplyr::left_join(missing_chi19_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# George
missing_geo19 <- setlist19 %>%
  filter(FID_1 == 9999)
missing_geo19_df <- as.data.frame(missing_geo19)
tour19_with_setlist <- tour19_with_setlist %>%
  dplyr::left_join(missing_geo19_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)

# Extract latitude and longitude from geometry column
tour19_with_setlist$latitude <- st_coordinates(tour19_with_setlist$geometry)[, 2]
tour19_with_setlist$longitude <- st_coordinates(tour19_with_setlist$geometry)[, 1]


map19 <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "ESRI") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "CartoDB") %>%
  addPolygons(data = states, color = "grey", fill = FALSE, weight = 2,
              highlight = highlightOptions(weight = 3, color = "white", bringToFront = FALSE), popup = FALSE) %>%
  addCircleMarkers(data = tour19_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~setlist, group = "Tour19",
                   color = color_palette[as.numeric(factor(tour19_with_setlist$Location))]) %>%
  setView(lat = 39, lng = -100, zoom = 4) %>%
  addLayersControl(
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    options = layersControlOptions(collapsed = FALSE))

encore_songs19 <- tour19_with_setlist[tour19_with_setlist$Set.Type == "Encore", ]
encore_songs19 <- sf::st_drop_geometry(encore_songs19)

encore_agg19 <- encore_songs19 %>%
  group_by(Location) %>%
  summarize(encore_setlist = paste(Song, collapse = "<br>"))

# Merge aggregated encore songs with tour23_with_setlist
tour19_with_setlist <- left_join(tour19_with_setlist, encore_agg19, by = "Location")

map19 <- map19 %>%
  addCircleMarkers(data = tour19_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~encore_setlist, group = "Encore Songs",
                   color = encore_palette)

# Add layers control
map19 <- map19 %>%
  addLayersControl(
    overlayGroups = c("Tour19", "Encore Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )

opening_songs19 <- tour19_with_setlist[tour19_with_setlist$Song.Order == 1, ]
opening_songs19 <- sf::st_drop_geometry(opening_songs19)

opening_agg19 <- opening_songs19 %>%
  group_by(Location) %>%
  summarize(opening_setlist = paste(Song, collapse = "<br>"))

tour19_with_setlist <- left_join(tour19_with_setlist, opening_agg19, by = "Location")

map19 <- map19 %>%
  addCircleMarkers(data = tour19_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~opening_setlist, group = "Opening Songs",
                   color = opening_palette)
map19 <- map19 %>%
  addLayersControl(
    overlayGroups = c("Tour19", "Encore Songs", "Opening Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )


map19
##########################################################################################
# 8. 2021 map
tour21 <- read.csv("summer_tours/summer2021.csv")

summer21_cities <- cities %>%
  filter(FID_1 == 2318|FID_1 == 9998|FID_1 == 2174|FID_1 == 2987|
           FID_1 == 9988|FID_1 == 9995|FID_1 == 2215|FID_1 == 2962|
           FID_1 == 9991|FID_1 == 726|FID_1 == 2390|FID_1 == 9996|
           FID_1 == 2382|FID_1 == 3296|FID_1 == 301|FID_1 == 63|
           FID_1 == 2264|FID_1 == 1879|FID_1 == 3592|FID_1 == 3754|
           FID_1 == 9990|FID_1 == 677|FID_1 == 910|FID_1 == 1052|FID_1 == 9992)

summer21_cities$FID_1[19:24] <- 0 
summer21_cities <- summer21_cities %>%
  filter(!FID_1 == 0)

tour21$Show_num <- as.factor(tour21$Show_num)
levels(tour21$Show_num)

setlists21 <- tour21 %>%
  group_by(Show_num) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

# Change George FID_1
tour21_with_geometry <- left_join(tour21, summer21_cities[, c("FID_1", "geometry")], by = "FID_1")

# Extract latitude and longitude from geometry column
tour21_with_geometry$latitude <- st_coordinates(tour21_with_geometry$geometry)[, 2]
tour21_with_geometry$longitude <- st_coordinates(tour21_with_geometry$geometry)[, 1]

tour21_with_geometry <- tour21_with_geometry[complete.cases(tour21_with_geometry$latitude, tour21_with_geometry$longitude), ]

# Convert tour21_with_geometry to sf object
tour21_with_geometry_sf <- st_as_sf(tour21_with_geometry, coords = c("longitude", "latitude"), crs = 4326)

# Transform the sf object to WGS84
tour21_with_geometry_sf <- st_transform(tour21_with_geometry_sf, crs = 4326)

# Group the songs by FID_1 and concatenate them into a single string
setlist21 <- tour21_with_geometry_sf %>%
  group_by(FID_1) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

# Merge the setlist data with the tour23_with_geometry_sf
tour21_with_setlist <- st_join(tour21_with_geometry_sf, setlist21)

# Missing cities; Raleigh, Philly, Bethel, Mansfield, Chicago, Morrison
# Chicago
missing_chi21 <- setlist21 %>%
  filter(FID_1 == 63)
missing_chi21_df <- as.data.frame(missing_chi21)
tour21_with_setlist <- tour21_with_setlist %>%
  dplyr::left_join(missing_chi21_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Philly
missing_phi21 <- setlist21 %>%
  filter(FID_1 == 2987)
missing_phi21_df <- as.data.frame(missing_phi21)
tour21_with_setlist <- tour21_with_setlist %>%
  dplyr::left_join(missing_phi21_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Bethel
missing_beth21 <- setlist21 %>%
  filter(FID_1 == 9988)
missing_beth21_df <- as.data.frame(missing_beth21)
tour21_with_setlist <- tour21_with_setlist %>%
  dplyr::left_join(missing_beth21_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Raeligh
missing_nc21 <- setlist21 %>%
  filter(FID_1 == 2318)
missing_nc21_df <- as.data.frame(missing_nc21)
tour21_with_setlist <- tour21_with_setlist %>%
  dplyr::left_join(missing_nc21_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Mansfield
missing_man21 <- setlist21 %>%
  filter(FID_1 == 9991)
missing_man21_df <- as.data.frame(missing_man21)
tour21_with_setlist <- tour21_with_setlist %>%
  dplyr::left_join(missing_man21_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Morrison
missing_mor21 <- setlist21 %>%
  filter(FID_1 == 9990)
missing_mor21_df <- as.data.frame(missing_mor21)
tour21_with_setlist <- tour21_with_setlist %>%
  dplyr::left_join(missing_mor21_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
#############################################
# Extract latitude and longitude from geometry column
tour21_with_setlist$latitude <- st_coordinates(tour21_with_setlist$geometry)[, 2]
tour21_with_setlist$longitude <- st_coordinates(tour21_with_setlist$geometry)[, 1]

map21 <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "ESRI") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "CartoDB") %>%
  addPolygons(data = states, color = "grey", fill = FALSE, weight = 2,
              highlight = highlightOptions(weight = 3, color = "white", bringToFront = FALSE), popup = FALSE) %>%
  addCircleMarkers(data = tour21_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~setlist, group = "Tour21",
                   color = color_palette[as.numeric(factor(tour21_with_setlist$Location))]) %>%
  setView(lat = 39, lng = -100, zoom = 4) %>%
  addLayersControl(
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    options = layersControlOptions(collapsed = FALSE))


encore_songs21 <- tour21_with_setlist[tour21_with_setlist$Set.Type == "Encore", ]
encore_songs21 <- sf::st_drop_geometry(encore_songs21)

encore_agg21 <- encore_songs21 %>%
  group_by(Location) %>%
  summarize(encore_setlist = paste(Song, collapse = "<br>"))

# Merge aggregated encore songs with tour23_with_setlist
tour21_with_setlist <- left_join(tour21_with_setlist, encore_agg21, by = "Location")

map21 <- map21 %>%
  addCircleMarkers(data = tour21_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~encore_setlist, group = "Encore Songs",
                   color = encore_palette)

# Add layers control
map21 <- map21 %>%
  addLayersControl(
    overlayGroups = c("Tour21", "Encore Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )



opening_songs21 <- tour21_with_setlist[tour21_with_setlist$Song.Order == 1, ]
opening_songs21 <- sf::st_drop_geometry(opening_songs21)

opening_agg21 <- opening_songs21 %>%
  group_by(Location) %>%
  summarize(opening_setlist = paste(Song, collapse = "<br>"))

tour21_with_setlist <- left_join(tour21_with_setlist, opening_agg21, by = "Location")

map21 <- map21 %>%
  addCircleMarkers(data = tour21_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~opening_setlist, group = "Opening Songs",
                   color = opening_palette)
map21 <- map21 %>%
  addLayersControl(
    overlayGroups = c("Tour21", "Encore Songs", "Opening Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )


map21
##########################################################################################
# 9. 2022 map
tour22 <- read.csv("summer_tours/summer2022.csv")

summer22_cities <- cities %>%
  filter(FID_1 == 415|FID_1 == 452|FID_1 == 661|FID_1 == 3296|FID_1 == 2382|
           FID_1 == 63|FID_1 == 301|FID_1 == 9996|FID_1 == 9988|
           FID_1 == 9993|FID_1 == 726|FID_1 == 9998|FID_1 == 2987|
           FID_1 == 9997|FID_1 == 2174)

summer22_cities$FID_1[11:14] <- 0
summer22_cities <- summer22_cities %>%
  filter(!FID_1 == 0)

tour22$Show_num <- as.factor(tour22$Show_num)
levels(tour22$Show_num)

setlists22 <- tour22 %>%
  group_by(Show_num) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

# Change George FID_1
tour22_with_geometry <- left_join(tour22, summer22_cities[, c("FID_1", "geometry")], by = "FID_1")

# Extract latitude and longitude from geometry column
tour22_with_geometry$latitude <- st_coordinates(tour22_with_geometry$geometry)[, 2]
tour22_with_geometry$longitude <- st_coordinates(tour22_with_geometry$geometry)[, 1]

tour22_with_geometry <- tour22_with_geometry[complete.cases(tour22_with_geometry$latitude, tour22_with_geometry$longitude), ]

# Convert tour22_with_geometry to sf object
tour22_with_geometry_sf <- st_as_sf(tour22_with_geometry, coords = c("longitude", "latitude"), crs = 4326)

# Transform the sf object to WGS84
tour22_with_geometry_sf <- st_transform(tour22_with_geometry_sf, crs = 4326)

# Group the songs by FID_1 and concatenate them into a single string
setlist22 <- tour22_with_geometry_sf %>%
  group_by(FID_1) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

# Merge the setlist data with the tour23_with_geometry_sf
tour22_with_setlist <- st_join(tour22_with_geometry_sf, setlist22)

# Missing cities; Chicago, Bethel, Philly
# Chicago
missing_chi22 <- setlist22 %>%
  filter(FID_1 == 63)
missing_chi22_df <- as.data.frame(missing_chi22)
tour22_with_setlist <- tour22_with_setlist %>%
  dplyr::left_join(missing_chi22_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Philly
missing_phi22 <- setlist22 %>%
  filter(FID_1 == 2987)
missing_phi22_df <- as.data.frame(missing_phi22)
tour22_with_setlist <- tour22_with_setlist %>%
  dplyr::left_join(missing_phi22_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Bethel
missing_beth22 <- setlist22 %>%
  filter(FID_1 == 9988)
missing_beth22_df <- as.data.frame(missing_beth22)
tour22_with_setlist <- tour22_with_setlist %>%
  dplyr::left_join(missing_beth22_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
#############################################
# Extract latitude and longitude from geometry column
tour22_with_setlist$latitude <- st_coordinates(tour22_with_setlist$geometry)[, 2]
tour22_with_setlist$longitude <- st_coordinates(tour22_with_setlist$geometry)[, 1]

map22 <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "ESRI") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "CartoDB") %>%
  addPolygons(data = states, color = "grey", fill = FALSE, weight = 2,
              highlight = highlightOptions(weight = 3, color = "white", bringToFront = FALSE), popup = FALSE) %>%
  addCircleMarkers(data = tour22_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~setlist, group = "Tour22",
                   color = color_palette[as.numeric(factor(tour22_with_setlist$Location))]) %>%
  setView(lat = 39, lng = -100, zoom = 4) %>%
  addLayersControl(
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    options = layersControlOptions(collapsed = FALSE))


encore_songs22 <- tour22_with_setlist[tour22_with_setlist$Set.Type == "Encore", ]
encore_songs22 <- sf::st_drop_geometry(encore_songs22)

encore_agg22 <- encore_songs22 %>%
  group_by(Location) %>%
  summarize(encore_setlist = paste(Song, collapse = "<br>"))

# Merge aggregated encore songs with tour23_with_setlist
tour22_with_setlist <- left_join(tour22_with_setlist, encore_agg22, by = "Location")

map22 <- map22 %>%
  addCircleMarkers(data = tour22_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~encore_setlist, group = "Encore Songs",
                   color = encore_palette)

# Add layers control
map22 <- map22 %>%
  addLayersControl(
    overlayGroups = c("Tour22", "Encore Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )



opening_songs22 <- tour22_with_setlist[tour22_with_setlist$Song.Order == 1, ]
opening_songs22 <- sf::st_drop_geometry(opening_songs22)

opening_agg22 <- opening_songs22 %>%
  group_by(Location) %>%
  summarize(opening_setlist = paste(Song, collapse = "<br>"))

tour22_with_setlist <- left_join(tour22_with_setlist, opening_agg22, by = "Location")

map22 <- map22 %>%
  addCircleMarkers(data = tour22_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~opening_setlist, group = "Opening Songs",
                   color = opening_palette)
map22 <- map22 %>%
  addLayersControl(
    overlayGroups = c("Tour22", "Encore Songs", "Opening Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )


map22
##########################################################################################
# 10. 2023 map
tour23 <- read_csv("summer_tours/summer2023.csv")

summer23_cities <- cities %>%
  filter(FID_1 == 2132|FID_1 == 1164|FID_1 == 910|FID_1 == 3592|
           FID_1 == 1879|FID_1 == 2264|FID_1 == 2318|FID_1 == 9998|
           FID_1 == 9997|FID_1 == 3296|FID_1 == 63|FID_1 == 2382|
           FID_1 == 2987|FID_1 == 2215|FID_1 == 2174|FID_1 == 1437 |
           FID_1 == 301|FID_1 == 661|FID_1 == 9999|FID_1 == 554)

summer23_cities$FID_1[18:20] <- 0

summer23_cities <- summer23_cities %>%
  filter(!FID_1 == 0)

tour23$Show_num <- as.factor(tour23$Show_num)
levels(tour23$Show_num)

setlists23 <- tour23 %>%
  group_by(Show_num) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

# Change George FID_1
tour23_with_geometry <- left_join(tour23, summer23_cities[, c("FID_1", "geometry")], by = "FID_1")

# Extract latitude and longitude from geometry column
tour23_with_geometry$latitude <- st_coordinates(tour23_with_geometry$geometry)[, 2]
tour23_with_geometry$longitude <- st_coordinates(tour23_with_geometry$geometry)[, 1]

tour23_with_geometry <- tour23_with_geometry[complete.cases(tour23_with_geometry$latitude, tour23_with_geometry$longitude), ]

# Convert tour22_with_geometry to sf object
tour23_with_geometry_sf <- st_as_sf(tour23_with_geometry, coords = c("longitude", "latitude"), crs = 4326)

# Transform the sf object to WGS84
tour23_with_geometry_sf <- st_transform(tour23_with_geometry_sf, crs = 4326)

# Group the songs by FID_1 and concatenate them into a single string
setlist23 <- tour23_with_geometry_sf %>%
  group_by(FID_1) %>%
  summarize(setlist = paste(Song, collapse = "<br>"))

# Merge the setlist data with the tour23_with_geometry_sf
tour23_with_setlist <- st_join(tour23_with_geometry_sf, setlist23)

# Missing cities; Rahleigh, Chicago, Philly, Boston, George
# Chicago
missing_chi23 <- setlist23 %>%
  filter(FID_1 == 63)
missing_chi23_df <- as.data.frame(missing_chi23)
tour23_with_setlist <- tour23_with_setlist %>%
  dplyr::left_join(missing_chi23_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Philly
missing_phi23 <- setlist23 %>%
  filter(FID_1 == 2987)
missing_phi23_df <- as.data.frame(missing_phi23)
tour23_with_setlist <- tour23_with_setlist %>%
  dplyr::left_join(missing_phi23_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Raeligh
missing_nc23 <- setlist23 %>%
  filter(FID_1 == 2318)
missing_nc23_df <- as.data.frame(missing_nc23)
tour23_with_setlist <- tour23_with_setlist %>%
  dplyr::left_join(missing_nc23_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# George
missing_geo23 <- setlist23 %>%
  filter(FID_1 == 9999)
missing_geo23_df <- as.data.frame(missing_geo23)
tour23_with_setlist <- tour23_with_setlist %>%
  dplyr::left_join(missing_geo23_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)
# Boston
missing_bos23 <- setlist23 %>%
  filter(FID_1 == 1437)
missing_bos23_df <- as.data.frame(missing_bos23)
tour23_with_setlist <- tour23_with_setlist %>%
  dplyr::left_join(missing_bos23_df[, c("FID_1", "setlist")], by = c("FID_1.x" = "FID_1")) %>%
  dplyr::mutate(setlist = ifelse(!is.na(setlist.y), setlist.y, setlist.x)) %>%
  dplyr::select(-setlist.x, -setlist.y)

# Extract latitude and longitude from geometry column
tour23_with_setlist$latitude <- st_coordinates(tour23_with_setlist$geometry)[, 2]
tour23_with_setlist$longitude <- st_coordinates(tour23_with_setlist$geometry)[, 1]

map23 <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "ESRI") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "CartoDB") %>%
  addPolygons(data = states, color = "grey", fill = FALSE, weight = 2,
              highlight = highlightOptions(weight = 3, color = "white", bringToFront = FALSE), popup = FALSE) %>%
  addCircleMarkers(data = tour23_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~setlist, group = "Tour23",
                   color = color_palette[as.numeric(factor(tour23_with_setlist$Location))]) %>%
  setView(lat = 39, lng = -100, zoom = 4) %>%
  addLayersControl(
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    options = layersControlOptions(collapsed = FALSE))


encore_songs23 <- tour23_with_setlist[tour23_with_setlist$`Set Type` == "Encore", ]
encore_songs23 <- sf::st_drop_geometry(encore_songs23)

encore_agg23 <- encore_songs23 %>%
  group_by(Location) %>%
  summarize(encore_setlist = paste(Song, collapse = "<br>"))

# Merge aggregated encore songs with tour23_with_setlist
tour23_with_setlist <- left_join(tour23_with_setlist, encore_agg23, by = "Location")

map23 <- map23 %>%
  addCircleMarkers(data = tour23_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~encore_setlist, group = "Encore Songs",
                   color = encore_palette)

# Add layers control
map23 <- map23 %>%
  addLayersControl(
    overlayGroups = c("Tour23", "Encore Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )


opening_songs23 <- tour23_with_setlist[tour23_with_setlist$`Song Order` == 1, ]
opening_songs23 <- sf::st_drop_geometry(opening_songs23)

opening_agg23 <- opening_songs23 %>%
  group_by(Location) %>%
  summarize(opening_setlist = paste(Song, collapse = "<br>"))

tour23_with_setlist <- left_join(tour23_with_setlist, opening_agg23, by = "Location")

map23 <- map23 %>%
  addCircleMarkers(data = tour23_with_setlist, radius = 8, stroke = FALSE, fillOpacity = 1,
                   lat = ~latitude, lng = ~longitude, popup = ~opening_setlist, group = "Opening Songs",
                   color = opening_palette)
map23 <- map23 %>%
  addLayersControl(
    overlayGroups = c("Tour23", "Encore Songs", "Opening Songs"),
    options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("OSM", "ESRI", "CartoDB"),
    position = "topright"
  )


map23

################################################################################################
