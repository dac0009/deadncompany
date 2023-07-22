# Cleaning up the spatial data

# Sicne the cities dataset was pulled from another ARCGIS Online users profile it was missing some cities 

# So I had to go in ArcPro myself and add spatial points for these cities. The issue is they all share the same FID so we need to fix that 
states <- st_read("states/states.shp")
cities <- st_read("USA_Cities/US_Cities.shp")

missing_points <- st_read("missing_points/bethel_wheatland.shp")

##################################################################
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
##########################################################












