# deadncompany
A look at Dead & Company's seven years of touring the United States. 

Produced maps ONLY ACCOUNT FOR SUMMER TOURS

Fall & PITS tour data is available -- if not please contact me 

ISSUES:
The code for the tmap does not actually give you # of shows played in each state. It gives you how many times they have visited each state!

Solution: 
# Count the number of shows played in each state
state_counts <- table(all_cities$ST)

state_counts_df <- all_tours %>%
  group_by(STATE_ABBR) %>%
  summarize(num_shows = n_distinct(Show_num))

# Print the state_counts_df data frame
print(state_counts_df)

# Remove duplicate rows based on the "NAME" column
unique_cities <- all_cities %>% distinct(NAME, .keep_all = TRUE)

# Print the unique cities data frame
print(unique_cities)

# Rename the "STATE_ABBR" column to "ST" in state_counts_df
names(state_counts_df)[names(state_counts_df) == "STATE_ABBR"] <- "ST"

# Merge the state_counts_df into the unique_cities data frame based on the 'ST' column
unique_cities_counts <- merge(unique_cities, state_counts_df, by = "ST", all.x = TRUE)

# Print the unique_cities_counts data frame
print(unique_cities_counts)

# Convert unique_cities_counts data frame to an sf object
unique_cities_counts_sf <- st_as_sf(unique_cities_counts, coords = c("longitude", "latitude"))

# Check CRS of states
st_crs(states)
# Check CRS of unique_cities_counts_sf
st_crs(unique_cities_counts_sf)

# Reproject unique_cities_counts_sf to match the CRS of states
unique_cities_counts_sf <- st_transform(unique_cities_counts_sf, crs = st_crs(states))

# Perform the left_join to merge unique_cities_counts_sf into states based on STATE_ABBR and ST
merged_states <- st_join(states, unique_cities_counts_sf)

# Replace NA values in num_shows column with 0
merged_states$num_shows[is.na(merged_states$num_shows)] <- 0

# Print the first few rows of the resulting sf object
print(merged_states)
merged_states$num_shows <- as.factor(merged_states$num_shows)
levels(merged_states$num_shows)
