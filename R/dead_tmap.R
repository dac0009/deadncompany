# All tour map 
# Load required library
library(tidyverse)
library(tmap)
library(tmaptools)

# Rename the columns in 'tour19' and 'tour23' to match the other datasets
colnames(tour19) <- colnames(tour16)
colnames(tour23) <- colnames(tour16)

# Combine all the datasets using rbind()
all_tours <- rbind(tour16, tour17, tour18, tour19, tour21, tour22, tour23)

# Check the structure of the combined dataset
str(all_tours)

# Combine data frames row-wise
all_cities <- rbind(summer16_cities, summer17_cities, summer18_cities,
                    summer19_cities, summer21_cities, summer22_cities)

all_cities$ST[15] <- "VA"
all_cities$ST[16] <- "PA"
all_cities$ST[17] <- "MI"
all_cities$ST[18] <- "WA"
all_cities$ST[19] <- "WI"
all_cities$ST[20] <- "CA"
all_cities$ST[33] <- "VA"
all_cities$ST[34] <- "PA"
all_cities$ST[35] <- "CA"
all_cities$ST[51] <- "NY"
all_cities$ST[52] <- "WA"
all_cities$ST[53] <- "MA"
all_cities$ST[54] <- "WI"
all_cities$ST[65] <- "VA"
all_cities$ST[66] <- "MA"
all_cities$ST[67] <- "WA"
all_cities$ST[68] <- "CA"
all_cities$ST[87] <- "VA"
all_cities$ST[88] <- "MA"
all_cities$ST[89] <- "NY"
all_cities$ST[90] <- "CA"
all_cities$ST[91] <- "MA"
all_cities$ST[92] <- "CO"
all_cities$ST[93] <- "NY"
all_cities$ST[104] <- "VA"
all_cities$ST[105] <- "PA"
all_cities$ST[106] <- "MI"
all_cities$ST[107] <- "MA"
all_cities$ST[108] <- "NY"

str(all_cities)



# Count the number of shows played in each state
state_counts <- table(all_cities$ST)

# Convert the result to a data frame
state_counts_df <- as.data.frame(state_counts,  responseName = "num_shows")

# Rename the 'ST' column to 'State' for consistency
state_counts_df <- rename(state_counts_df, ST = Var1)

# Print the state_counts_df data frame
print(state_counts_df)

# Remove duplicate rows based on the "NAME" column
unique_cities <- all_cities %>% distinct(NAME, .keep_all = TRUE)

# Print the unique cities data frame
print(unique_cities)

# Merge the state_counts_df into the unique_cities data frame based on the 'ST' column
unique_cities_counts <- merge(unique_cities, state_counts_df, by = "ST", all.x = TRUE)

# Print the unique_cities_with_counts data frame
print(unique_cities_counts)

# Convert unique_cities_counts data frame to an sf object
unique_cities_counts_sf <- st_as_sf(unique_cities_counts, coords = c("longitude", "latitude"))

# Perform a left_join to merge unique_cities_counts_sf into states based on STATE_ABBR and ST
merged_states <- st_join(states, unique_cities_counts_sf)

# Replace NA values in num_shows column with 0
merged_states$num_shows[is.na(merged_states$num_shows)] <- 0

# Print the first few rows of the resulting sf object
print(merged_states)
merged_states$num_shows <- as.factor(merged_states$num_shows)
levels(merged_states$num_shows)

############################################

# Create a map with polygons colored based on the 'num_shows' factor

tm_shape(merged_states) +
  tm_polygons(col = "num_shows", palette = "Reds", title = "", legend.show = FALSE) +
  tm_shape(merged_states) +
  tm_fill(col = "num_shows", palette = "Reds", 
  title = "# of concerts", legend.is.portrait = FALSE) +
  tm_shape(states) +
  tm_borders(col = "#000000", lwd = 1) +
  tm_shape(unique_cities_counts) +
  tm_bubbles(size = 0.25, col = "#8A2BE2") +
  tm_layout(legend.outside.position = "bottom",legend.outside.size = 1,legend.outside = TRUE, 
            main.title = "Seven Summers of Dead & Company",
            main.title.position = "center", 
            main.title.fontface = "bold",
            legend.text.size = 0.8)



# Select only the num_shows and STATE_ABBR columns
shows_state_df <- merged_states %>%
  select(num_shows, STATE_ABBR) %>%
  filter(!num_shows == 0)

# View the new dataframe
print(shows_state_df)

