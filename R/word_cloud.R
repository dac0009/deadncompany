# Creating word clouds for each tour. Goal is to have these along side their respective map on the app
# Load Required Packages
library(tidyverse)
library(circlize)
library(wordcloud)
library(gridExtra)
library(grid)
######################
# Define color palette

cloud_palette <- c("#FF0000", "#FF7F00", "#00FF00", "#00FFFF", "#0000FF", 
                   "#8B00FF", "#FF00FF", "#FF1493", "#FF4500", "#FFD700", 
                    "#32CD32", "#00CED1", "#4169E1", "#800080", "#FFC0CB", 
                    "#00FF7F", "#FFA500", "#FFFF00", "#8A2BE2", "#00BFFF", 
                    "#FF69B4", "#7FFF00", "#FF6347", "#FFA07A", "#8FBC8F")
#######################
# Read in tour data   
tour16 <- read.csv("summer_tours/summer2016.csv")
tour17 <- read.csv("summer_tours/summer2017.csv")
tour18 <- read.csv("summer_tours/summer2018.csv")
tour19 <- read.csv("summer_tours/summer2019.csv")
tour21 <- read.csv("summer_tours/summer2021.csv")
tour22 <- read.csv("summer_tours/summer2022.csv")
tour23 <- read_csv("summer_tours/summer2023.csv")
########################
# Filter out Drums & Space
summer16_no_ds <- tour16[!(tour16$Song %in% c("Drums", "Space")), ]
summer17_no_ds <- tour17[!(tour17$Song %in% c("Drums", "Space")), ]
summer18_no_ds <- tour18[!(tour18$Song %in% c("Drums", "Space")), ]
summer19_no_ds <- tour19[!(tour19$Song %in% c("Drums", "Space")), ]
summer21_no_ds <- tour21[!(tour21$Song %in% c("Drums", "Space")), ]
summer22_no_ds <- tour22[!(tour22$Song %in% c("Drums", "Space")), ]
summer23_no_ds <- tour23[!(tour23$Song %in% c("Drums", "Space")), ]
#######################
# Calculate song frequency 
song_freq16 <- summer16_no_ds %>%
  count(Song) %>%
  arrange(desc(n))
song_freq17 <- summer17_no_ds %>%
  count(Song) %>%
  arrange(desc(n))
song_freq18 <- summer18_no_ds %>%
  count(Song) %>%
  arrange(desc(n))
song_freq19 <- summer19_no_ds %>%
  count(Song) %>%
  arrange(desc(n))
song_freq21 <- summer21_no_ds %>%
  count(Song) %>%
  arrange(desc(n))
song_freq22 <- summer22_no_ds %>%
  count(Song) %>%
  arrange(desc(n))
song_freq23 <- summer23_no_ds %>%
  count(Song) %>%
  arrange(desc(n))

str(song_freq16)
str(song_freq17)
str(song_freq18)
str(song_freq19)

# Set parameters for the word cloud 
max_words <- 130

background_color <- "#39312F"

par(bg = background_color)

# Creating the word cloud for each tour
song_cloud16 <- wordcloud(words = song_freq16$Song[1:max_words], freq = song_freq16$n[1:max_words],
                        scale = c(1.5, 0.3), colors = encore_palette, bg = background_color, rot.per = 0.2,
                        random.order = FALSE, min.freq = 1, max.words = max_words,
                        random.color = TRUE, rotate.min = -45, rotate.max = 45,
                        ordered.colors = FALSE, use.r.layout = FALSE, asp = 0.75,
                        family = "plain", par.settings = list(cex = 0.8, col = "darkblue"))

song_cloud17 <- wordcloud(words = song_freq17$Song[1:max_words], freq = song_freq17$n[1:max_words],
                          scale = c(1.5, 0.3), colors = encore_palette, bg = background_color, rot.per = 0.2,
                          random.order = FALSE, min.freq = 1, max.words = max_words,
                          random.color = TRUE, rotate.min = -45, rotate.max = 45,
                          ordered.colors = FALSE, use.r.layout = FALSE, asp = 0.75,
                          family = "plain", par.settings = list(cex = 0.8, col = "darkblue"))

song_cloud18 <- wordcloud(words = song_freq18$Song[1:max_words], freq = song_freq18$n[1:max_words],
                          scale = c(1.5, 0.3), colors = encore_palette, bg = background_color, rot.per = 0.2,
                          random.order = FALSE, min.freq = 1, max.words = max_words,
                          random.color = TRUE, rotate.min = -45, rotate.max = 45,
                          ordered.colors = FALSE, use.r.layout = FALSE, asp = 0.75,
                          family = "plain", par.settings = list(cex = 0.8, col = "darkblue"))

song_cloud19 <- wordcloud(words = song_freq19$Song[1:max_words], freq = song_freq19$n[1:max_words],
                          scale = c(1.5, 0.3), colors = encore_palette, bg = background_color, rot.per = 0.2,
                          random.order = FALSE, min.freq = 1, max.words = max_words,
                          random.color = TRUE, rotate.min = -45, rotate.max = 45,
                          ordered.colors = FALSE, use.r.layout = FALSE, asp = 0.75,
                          family = "plain", par.settings = list(cex = 0.8, col = "darkblue"))
 
song_cloud21 <- wordcloud(words = song_freq21$Song[1:max_words], freq = song_freq21$n[1:max_words],
                          scale = c(1.5, 0.3), colors = encore_palette, bg = background_color, rot.per = 0.2,
                          random.order = FALSE, min.freq = 1, max.words = max_words,
                          random.color = TRUE, rotate.min = -45, rotate.max = 45,
                          ordered.colors = FALSE, use.r.layout = FALSE, asp = 0.75,
                          family = "plain", par.settings = list(cex = 0.8, col = "darkblue"))

song_cloud22 <- wordcloud(words = song_freq22$Song[1:max_words], freq = song_freq22$n[1:max_words],
                          scale = c(1.5, 0.3), colors = encore_palette, bg = background_color, rot.per = 0.2,
                          random.order = FALSE, min.freq = 1, max.words = max_words,
                          random.color = TRUE, rotate.min = -45, rotate.max = 45,
                          ordered.colors = FALSE, use.r.layout = FALSE, asp = 0.75,
                          family = "plain", par.settings = list(cex = 0.8, col = "darkblue"))

song_cloud23 <- wordcloud(words = song_freq23$Song[1:max_words], freq = song_freq23$n[1:max_words],
                          scale = c(1.5, 0.3), colors = encore_palette, bg = background_color, rot.per = 0.2,
                          random.order = FALSE, min.freq = 1, max.words = max_words,
                          random.color = TRUE, rotate.min = -45, rotate.max = 45,
                          ordered.colors = FALSE, use.r.layout = FALSE, asp = 0.75,
                          family = "plain", par.settings = list(cex = 0.8, col = "darkblue"))

# Combine song frequency data from multiple years
all_song_freq <- rbind(song_freq16, song_freq17, song_freq19,song_freq21, song_freq22, song_freq23)
# Aggregate total frequency for each song
total_freq <- aggregate(n ~ Song, data = all_song_freq, sum)
# Sort the data frame by total frequency in descending order
total_freq <- arrange(total_freq, desc(n))
summary(total_freq)
str(total_freq)
######################################################################################################
color_palette2 <- c("#0000FF", "#E7298A", "#E63946", "#00BFFF", "#1D3557", "#FFC300",
                    "#F3722C", "#FF6F91", "#9A031E", "#03A696", "#06D6A0", "#FF5733",
                    "#C70039", "#900C3F", "#FDDF50", "#1287A5", "#F09727")

max_words2 <- 156

total_cloud <- wordcloud(words = total_freq$Song[1:max_words2], freq = total_freq$n[1:max_words2],
                         scale = c(1.5, 0.3), colors = color_palette2, bg = "white", rot.per = 0.2,
                         random.order = FALSE, min.freq = 1, max.words = max_words2,
                         random.color = TRUE, rotate.min = -45, rotate.max = 45,
                         ordered.colors = FALSE, use.r.layout = FALSE, asp = 0.75,
                         family = "plain", par.settings = list(cex = 0.8, col = "darkblue"))

##################################################################################################

print(total_freq)

# Assuming total_freq is the data frame containing total song occurrences
# Replace 10 with the number of top songs you want to display
top_n_songs <- total_freq[1:50, ]

# Create the bar plot
bar_plot <- ggplot(top_n_songs, aes(x = reorder(Song, n), y = n)) +
  geom_bar(stat = "identity", fill = "#457B9D") +
  labs(x = "Song", y = "Total Occurrences", title = "Top 50 Songs by Total Occurrences",
       subtitle = "Across All Seven Tours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "italic"))

print(bar_plot)


str(top_n_songs)



str(all_tours)

all_encore <- all_tours %>%
  filter(Set.Type == "Encore") %>%
  select(Song, Set.Type)
str(all_encore)
library(dplyr)

# Group the data by the Song and Set.Type columns and calculate the number of times each song was played as an Encore
all_encore <- all_encore %>%
  group_by(Song, Set.Type) %>%
  summarise(times_played = n())

# Print the modified dataframe
print(all_encore)

all_openers <- all_tours %>%
  filter(Song.Order == 1) %>%
  select(Song, Song.Order)
str(all_openers)
library(dplyr)

# Group the data by the Song and Set.Type columns and calculate the number of times each song was played as an Encore
all_openers <- all_openers %>%
  group_by(Song, Song.Order) %>%
  summarise(times_played = n())

# Print the modified dataframe
print(all_openers)

