library(tidyverse)
library(circlize)
library(wordcloud)
library(gridExtra)
library(grid)

tour23 <- read_csv("summer_tours/summer2023.csv")
str(tour23)


summer23_no_ds <- tour23[!(tour23$Song %in% c("Drums", "Space")), ]

song_freq23 <- summer23_no_ds %>%
  count(Song) %>%
  arrange(desc(n))

str(song_freq23)

max_words <- 118

background_color <- "#39312F"

par(bg = background_color)

song_cloud <- wordcloud(words = song_freq23$Song[1:max_words], freq = song_freq23$n[1:max_words],
                        scale = c(1.5, 0.3), colors = encore_palette, bg = background_color, rot.per = 0.2,
                        random.order = FALSE, min.freq = 1, max.words = max_words,
                        random.color = TRUE, rotate.min = -45, rotate.max = 45,
                        ordered.colors = FALSE, use.r.layout = FALSE, asp = 0.75,
                        family = "plain", par.settings = list(cex = 0.8, col = "darkblue"))


##################################################################################
# Display the most played songs (songs that were played 5-10 times) on a bar chart
# Subset the songs played 5-10 times as top songs
top_n_songs <- song_freq23 %>%
  filter(n >= 5 & n <= 10)

my_colors <- c("#E2D200", "#Ff3300", "#990033")

# Categorize the songs based on frequency using ifelse()
top_n_songs$category <- ifelse(top_n_songs$n >= 9, "9+", ifelse(top_n_songs$n >= 6, "6-8", "5"))

# Create a bar plot of the top songs with custom colors
top_songs_bar_plot <- ggplot(top_n_songs, aes(x = Song, y = n, fill = category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = my_colors) +
  labs(title = "Frequency of Top Songs", x = "Song", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the bar plot
print(top_songs_bar_plot)


# Display the least played songs (songs played 1-4 times) on a bar chart
scale_fill_manual(values = rare_colors[1:4])

rare_songs <- song_freq23 %>%
  filter(n >= 1 & n <= 4)

rare_colors <- c("#85D4E3", "#F3DF6C", "#F8AFA8", "#FD6467")

rare_songs$category <- ifelse(rare_songs$n == 4, "4", ifelse(rare_songs$n >= 2, "2-3", "1"))

rare_songs_bar_plot <- ggplot(rare_songs, aes(x = Song, y = n, fill = category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = rare_colors[1:4]) +
  labs(title = "Frequency of Rare Songs", x = "Song", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(rare_songs_bar_plot)


####################################################################################
# Messy Pie Chart
install.packages("plotly")
library(plotly)

# Create the interactive pie chart
pie_chart <- plot_ly(song_freq23, labels = ~Song, values = ~n, type = "pie",
                     hoverinfo = "text", text = ~paste(Song, "<br>", "Times Played:", n))

# Customize the pie chart layout
pie_chart <- pie_chart %>% layout(title = "Song Occurances",
                                  showlegend = FALSE,
                                  hoverlabel = list(bgcolor = "white"))

# Print the interactive pie chart
print(pie_chart)

####################################################################################

# Clean Pie Chart
# Create the interactive pie chart
pie_chart <- plot_ly(song_freq23, labels = ~Song, values = ~n, type = "pie",
                     hoverinfo = "label+text",
                     text = ~paste("Times Played:", n),
                     textinfo = "none")

# Customize the pie chart layout
pie_chart <- pie_chart %>% layout(title = "Song Occurrences",
                                  showlegend = FALSE,
                                  hoverlabel = list(bgcolor = "white"))

# Print the interactive pie chart
print(pie_chart)





#############################
# Interactive Pie Chart not working

# Create the interactive pie chart
pie_chart <- plot_ly(song_freq23, labels = ~Song, values = ~n, type = "pie",
                     hoverinfo = "label+text",
                     text = ~paste("Times Played:", n),
                     textinfo = "none")

# Customize the pie chart layout
pie_chart <- pie_chart %>% layout(title = "Song Occurrences",
                                  showlegend = FALSE,
                                  hoverlabel = list(bgcolor = "white"),
                                  animation = list(
                                    duration = 1000,  # Duration of the animation in milliseconds
                                    easing = "cubic-in-out",  # Easing function for the animation
                                    fromcurrent = TRUE,  # Continue animation from the current rotation angle
                                    rotation = 90,  # Initial rotation angle in degrees
                                    redraw = FALSE  # Redraw chart only once when animation is complete
                                  ))

# Print the interactive pie chart
print(pie_chart)






