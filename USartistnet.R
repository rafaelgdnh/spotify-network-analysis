## Rafael Godinho ##
## Tie Formation in US Spotify Artist Network ##


# Clear workspace
rm(list = ls())

# Set working directory
setwd("~spotify-network-analysis")

library(igraph)
library(network)
library(grDevices)
library(ergm)
library(ggplot2)

# Read CSV files for each year
spotify_data_2017 = read.table("data/us-artist_network-2017.csv", header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
spotify_data_2018 = read.table("data/us-artist_network-2018.csv", header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
spotify_data_2019 = read.table("data/us-artist_network-2019.csv", header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
spotify_artists_info = read.table("data/spotify_artists_info_complete_reduced_genres.csv", header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)

# Merge data
merged_data = rbind(spotify_data_2017, spotify_data_2018, spotify_data_2019)


# Create graph
graph = graph_from_data_frame(merged_data, directed = FALSE)

# Adjust vertex size based on degree
vertex_size = ifelse(degree(graph) > 10, 5, 2)  

# Modify layout algorithm
l1 = layout_with_kk(graph)
l2 = layout_with_kk(graph)

V(graph)$color[degree(graph) > 8] = "red"
V(graph)$color[degree(graph) == 1] = "blue"
V(graph)$color[is.na(V(graph)$color)] = "green"

# Plot the graph 
plot(graph, layout = l1, vertex.label = NA)
plot(graph, layout = l2, vertex.label = NA, vertex.size = vertex_size)


# Create edgelist matrix
edgelist = as.matrix(merged_data[, c("artist_1", "artist_2")])

# Create network object
artist_network = network(edgelist, directed = FALSE)

# Merge artist information with merged data based on name
merged_data = merge(merged_data, spotify_artists_info, by.x = "artist_1", by.y = "name", all.x = TRUE)
merged_data = merge(merged_data, spotify_artists_info, by.x = "artist_2", by.y = "name", all.x = TRUE)

# Create vertex attributes for genres and popularity
## Remove single quotes from genres.x
merged_data$genres.x = gsub("'", "", merged_data$genres.x) 

## Remove single quotes from genres.y
merged_data$genres.y = gsub("'", "", merged_data$genres.y) 

# Combine genres.x and genres.y into single attribute and remove duplicates
merged_data$genres = paste(merged_data$genres.x, merged_data$genres.y, sep = ", ")
merged_data$genres = gsub("\\[|\\]", "", merged_data$genres) 
merged_data$genres = sapply(strsplit(merged_data$genres, ",\\s*"), function(x) paste(unique(x), collapse = ", "))

# Combine popularity.x and popularity.y into single attribute and remove duplicates
merged_data$popularity = pmax(merged_data$popularity.x, merged_data$popularity.y, na.rm = TRUE)

# Set vertex attributes for hit songs, genres and popularity
set.vertex.attribute(artist_network, "hit_songs", merged_data$count)
set.vertex.attribute(artist_network, "genres", merged_data$genres)
set.vertex.attribute(artist_network, "popularity", merged_data$popularity)

# Fit updated ERGM model
model = ergm(artist_network ~ edges + nodematch("hit_songs") + nodematch("popularity") + nodematch("genres"))

# View model summary
model_summary = summary(model)
model_summary


# Get coefficient estimates and standard errors from model summary
coefs = coef(model)
se = sqrt(diag(vcov(model)))

# Create a data frame for plotting
coefs_df = data.frame(Coefficient = names(coefs), Estimate = coefs, SE = se)

# Create coefficient plot 
ggplot(coefs_df, aes(x = Coefficient, y = Estimate)) +
  geom_bar(stat = "identity", fill = "forestgreen", color = "black") +
  geom_errorbar(aes(ymin = Estimate - 2 * SE, ymax = Estimate + 2 * SE), width = 0.2, color = "black") +
  coord_flip() +
  xlab("Coefficient") +
  ylab("Estimate") +
  ggtitle("Coefficient Plot")


# Find artist with lowest degree
lowest_degree_artist = names(degree_counts)[order(degree_counts)[1]]
lowest_degree = degree_counts[lowest_degree_artist]

# Print top 5 artists with highest degree
for (i in 1:5) {
  print(paste(top_5_artists[i]))
  print(paste(top_5_degrees[i]))
}

# Print artist with lowest degree
print(lowest_degree_artist)
print(lowest_degree)


# Split genres attribute into separate genre labels
genre_labels = strsplit(merged_data$genres, ", ")

# Create table to count the occurrences of each genre
genre_counts = table(unlist(genre_labels))

# Sort genre counts in descending order
sorted_genre_counts = sort(genre_counts, decreasing = TRUE)

# Set up plot area
par(mar = c(5, 7, 4, 10))  

# Plot the genre bar plot
barplot(sorted_genre_counts, horiz = TRUE, las = 1, main = "Genre Distribution",
        xlab = "Frequency", xlim = c(0, max(sorted_genre_counts) + 100),
        col = "steelblue")


# Get top 5 genres
top_5_genres = names(sorted_genre_counts)[1:5]

# Print top 5 genres
for (genre in top_5_genres) {
  print(genre)
}


# Sort artist counts in descending order
sorted_counts = artist_counts[order(artist_counts$total_count, decreasing = TRUE), ]

# Get pair of artists with highest count 
top_collaborators = head(sorted_counts, 5)[c("artist_1", "artist_2", "total_count")]

# Print pair
print(top_collaborators)


# Calculate degree of each artist in network
degree_values = degree(graph)

# Create degree distribution plot
hist(degree_values, breaks = 20, col = "#3CB371", xlab = "Degree", ylab = "Frequency", main = "Degree Distribution")


# Top 5 Popular Artists:

#1 Bad Bunny		100	['latin', 'reggaeton', 'trap']
#2 The Weeknd		98	['r&b', 'pop']
#3 Drake		98	['rap', 'pop rap', 'pop', 'hip hop']
#4 J Balvin		97	['latin', 'reggaeton']
#5 Billie Eilish		95	['pop', 'electropop']

# Top 5 Artists With Highest Degree:

#1 Quavo 71 ['pop rap', 'rap', 'trap', 'hip hop']
#2 Nicki Minaj 70 ['pop', 'pop rap', 'dance pop', 'hip hop']
#3 Future 69 ['pop rap', 'rap', 'trap', 'hip hop']
#4 Travis Scott 69 ['rap']
#5 Gucci Mane 65 ['pop rap', 'rap', 'trap', 'hip hop']

# Top 5 Genres within all collaborations 

#1 Hip hop
#2 rap
#3 pop rap
#4 trap
#5 pop

# Most Successful Collaborations

#1 Gunna Lil Baby ['rap', 'trap', 'hip hop'] 34
#2 Quavo Travis Scott 25
#3 21 Savage ['rap', 'trap', 'hip hop'] Metro Boomin ['pop rap', 'rap', 'trap', 'hip hop'] 21
