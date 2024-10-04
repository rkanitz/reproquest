# Load necessary libraries
library(ggplot2)
library(dplyr)
library(cluster)  # For k-means and hierarchical clustering
library(factoextra)  # For visualizing clusters

# Load the dataset
coffee_data <- read.csv("coffee_data_complete.csv")

# Omit NAs (as you mentioned you've handled this)
coffee_data <- na.omit(coffee_data)

# Select relevant variables for clustering
# (You might want to adjust these based on your game design)
cluster_data <- coffee_data %>%
  select(cups_per_day, spend_per_week, age)

# --- Option A: K-means clustering ---

# Determine the optimal number of clusters (using the elbow method)
# (You might want to adjust the range of k values)
set.seed(42)  # For reproducibility
fviz_nbclust(cluster_data, kmeans, method = "wss", k.max = 10) +
  labs(title = "Elbow Method for Optimal k")

# Perform k-means clustering (let's say we choose k = 4)
set.seed(42)
kmeans_result <- kmeans(cluster_data, centers = 4)

# Visualize the clusters
plot_a <- fviz_cluster(kmeans_result, data = cluster_data,
                       geom = "point", ellipse.type = "convex",
                       ggtheme = theme_minimal()) +
  labs(title = "K-means Clustering (k = 4)")

# Save the plot
ggsave("challenge4_plot_a.png", plot_a)

# --- Option B: Hierarchical clustering ---

# Calculate the distance matrix
distance_mat <- dist(cluster_data, method = "euclidean")

# Perform hierarchical clustering (using Ward's method)
hclust_result <- hclust(distance_mat, method = "ward.D2")

# Cut the dendrogram to create clusters (let's say we want 4 clusters)
cut_tree_result <- cutree(hclust_result, k = 4)

# Visualize the clusters
plot_b <- fviz_cluster(list(data = cluster_data, cluster = cut_tree_result),
                       geom = "point", ellipse.type = "convex",
                       ggtheme = theme_minimal()) +
  labs(title = "Hierarchical Clustering (4 Clusters)")

# Save the plot
ggsave("challenge4_plot_b.png", plot_b)