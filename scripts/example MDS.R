# https://www.geeksforgeeks.org/multidimensional-scaling-using-r/

### Simple Analysis of Multidimensional Scaling in R

# Load necessary libraries
library(cluster)

# Load the iris data set
data(iris)

# Perform MDS analysis
mds_iris <- cmdscale(dist(iris[, 1:4]))

# Plot the results
plot(mds_iris[, 1], mds_iris[, 2], 
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# Plot the points and label them with 
# the first two letters of the species name
points(mds_iris[, 1], mds_iris[, 2], 
       pch = 21, bg = "lightblue")
text(mds_iris[, 1], mds_iris[, 2], 
     labels = substr(iris$Species, 1, 2), 
     pos = 3, cex = 0.8)

# Form clusters using K-means clustering (specify the number of clusters, e.g., 3)
kmeans_clusters <- kmeans(mds_iris, centers = 3)$cluster

# Add the cluster information to the plot
points(mds_iris[, 1], mds_iris[, 2], 
       pch = 21, bg = kmeans_clusters, cex = 1.2)

### K-means Clustering of MDS Iris Data
# Load necessary libraries
library(cluster)
library(ggpubr)
library(ggrepel)

# Load the iris data set
data(iris)

# Perform MDS analysis
mds_iris <- cmdscale(dist(iris[, 1:4]))

# Form clusters using K-means clustering (specify the number of clusters, e.g., 3)
kmeans_clusters <- kmeans(mds_iris, centers = 3)$cluster

# Add cluster information to the MDS results
mds_df <- as.data.frame(mds_iris)
mds_df$groups <- as.factor(kmeans_clusters)
mds_df$species <- iris$Species  # Add species information

# Plot using ggscatter with labels using ggrepel
ggscatter(mds_df, x = "V1", y = "V2",
          color = "groups",
          palette = "jco",
          size = 3,
          ellipse = TRUE,
          ellipse.type = "convex",
          title = "K-means Clustering of MDS Iris Data",
          xlab = "MDS Dimension 1",
          ylab = "MDS Dimension 2") +
  geom_text_repel(aes(label = species), box.padding = 0.5)

### MDS with Custom Distance Matrix
# Load the USArrests data set
data(USArrests)

# Calculate the distance matrix
distance_matrix <- dist(USArrests)

# Perform MDS analysis using
# the distance matrix
mds_usarrests <- cmdscale(distance_matrix)

# Plot the results
plot(mds_usarrests[,1], mds_usarrests[,2],
     type = "n")
text(mds_usarrests[,1], mds_usarrests[,2],
     labels = row.names(USArrests))

### MDS with 3D Plot
# Load the iris data set
data(iris)

# Perform MDS analysis
mds_iris <- cmdscale(dist(iris[,1:4]),
                     k = 3)

# Plot the results
library(scatterplot3d)
colors <- c("red", "blue", "pink")
colors <- colors[as.numeric(iris$Species)]
scatterplot3d(mds_iris[,1:3], pch = 16,
              xlab = "Sepal Length",
              ylab = "Sepal Width",
              zlab = "Petal Length",
              color=colors)


