# ---------------------------------
# GROUP 3 : HIERARCHICAL CLUSTERING
# ---------------------------------
# Load required packages
library("dplyr")
library("dendextend")

# Read the cleaned dataset
data <- read.csv("bigmartnew.csv")

# Feature selection: Select only continuous numerical columns
data.item <- data.frame(data$Item_Visibility, data$Item_MRP)
#data.item <- data.frame(data$Item_Weight, data$Item_Visibility, data$Item_MRP)
data.item
# Normalize the data since the units for each columns are different
data.item <- tbl_df(scale(data.item))

# Calculate the distance between the items using Euclidean distance measurement
dist.data.item <- dist(data.item, method='euclidean')

# Perform the hierarchical clustering on the distance-calculated dataset
hc.data.item <- hclust(dist.data.item)

# Plot the dendrogram (wait awhile for the dendrogram to appear)
plot(hc.data.item)

# Define k(cluster) or height(h) and then segment data into those clusters
#k=4
h=5

clust.data.item <- cutree(hc.data.item, h=h)
#clust.data.item <- cutree(hc.data.item, k=k)
segment.data.item <- mutate(data.item, cluster=clust.data.item)

# Count the number of observations per cluster
count(segment.data.item, cluster)


# Plot the dendrogram (wait awhile for the dendrogram to appear)
dend.data.item <- as.dendrogram(hc.data.item)
dend_colored.data.item <- color_branches(dend.data.item, h=h)
#dend_colored.data.item <- color_branches(dend.data.item, k=k)
plot(dend_colored.data.item)

# Interpret result using tabulated average values
segment.data.item %>% 
  group_by(cluster) %>% 
  summarise_all(funs(mean(.)))
