# ----------------------------
# GROUP 3 : K-MEANS CLUSTERING
# ----------------------------
# Load required packages
library("dplyr")
library("cluster")
library("ggplot2")
library("purrr")

# Read the cleaned dataset
data <- read.csv("bigmartnew.csv")


# Feature selection: Select only continuous numerical columns
data.item <- data.frame(data$Item_Visibility, data$Item_MRP)
#data.item <- data.frame(data$Item_Weight, data$Item_Visibility, data$Item_MRP)

# Normalize the data since the units for each columns are different
data.item <- tbl_df(scale(data.item))

##-- Finding best k using Elbow method --##
# -----------------------------------------
# Use map_dbl to simulate many models with varying value of k from 1 to 10
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = data.item, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

# Plot the elbow plot to find the first kink (best # of k)
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)
# -----------------------------------------

# Build a kmeans model
c=3
kmodel.data.item <- kmeans(data.item, centers = c)

# Extract the cluster assignment vector from the kmeans model
clust.data.item <- kmodel.data.item$cluster

# Create a new dataframe appending the cluster assignment
segment.data.item <- mutate(data.item, cluster = clust.data.item)

# Count the number of observations per cluster
count(segment.data.item, cluster)

# Plot the scatterplot to visualise the clusters
ggplot(segment.data.item, aes(x = data$Item_Visibility, y = data$Item_MRP, color = factor(cluster))) +
  geom_point()

# Interpret result using tabulated average values
segment.data.item %>% 
  group_by(cluster) %>% 
  summarise_all(funs(mean(.)))
