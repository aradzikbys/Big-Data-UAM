# Data analysis - 04 - cluster analysis (number of clusters, hierarchical  &
# non-hierarchical cluster analysis (k-means) cluster analysis. Clusters visualization
# on dendrogram and PCA.)

#########
# For the mtcars dataset, perform a hierarchical cluster analysis (mean binding method)
# and a non-hierarchical cluster analysis (k-means).Draw a dendrogram.
# How many clusters make sense? Determine the CH index and the silhouette factor.
# How much clusters you can suggest based on them?

# Clear environment
rm(list = ls())


###############################################
# 01 Libraries
###############################################

# For fancy dendrogram:
library(dendextend)

# Other
library(dplyr)

# For cascadeKM() command
library(vegan)

# For silhouette() command
library(cluster)

# For ggbiplot:
library(ggbiplot)
library(devtools)
install_github("vqv/ggbiplot")
library(wesanderson)


###############################################
# 02 Data set
###############################################

?mtcars

# Assign data set to variable, check data set (missing values, distribution 
# of classes, types of attributes):
dataset04 <- mtcars

dim(dataset04)
head(dataset04)

# skimr - no missing values, car names are not columns (rownames)
skimr::skim(dataset04)
summary(dataset04)

# Discrepancy between boxes scales >> data will need to be scaled
boxplot(dataset04)

# Scale data for hierarchical clustering (sd = 1, mean = 0)
dataset04_sc <- as.data.frame(scale(dataset04))
skimr::skim(dataset04_sc)
summary(dataset04_sc)


###############################################
# 03 Number of clusters
###############################################

                          # dataset, min # of cluster, max # of clusters
model_cascade <- cascadeKM(dataset04, 2, 10)
model_cascade$results
plot(model_cascade) # clusters proposed: 10

model_cascade_20 <- cascadeKM(dataset04, 2, 20)
plot(model_cascade_20) # clusters proposed: 20

# Same, but for scaled data 
model_cascade_sc <- cascadeKM(dataset04_sc, 2, 10)
model_cascade_sc$results
plot(model_cascade_sc) # clusters proposed: 4

# Based on Calinski criterion we should choose 10 clusters (the bigger value the better).
# While increasing max number of clusters in arguments, cascade model suggests
# more and more clusters as outcome (i.e with max set to 20, model proposes 20 clusters).
# >> it seems a lot for just 32 different cars.
# With 5 clusters CH index increases comparing to 4 or 6 clusters. After scaling
# the data, we get highest CH index with 4 clusters.
# Final number of clusters to be confirmed based on dendrogram and silhouette factors
# (4 or 5 clusters).


###############################################
# 03 Hierarchical clustering
###############################################

# Distance matrix
dist_mat <- dist(dataset04_sc, method = 'euclidean')

# Hierarchical clustering & dendrogram
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
# Based on dendrogram >> 5 clusters

# Clusters marked on dendrogram:
rect.hclust(hclust_avg , k = 5, border = 2:6)

# Height of the cut:
abline(h = 3.5, col = 1, lwd = 2)

# Dendrogram with colorful branches:
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3.5)
plot(avg_col_dend)


###############################################
# 05 k-means clustering
###############################################

# To get same results we need to set.seed():
set.seed(500)

# k-means model with 4 clusters:
model_kmeans_4 <- kmeans(dataset04, 
                       centers = 4, 
                       nstart = 100) 

sil_index_4 <- silhouette(model_kmeans_4$cluster, 
                        dist = dist(dataset04,
                                    method = 'euclidean'))
plot(sil_index_4)
# Silhouettes have different thickness (max for cluster 3), but all of them
# have similar height >> we aim for most unity in terms of shape.


# k-means model with 5 clusters:
model_kmeans_5 <- kmeans(dataset04, 
                         centers = 5, 
                         nstart = 100) 
sil_index_5 <- silhouette(model_kmeans_5$cluster, 
                          dist = dist(dataset04,
                                      method = 'euclidean'))
plot(sil_index_5)
# Now much better in terms of thickness, height remains similar.

model_kmeans_6 <- kmeans(dataset04, 
                         centers = 6, 
                         nstart = 100) 
sil_index_6 <- silhouette(model_kmeans_6$cluster, 
                          dist = dist(dataset04,
                                      method = 'euclidean'))
plot(sil_index_6)
# With more clusters silhouettes are getting more diverse >> we will keep 5 
# clusters.


# mean s(i) for each cluster
summary(sil_index_5)

# mean s(i)
mean(sil_index_5[, 3]) 
# Each cluster mean s(i) is close to mean s(i) for whole data set


# Visualize data in PCA:

# Assign cluster from k-means to data set:
dataset04$Cluster <- as.factor(model_kmeans_5$cluster)
str(dataset04) # new column added
dim(dataset04)

# Create PCA model (w/o cluster column):
dataset04_pca <- prcomp(dataset04[,-12], scale. = TRUE)

ggbiplot(dataset04_pca, choices = c(1,2), ellipse=TRUE, groups = dataset04$Cluster) +
  labs(colour = 'Cluster') +
  scale_color_manual(values= wes_palette('FantasticFox1', n = 5))


###############################################
# ANSWER
###############################################
# Based on CH index for scaled and not-scaled data, as well as dendrogram and 
# silhouette factor >> 5 clusters.