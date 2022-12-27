# Function to calculate the number of possible clusterings
# for n observations and K clusters
N.Clusteringss <- function(n = 100, K = 4){
  result <- 0
  for (k in 1:K)
    result <- result + (-1)^(K-k) * choose(K, k) * k^n
  result / factorial(K)
  }
N.Clusteringss() # 6.695575e+58
N.Clusteringss(n = 100, K = 5) # 6.573841e+67



###############################
### Hierarchical clustering ###
###############################
# https://www.datacamp.com/tutorial/hierarchical-clustering-R

#################
# Seeds example
#################

# to maintain reproducibility of the results 
set.seed(786)

# Read file to data frame
file_loc <- 'seeds_dataset.txt'
seeds_df <- read.csv(file_loc,sep = '\t',header = FALSE)

head(seeds_df)

# Add columns names
feature_name <- c('area','perimeter','compactness','length.of.kernel','width.of.kernal','asymmetry.coefficient','length.of.kernel.groove','type.of.seed')
colnames(seeds_df) <- feature_name

# Check data set:
dim(seeds_df)
summary(seeds_df)
skimr::skim(seeds_df) # There are missing values

# Remove missing values
seeds_df <- na.omit(seeds_df)
skimr::skim(seeds_df)

# Assign type of seed as separate df & remove it from original df
seeds_label <- seeds_df$type.of.seed
table(seeds_label) # How many data points for each seed? 1 x 66,  2 x 68  3 x 65
seeds_df$type.of.seed <- NULL
skimr::skim(seeds_df) 

# Scale data > sd = 1, mean = 0
seeds_df_sc <- as.data.frame(scale(seeds_df))
skimr::skim(seeds_df_sc)
summary(seeds_df_sc)

# Distance matrix
dist_mat <- dist(seeds_df_sc, method = 'euclidean')

# Hierarchical clustering & dendrogram
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

# Cut dendrogram to create desired number of clusters: 3 (3 types of seed)
cut_avg <- cutree(hclust_avg, k = 3)
plot(hclust_avg)
# Clusters marked on dendrogram:
rect.hclust(hclust_avg , k = 3, border = 2:6)
# Height of the cut:
abline(h = 3, col = 2)

# For colorful branches:
library(dendextend)
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)

# Append clusters from original df:
library(dplyr)
seeds_df_cl <- mutate(seeds_df, cluster = cut_avg)
count(seeds_df_cl, cluster)
# 1 x 63,  2 x 72  3 x 64
# (vs 1 x 66,  2 x 68  3 x 65 in original data frame)

# Cross check > confusion matrix:
table(seeds_df_cl$cluster,seeds_label)

#################
# Vltava example
#################

vltava <- read.delim('http://www.davidzeleny.net/anadat-r/data-download/vltava-spe.txt',
                     row.names = 1)

# Forest vegetation data from Vltava river valley close to Zlatá Koruna, Český Krumlov, Czech Republic.
# In each plot, all species of tree, shrub and herb layer were recorded and 
# their abundance was estimated using 9-degree ordinal Braun-Blanquette scale 
# (these values were consequently transformed into percentage).

# 97 sites (trees) & 274 species
dim(vltava)
library(dplyr)
vltava %>% View()

# For vegdist() command (Bray-Curtis distance)
library(vegan)

# If data is in percent >> we use method Braya-Curtisa
dis <- vegdist(sqrt(vltava), method = 'bray')

# Distance matrix 97x97 (for 97 locations)
# hc clust > first argument > distance matrix
tree.average <- hclust(dis, method = 'average')

# Division of the tree into 5 parts
lab.average <- cutree(tree.average, 5)
table(cutree(tree.average, 5))
# 29 elements in first group with k (partition) = 5

# Dendorgram
plot(tree.average)

# Aligned dendorgram
# Height = Bray-Curtis distanc
plot(tree.average, hang = -1) 
rect.hclust(tree.average, k = 5, border = 2, cluster = lab.average) 




#################
# Iris example
#################
# We now pretend we don't know the species anymore, 
# and we will see how well the clustering methods recover the species
pairs(iris[, 1:4], pch = 20, col = rep(1:3, each = 50), cex = 1.5)

# ED - euclides distance
dist.iris <- dist(iris[, 1:4])

# cluster analysis using "complete" method
cluster.iris <- hclust(dist.iris, method = 'complete')
plot(cluster.iris, labels = FALSE)
cutree(cluster.iris, k = 3)
rect.hclust(cluster.iris, k = 3, border = 2)

# For recode() command
library(car)

# Exchange 2 & 3
lab.cluster <- car::recode(cutree(cluster.iris, k = 3), "2 = 3; 3 = 2")

# Error of clustering for original data (16%)
1 - sum(diag(table(iris$Species, lab.cluster))) / nrow(iris) 

# PCA to visualize results of clustering:
model.pca <- prcomp(iris[, 1:4], scale. = TRUE)

plot(model.pca$x, col = lab.cluster, pch = as.numeric(iris$Species))

legend('topright', legend = levels(iris$Species), 
       pch = 20, col = 1:3, title = 'Clustering')

legend('bottom', legend = levels(iris$Species), pch = 1:3, 
       title = 'Original')


cluster.iris.pca <- hclust(dist(model.pca$x), method = 'complete')

# Error of clustering for PC data (21.33%)
1 - sum(diag(table(iris$Species, col = cutree(cluster.iris.pca, k = 3)))) / nrow(iris) 




###############################
###    K-means clustering   ###
###############################
# https://www.kdnuggets.com/2020/06/centroid-initialization-k-means-clustering.html
# https://www.datacamp.com/tutorial/k-means-clustering-r
# https://uc-r.github.io/kmeans_clustering

#################
# UBER example
#################
# Load the .csv files
apr14 <- read.csv('uber-raw-data-apr14.csv')
may14 <- read.csv('uber-raw-data-may14.csv')
jun14 <- read.csv('uber-raw-data-jun14.csv')
jul14 <- read.csv('uber-raw-data-jul14.csv')
aug14 <- read.csv('uber-raw-data-aug14.csv')
sep14 <- read.csv('uber-raw-data-sep14.csv')

# Bind data togheter
library(dplyr)
data14 <- bind_rows(apr14, may14, jun14, jul14, aug14, sep14)

# Summary of data
summary(data14)
skimr::skim(data14)

# The dataset contains the following columns:
  
# Date.Time : the date and time of the Uber pickup;
# Lat: the latitude of the Uber pickup;
# Lon: the longitude of the Uber pickup;
# Base: the TLC base company code affiliated with the Uber pickup.


# Divide Date.Time
library(lubridate)

# Separate or mutate the Date/Time columns
data14$Date.Time <- mdy_hms(data14$Date.Time)
data14$Year <- factor(year(data14$Date.Time))
data14$Month <- factor(month(data14$Date.Time))
data14$Day <- factor(day(data14$Date.Time))
data14$Weekday <- factor(wday(data14$Date.Time))
data14$Hour <- factor(hour(data14$Date.Time))
data14$Minute <- factor(minute(data14$Date.Time))
data14$Second <- factor(second(data14$Date.Time))

data14$Month # levels from 4 to 9 (as expected)

head(data14)

# k-means function
# 5 clusters = 5 boroughs
set.seed(20)
clusters <- kmeans(data14[,2:3], 5)

# Save the cluster number in the dataset as column 'Borough'
data14$Borough <- as.factor(clusters$cluster)

# Inspect 'clusters'
str(clusters)

# cluster: a vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# size: the number of points in each cluster.

# Plot results:
library(ggplot2)
library(ggmap)

NYCMap <- get_map("New York", zoom = 10)
ggmap(NYCMap) + geom_point(aes(x = Lon[], y = Lat[], colour = as.factor(Borough)),data = data14) +
  ggtitle("NYC Boroughs using KMean")

# Uber's growth within the boroughs for each month
library(DT)

data14$Month <- as.double(data14$Month)
month_borough_14 <- count_(data14, vars = c('Month', 'Borough'), sort = TRUE) %>% 
  arrange(Month, Borough)
datatable(month_borough_14)

library(dplyr)
monthly_growth <- month_borough_14 %>%
  mutate(Date = paste("04", Month)) %>%
  ggplot(aes(Month, n, colour = Borough)) + geom_line() +
  ggtitle("Uber Monthly Growth - 2014")
monthly_growth



# Basic algorithm (100 random initializations; 3 groups)
model.kmeans <- kmeans(iris[, 1:4], 
                       centers = 3, 
                       nstart = 100) 

# VarianceTotal = VarianceBetweenGroups (Big) + VarianceWithinGroups (Small)
# Between/Total -> Big [0, 1]
# Within/Total -> Small [0, 1]
# Between/Within -> Big [R+]

# Clustering vector
model.kmeans$cluster 
# Centers of clusters
model.kmeans$centers
# Number of objects in each class
model.kmeans$size 

lab.cluster.kmeans <- car::recode(model.kmeans$cluster, "2 = 3; 3 = 2")
1 - sum(diag(table(iris$Species, lab.cluster.kmeans))) / nrow(iris)

# Cluster means from hierarchical clustering
cluster.means <- by(iris[, 1:4], lab.cluster, colMeans) 


# initial centers from hierarchical clustering
model.kmeans.nonrandom <- kmeans(iris[, 1:4], 
                                 centers = matrix(unlist(cluster.means), 
                                                  nrow = 3, 
                                                  byrow = TRUE)) 
# k-means with non-random initialization



# for pam() command
library(cluster) 

# Partitioning around medoids
model.pam <- pam(iris[, 1:4], k = 3) 

# Clustering vector
model.pam$clustering 

# Cluster plot (in PC space)
clusplot(model.pam, color = TRUE, main = 'PAM clustering') 

# Error of clustering for original data for PAM method (10.67%)
1 - sum(diag(table(iris$Species, model.pam$clustering))) / nrow(iris) 

# Clara method (for big data sets)
model.clara <- clara(iris[, 1:4], k = 3)

# Error of clustering for original data for clara method (10%)
1 - sum(diag(table(iris$Species, model.clara$clustering))) / nrow(iris) 




##################################
### Clustering - # of clusters ###
##################################

# For cascadeKM() command
library(vegan) 
model.cascade <- cascadeKM(iris[, 1:4], 2, 10)

# CH index - the bigger value the better
model.cascade$results 
# Division into 3 groups (Calinski criterion)

plot(model.cascade)
# Left part of the graph: how division is changing depending on number of clusters
# i.e. red group - only with 5 clusters division starts (into red and blue ones)


# Silhouette index
# For silhouette() command
library(cluster) 
sil.index <- silhouette(model.kmeans$cluster, 
                        dist = dist(iris[, 1:4],
                        method = 'euclidean'))

# mean s(i) for each cluster
summary(sil.index)

# No narrow silhouettes - good clustering
plot(sil.index) 

# s(i)
sil.index[, 3] 

# mean s(i)
mean(sil.index[, 3]) 


##########################################
### Fuzzy Clustering (soft clustering) ###
##########################################

# For fanny clustering
library(cluster)

# For dune data set
library(vegan) 

data(dune)
?dune

skimr::skim(dune)
# Bray-Curtis distance - distance between 2 positions in ecology
# 0 - they are maximally similar, 1 - extremely different

(d <- vegdist(dune)) 

# In ecology and biology, the Bray–Curtis distance (dissimilarity), is a measure used to 
# calculate dissimilarity between two different sites, based on counts at each site. 
# The BC dissimilarity is bound between 0 and 1, where 0 means the two sites 
# have the same composition (identical), and 1 means the two sites are completely disjoint. 
# It is often multiplied by 100 and interpreted as a percentage

heatmap(as.matrix(d), 
        symm = TRUE, 
        Rowv = NA,
        # Distance plot more yellow means larger distance (less similar sites)
        col = heat.colors(3)) 
fanny(d, 3)
# The default memb.exp = 2 gives complete fuzziness (in this case)
# The parameter memb.exp (fuzzyfier) is a real number greater than 1 and it defines the level of cluster fuzziness.
# Note that, a value close to 1 gives a cluster solution which becomes increasingly 
# similar to the solution of hard clustering such as k-means,
# whereas a value of mm close to infinite leads to complete fuzzyness (1/k).
# Values too close to 1 can lead to slow convergence.

# Crisper clustering
model.fanny <- fanny(d, 3, memb.exp = 1.7) 
summary(model.fanny)

model.fanny$membership
model.fanny$coeff
# Dunn’s partition coefficient F(k) is the sum of all squared membership coefficients, 
# divided by the number of observations. Its value is between 1/k and 1.
# The normalized form of the coefficient is also given (F(k)-1/k)/(1-1/k) and ranges between 0 and 1.
# A low value of Dunn’s coefficient indicates a very fuzzy clustering, 
# whereas a value close to 1 indicates a near-crisp clustering. Larger F(k) means better solution.

# Simple ordination - PCA
model.pca <- prcomp(dune, scale. = TRUE)

# Initiate plot
ordiplot(model.pca, type = 'none') 

# Classical visualization for fuzzy clusterring 
stars(model.fanny$membership, locations = model.pca$x[, 1:2], 
      draw.segm = TRUE, add = TRUE, scale = FALSE, len = 1, labels = '')

# Convex hull / powloka wypukla
ordihull(model.pca, model.fanny$clustering, col = 'blue', lwd = 2) 

# Nice plots for clustering: fviz_cluster() & fviz_silhouette()
library(factoextra)

# fviz_cluster()
fviz_cluster(list(data = dune, 
                  cluster = model.fanny$clustering), 
             frame.type = 'norm')

# fviz_silhouette()
fviz_silhouette(model.fanny, label = TRUE)

# Visualize correlation (or other) matrix
library(corrplot) 
corrplot(model.fanny$membership, is.corr = FALSE, cl.pos = 'n')



# For cmeans() method - the fuzzy version of the kmeans clustering
library(e1071) 

# It is not possible to use distance matrix, we need raw data
model.cmaens <- cmeans(dune, centers = 3) 

# Dunn's coefficient
(Fk <- sum(model.cmaens$membership^2) / nrow(dune))

# Normalized Dunn's coefficient (for k = 3 clusters)
(Fk - 1/3) / (1 - 1/3) 
fviz_cluster(list(data = dune, cluster = model.cmaens$cluster), frame.type = 'norm')
corrplot(model.cmaens$membership, is.corr = FALSE, cl.pos = 'n')
