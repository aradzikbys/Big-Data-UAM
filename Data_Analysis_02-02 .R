# Data analysis - 02 - PCA - loadings of components, ggbiplot

##########
# EX02
#########
# Using categorical and continuous variables from the Cars93 dataset from the MASS package,
# perform principal component analysis. Compare (in separate charts):
#     >> American and other cars (Origin),
#     >> Car types (Type).

# Clear enviroment
rm(list = ls())


###############################################
# 01 Libraries
###############################################

# For data set:
library('MASS')
library(tidyverse)
# For ggbiplot:
library(ggbiplot)
library(devtools)
install_github("vqv/ggbiplot")

###############################################
# 02 Data set
###############################################

?Cars93

dataset02 <- Cars93
head(dataset02)

# skimr: summary statistics about variables in data frame.
# We miss values for Luggage.room (11) and Rear.seat.room (2),
# to perform PCA we need to exclude NA values:
skimr::skim(dataset02)
dataset02 <- na.omit(dataset02)

# 2 groups in which we will compare data later:
origin <- c(dataset02$Origin)
type <- c(dataset02$Type)

# For labels on chart:
make <- c(dataset02$Make)

# Transform some factor variables into numbers
# (we won't use lables used to analyze data, like Manufacturer & Model, Type, Origin and Make)
skimr::skim(dataset02)

# Remove "label" columns from data set
lbls <- c('Manufacturer','Model','Type','Origin','Make')
dataset02[,lbls] <- list(NULL)

# Change factor columns to numeric
for (i in 1:ncol(dataset02)){
  if(sapply(dataset02[i], class) == 'factor'){
    dataset02[i] <- lapply(dataset02[i], function(x) as.integer(x))
  }
}


###############################################
# 03 PCA
###############################################


# We need to re-scale the data >> there is discrepancy between boxes scales
boxplot(dataset02)

# PCA model:
dataset02_pca <- prcomp(dataset02, scale. = TRUE)

# Summary of PCA model - importance of components
summary(dataset02_pca) 
# First 4 PCs explain 82% of variance >> we can confirm with scree plot

# Scree plot 
screeplot(dataset02_pca, type = 'l', pch = 20) 
# As of 5th PC the line flattens out >> it's ok to keep first 4 PCs


###############################################
# 04 Biplots
###############################################

# Initial biplot (with PC1 and PC2 as axis):\
ggbiplot(dataset02_pca)
ggbiplot(dataset02_pca, choices = c(1,2))
# Price is highly correlated with horsepower
# There is negative correlation between MPG (both highway and city) and cars'
# weight and engine size, also fuel tank capacity (if we consume more gas > tank should be bigger),


# Biplot #1: grouped by origin:
ggbiplot(dataset02_pca, ellipse=TRUE, groups=origin, labels=make) +
  geom_point(aes(colour=origin), size = 2.5)
# USA cars are bigger: they can fit more passengers, size of engine and car itself (lenght/width)
# are bigger than non-USA. USA cars have also lower RPM value - engines are less dynamic >>
# meaning they are fitted to drive long distance, w/o big acceleration. Also, manual
# transmission is not available in them.
# Non-USA cars are more efficient in terms of gas usage (but based purely on labels it's rather
# for Japan cars - Subaru, Mazda, Suzuki). European cars (BMW, VW, Volvo) have worse parameters
# in terms of MPG. Also, non-USA cars (rather European) are more expensive.


# Biplot #2: grouped by origin AND type:
ggbiplot(dataset02_pca, ellipse=TRUE, groups=type, labels=make) +
  geom_point(aes(colour=type, shape = origin), size = 2.5)
# Midsize ans Sporty cars are the most diverse group (midsize in terms of price -
# cars from USA are cheaper than those non-USA originated). 
# The most clustered groups (with homogeneous parameters) are Small and Large cars.
# All cars classified as Large are from USA.
# There is no surprise that Small cars have the best parameters in terms of gas 
# usage.
