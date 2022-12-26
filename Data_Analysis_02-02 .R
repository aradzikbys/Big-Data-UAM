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

# Install and load packages
library('MASS')
library(dplyr)
library(ggbiplot)
library(devtools)
install_github("vqv/ggbiplot")

# Check dataset, assign to variable
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

# Check which columns are numeric (int, dbl) >> we need to remove factors and labels
str(dataset02)

# Categorical (fct) and continuous (int, dbl) variables:
categorical <- c('Manufacturer', 'Model', 'Type', 'AirBags', 'DriveTrain',
                     'Cylinders', 'Man.trans.avail','Origin', 'Make')

continuous <- c('Min.Price', 'Price', 'Max.Price', 'MPG.city', 'MPG.highway', 
                    'EngineSize', 'Horsepower', 'RPM', 'Rev.per.mile', 'Fuel.tank.capacity', 
                    'Passengers', 'Length', 'Wheelbase', 'Width', 'Turn.circle', 
                    'Rear.seat.room', 'Luggage.room', 'Weight')

# For PCA analysis leave continuous variables in data set:
dataset02 <- dataset02[ , continuous]


# skimr: summary statistics about variables in data frame.
# we don't miss any values now, out of 18 colums 18 are numeric (int or num)
skimr::skim(dataset02)
str(dataset02)

# We need to re-scale the data >> there is discrepancy between boxes scales
# (RPM, Rev. per mile, Weight)
boxplot(dataset02)

# PCA model:
dataset02_pca <- prcomp(dataset02, scale. = TRUE)

# Summary of PCA model - importance of components
summary(dataset02_pca) 
# First 3 PCs explain 83.3% of variance >> we can confirm with screeplot

# Screeplot 
screeplot(dataset02_pca, type = 'l', pch = 20) 
# As of 4th PC the line flattens out, we should keep first 3 PCs


# Initial biplot (with PC1 and PC2 as axis):
ggbiplot(dataset02_pca, choices = c(1,2))
# Price is highly correlated with horsepower
# There is negative correlation between MPG (both highway and city) and cars'
# weight and engine size, also fuel tank capacity (if we consume more gas > tank should be bigger),


# Biplot #1: grouped by origin:
ggbiplot(dataset02_pca, ellipse=TRUE, groups=origin, labels=make) +
  geom_point(aes(colour=origin), size = 2.5)
# USA cars are bigger: they can fit more passengers, size of engine and car itself (lenght/width)
# are bigger than non-USA. USA cars have also lower RPM value - engines are less dynamic >>
# meaning they are fitted to drive long distance, w/o big acceleration.
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
