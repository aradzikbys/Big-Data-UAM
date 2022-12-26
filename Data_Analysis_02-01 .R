# Data analysis - 02 - PCA - loadings of componenrs, ggbiplot

##########
# EX01
#########
# Using the painters set (subjective assessments of painters) from the MASS package,
# perform a principal component analysis. Investigate the loadings of the first three
# principal components. Draw a scatter diagram for the first two principal components
# using different colors or symbols to distinguish schools of painting.

# Clear enviroment
rm(list = ls())

# Install and load packages
install.packages("UsingR")
library(UsingR)
library('MASS')
library(plyr)
library(dplyr)
library(ggbiplot)
library(devtools)
install_github("vqv/ggbiplot")

# Check dataset, assign to variable
?painters

dataset01 <- painters
head(dataset01)

#'School' >> qualitive variable, indicated by a factor level  A-H.
# The school to which a painter belongs:
# A: Renaissance;
# B: Mannerist;
# C: Seicento;
# D: Venetian;
# E: Lombard;
# F: Sixteenth Century;
# G: Seventeenth Century;
# H: French.
school <- c(dataset01$School)
school

# Re-name levels in the factor
# Our current levels: "A" "B" "C" "D" "E" "F" "G" "H"
levels(school)

school <- mapvalues(school,
          from = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
          to = c('Renaissance', 'Mannerist', 'Seicento', 'Venetian', 'Lombard', '16th Century', '17th Century', 'French'))

# New levels: "Renaissance"  "Mannerist"    "Seicento"     "Venetian"     "Lombard"      "16th Century"
# "17th Century" "French"
levels(school)

# Remove "school" variable from data set
dataset01 <- dataset01[,-5]

# Boxplot >> there is no need to scale the data (boxes are close to each other) 
boxplot(dataset01)

# Principal Components Analysis
dataset01_pca <- prcomp(dataset01)

# Summary of PCA model
summary(dataset01_pca)
# Importance of first 3 components: they correspond to 93.6% of variance:
#     PC1: explains 56% of the total variance,
#     PC2: explains 28.5% of the total variance,
#     PC3: explains 9.1% of the total variance.


# Biplot (with ggplot)

# Biplot - how initial variables map into principal components?
ggbiplot(dataset01_pca, choices = c(1,2))

# Biplot with grouped schools:
ggbiplot(dataset01_pca, ellipse=TRUE, groups=school, labels = rownames(dataset01)) +
  geom_point(aes(colour=school), size = 2.5)

# Rotation:
dataset01_pca$rotation 

###################################
# SUMMARY
###################################
# Based on biplot and rotation:
# To PC1 mostly contribute Expression (0.66), then in some part Composition (0.48)
# PC2 has strong negative weight for Colour (-0.84),
# PC3 has strong negative weight for Compostiion (-0.78) and positive weight of Expression (0.513)

# Based on biplot:
# There is no correlation between Composition and Colour (angle close to 90 degrees),
# There is strong positive correlation between Composition and Expression (small angle),
# Colour and Drawing variables have negative correlation.

# >> Venetian painters were highly valued for Color, while on other characteristics
# they were received poorly (especially Drawing).
# >> Manerists were valued mostly for Drawing. Group is tighly clustered: there are small
# discrepancies between ratings they've received.
# >> Besides Bourdon, French painters were highly valued for Drawing, as well as Expression
# and Composition, all of them received rather low ratings for Colour.
# >>16th Century Painters received rather low ratings in terms of Expression and Composition. 
# High rated painters: Raphael, Rubens; low rated painters: Fr. Penni, Belini