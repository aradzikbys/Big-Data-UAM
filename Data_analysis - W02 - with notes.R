##############################
#             PCA            #
##############################
# Additional resources:
# https://www.datacamp.com/tutorial/pca-analysis-r
# https://www.r-bloggers.com/2016/07/performing-principal-components-regression-pcr-in-r/


##############
## Example 0
##############
# Artificial example
# Generate some data (Gaussian distribution)
# Set.seed () >> initialize a pseudo-random number generator
# set seed() works for 1 generation
set.seed(1000)

# 100 numbers in range (0,1)
x1 <- rnorm(100) 

set.seed(2000)
temp1 <- rnorm(100)

# x2 correlates with x1 with some "noise" >> [+ sqrt(1/3) * temp1]
x2 <- -sqrt(2/3) * x1 + sqrt(1/3) * temp1

# Data frame based on x1 and x2
data.set <- data.frame(x1, x2)

# Standardize the data to have mean 0 and variance 1 >> scale()
data.set1 <- scale(data.set)


# Scatter plots (slight difference in scale between data.set and data.set1)
plot(data.set, pch = 20) 
plot(data.set1, pch = 20, col = 2) 

# Means
summary(data.set)
summary(data.set1)
round(colMeans(data.set1), 10) 

# Variances (1.0) and covariances on diagonals = -0.8108832
# similar to "a" in front of x1 in our equation (-sqrt(2/3) = -0.81649658092)
var(data.set) 
var(data.set1) 

# Correlation for not-scaled data = same as for data.set1
cor(data.set)
cor(data.set1)

# Scatterplot with data id:
plot(data.set1, type = 'n'); text(data.set1, labels = c(1:100), cex = 0.7) 

# PCA with scores
model.pca <- prcomp(data.set) 

# Explained variance
summary(model.pca) 

# Loadings - coefs of linear combination (rotation matrix)
model.pca$rotation 

# Scores - observations in the new space
model.pca$x 

# Standard deviations of new variables
model.pca$sdev

# Total variance in the original data
sum(diag(var(data.set))) 

# Total variance in the rotated data
sum(diag(var(model.pca$x)))

# Correlation between new variables
round(cor(model.pca$x), 10) 

# Plot transformed data; data might flip (the sign of PC's is not unequivocal)
plot(model.pca$x, pch = 20)

# Biplot is similar, but axes are scaled differently
biplot(model.pca)



##############
## Example 1
##############
# Body measurements of female sparrows:
# X1 = total length, 
# X2 = alar length, 
# X3 = length of beak and head, 
# X4 = length of humerus, 
# X5 = length of keel and sternum; all in mm).  
# Birds 1 to 21 survived a severe storm near Brown University
# in Rhode Island while the remainder died. (Original source Bumpus 1898)

bumpus <- read.table('bumpus.csv', 
                    header = TRUE, 
                    sep = ';', 
                    row.names = 1, 
                    dec = ',')

bumpus

# Summary statistics about variables in data frames
skimr::skim(bumpus)

# Change 1/2 (alive/death) to "labels" rather than numbers
# It's qualitative variable >> we change it to factor
group <- factor(bumpus$Survival)
group

# Remove 'SURVIVAL' variable:
bumpus <- bumpus[, -6]


# Variation
var(bumpus)
# Diagnoal parameters
diag(var(bumpus)) 

boxplot(bumpus)
# Big differences in scale, we have to scale data
# The more differentiation in boxplot, the harder to compare date >> then
# it's good then to change scale

# PCA model with re-scaled data:
model.pca <- prcomp(bumpus, scale. = TRUE)

# Summary of PCA model - importance of components
# Two first variables (total lenght, alar extent) explain 82.95% of total variance
summary(model.pca) 

# LOADINGS - parameters showing the contribution of individual original variables
# to the formation of principal components. The greater the absolute value of the
# loading, the greater the variable's contribution to the construction of the PC.

# SCORES - coordinates of the observations in the new coordinate system
# created by the principal components, which are most often visualized.

# Loadings of variables in each PC:
model.pca$rotation 
# Scores - observations in the new space
model.pca$x 

# We choose only first 2 components (PC1, PC2) - most important
plot(model.pca$x, pch = 20, col = group)
legend('bottomleft', 
       legend = c('survived', 'died'), 
       pch = 20, 
       col = c(1,2), 
       bty = 'n',
       cex = 1)

biplot(model.pca, scale = 0)

# In general, biplot vectors can be interpreted in three ways (Rossiter 2014):

# The orientation (direction) of the vector, with respect to the principal
# component space, in particular, its angle with the principal component axes:
# the more parallel to a principal component axis is a vector, the more it contributes only to that PC.

# The length in the space; the longer the vector, the more variability of this
# variable is represented by the two displayed principal components; short vectors
# are thus better represented in other dimension.

# The angles between vectors of different variables show their correlation in
# this space: small angles represent high positive correlation, right angles
# represent lack of correlation, opposite angles represent high negative correlation.


# PC1 is some kind of average of all the measurements => measure of size of the bird
# PC2 has a negative weight for 'sternum' and positive weights for 'alar', 
# 'head' and 'humerus' => measure of shape of the bird (?)



##############################
# PCA is not scale invariant #
##############################

# Only center 'Bumpus' data, don't re-scale data:
model.pca2 <- prcomp(bumpus)

# Proportion of the variance explained is different
summary(model.pca); summary(model.pca2) 
model.pca2$rotation

par(mfrow = c(2, 1)) # Split the screen
biplot(model.pca)
biplot(model.pca2) # Different rotations



#########################################
# How many principal components to use? #
#########################################

par(mfrow = c(1, 1))

# 1
# Screeplot >> we left so many variables until chart (line) will be flat
plot(model.pca) 
plot(model.pca, type = 'l', pch = 20)

# 2
# Once we will reach over 90% of explanation of total variance
summary(model.pca)

# 3
# Eigenvalues bigger than mean
# (eigenvector is a direction, eigenvalue is a number)
sum(model.pca$sdev^2 > mean(model.pca$sdev^2)) 
# Only PC1 has eigenvalue higher than mean (= 1)


# Correlation between original variables and principal components
round(cor(scale(bumpus), model.pca$x), 2) 

# Correlation^2: amount of variation of each of the variables that
# is explained by the principal components; note the rows sum to 1
# (How  variable is explained by other PCs)?
round(cor(scale(bumpus), model.pca$x)^2, 2) 





##############
## Example 2
##############
# USJudgeRatings example

?USJudgeRatings
skimr::skim(USJudgeRatings)

# PCA model & summary:
model.pca <- prcomp(USJudgeRatings, center = TRUE)

summary(model.pca)
# First variable is the most important (explains 85% of variance)

# Based on screeplot >> we should choose 2-3 PCs
screeplot(model.pca, type = 'l', pch = 20)

biplot(model.pca, cex = 0.7)

plot(model.pca$x, type = 'n')
text(model.pca$x, 
     labels = abbreviate(row.names(USJudgeRatings), 3), 
     col = 'red', cex = 0.7)

# Amount of variation of each of the variables that
# is explained by the principal components
round(cor(USJudgeRatings, model.pca$x) ^ 2, 2) 




##############################
#             PCR            #
##############################

library(pls) # For pcr() command
library(ISLR) # For dataset Hitters
# We have data from 1986 showing 322 major league baseball players versus 20 variables
?Hitters

# We would like to predict Salary: 1987 annual salary on opening day in thousands of dollars
# NA data (59 players)
# data of salary missing for 59 players:
sum(is.na(Hitters$Salary)) 

# Full stats:
# Complete_rate > 0.817 (for salary we have data for 82%)
skimr::skim(Hitters)

# 322 observations
dim(Hitters)

# Remove NAs
Hitters <- na.omit(Hitters) 

# 263 observations
dim(Hitters) 

# Check stats: data is complete
skimr::skim(Hitters)



# Variance inflation factor

# VIF estimates how much the variance of a regression coefficient is inflated
# due to multicollinearity in the model. The more your VIF increases, the less
# reliable regression results are going to be.
# In general, a VIF above 10 indicates high correlation and is cause for concer

# For vif() function
library(DAAG)

# Some variables are multicollinear
# VIF = 1:not correlated.
# 1< VIF <5: moderately correlated.
# VIF >5: highly correlated.
vif(lm(Salary ~ ., data = Hitters)) 


# VIF = 1 / (1-R^2)
# 1/VIF = 1 - R^2
# R^2 = 1 - 1/VIF
# kCV = Cross Validation (defaulf k=10)


# Before performing PCR, it is preferable to standardize data (this step is not
# necessary but strongly suggested >> PCA IS NOT SCALE INVARIANT).
# The scaling will prevent the algorithm to be skewed towards predictors that
# are dominant in absolute scale but perhaps not so relevant as others.

# Next step >> we should remove all the observations containing missing data.

# 10-fold CV validation (randomly allocate data to validation sets)
# https://www.kdnuggets.com/2018/01/training-test-sets-cross-validation.html

model.pcr.cv <- pcr(Salary ~ ., 
                    data = Hitters, 
                    scale = TRUE, 
                    # 10-fold CV validation:
                    validation = 'CV') 

# Leave-One-Out
# https://www.statology.org/leave-one-out-cross-validation-in-r/
model.pcr.loo <- pcr(Salary ~ .,
                     data = Hitters, 
                     scale = TRUE, 
                     # LOO CV validation:
                     validation = 'LOO') 

#RMSEP: root mean square error of prediction
# we look for smallest CV value
summary(model.pcr.cv)
summary(model.pcr.loo)


# Mean squared error of prediction for different number of PCs
msep <- MSEP(model.pcr.cv) 

# Optimal number of components - 18 (LOWEST VALUE WITH CV)
msep$comps[which.min(msep$val[1, 1, ])] 
validationplot(model.pcr.cv, val.type = 'MSEP')

# We look for a point with minimum
abline(v = msep$comps[which.min(msep$val[1, 1, ])], col = 'blue', lwd = 2)



# Let’s see how well it predicts. We will form a train and test split
# Each variance have 50-50 chance to go under FALSE/TRUE > end result will be close
# to 50-50
train <- sample(c(TRUE, FALSE), 
                nrow(Hitters), 
                rep = TRUE) # Sample with replacement
table(train)
test <- !train

model.pcr.train <- pcr(Salary ~ ., 
                       data = Hitters, 
                       scale = TRUE, 
                       subset = train, 
                       validation = 'CV')
summary(model.pcr.train)

msep.train <- MSEP(model.pcr.train)

# Best number of components
best.number.variables <- msep.train$comps[which.min(msep.train$val[1, 1, ])] 
validationplot(model.pcr.train, val.type = 'MSEP')
(model.pcr.predict <- predict(model.pcr.train, 
                              Hitters[test, -19], 
                              # Prediction (Salary)
                              ncomp = best.number.variables))
# RMSEP
sqrt(mean((model.pcr.predict - Hitters$Salary[test])^2)) 

#MAE
mean(abs(model.pcr.predict - Hitters$Salary[test]))

#MAPE
mean(abs((model.pcr.predict - Hitters$Salary[test]) / Hitters$Salary[test]))

# PCR for all variables
# To transform non-numeric variables into numbers
x <- model.matrix(~ ., data = Hitters) 

model.pca <- prcomp(x)
model.lm.pcr <- lm(Hitters$Salary ~ model.pca$x) # lm for all PCs
model.lm.pcr.predict <- predict(model.lm.pcr)
model.lm.original <- lm(Hitters$Salary ~ x - 1) # lm for all original variables
model.lm.original.predict <- predict(model.lm.original)

# Run PCR using all of the scores or we can use
# all of the original covariates => It’s the same.
round(max(abs(model.lm.pcr.predict - model.lm.original.predict)), 10)
