# Useful links:
# https://www.r-bloggers.com/2017/08/how-to-prepare-and-apply-machine-learning-to-your-dataset/

##########################################
###                 kNN                ###
##########################################
# kNN - non-parametric supervised learning method. It is commonly used for simple
# recommendation systems, pattern recognition, data mining, financial market
# predictions, intrusion detection, and more.
# KNN is a lazy algorithm, this means that it memorizes the training data set
# instead of learning a discriminative function from the training data.
# https://www.ibm.com/topics/knn
# https://www.edureka.co/blog/knn-algorithm-in-r/

# For knn() command used for classification and regression
library(class)

# Shumway (1988).
# Variables are the population (Nuclear explosion vs. Earthquake) and two seismological features 
# namely body wave magnitude and surface wave magnitude).
data.set <- read.table('http://www.drizzt.home.amu.edu.pl/images/DADA_AIPD/earthquake.txt', 
                       header = TRUE)

# KNN can be used for solving both classification and regression problems.
# Classification - relies on labels (factors), regression - on values (discrete)
data.set$class <- as.factor(data.set$class)

# Scatter plot for data set
plot(data.set$body, 
     data.set$surface, 
     col = data.set$class, 
     pch = 20, cex = 2)
text(data.set$body, 
     data.set$surface,labels = data.set$class)

# 1NN prediction - result: same as initial data set (1 neighbor = 1 exact data point)
      # training set:
(knn(data.set[, 2:3],
     # testing set:
     data.set[, 2:3],
     # factor of true classifications (classes of training data set)
     cl = data.set$class, 
     # number of neighbors
     k = 1) -> pred.knn) 

# Error rate (resub.)
mean(pred.knn != data.set$class) 


# 3NN prediction - if training and testing datasets are the same > we will get
# the same labels as in initial data.set
knn(data.set[, 2:3],
    data.set[, 2:3], 
    cl = data.set$class, 
    k = 3)

# With paramenter prob = TRUE >>
# probabaility aposteriori (proportion of votes for the winning class)
model.3NN <- knn(data.set[, 2:3], 
                 data.set[, 2:3], 
                 cl = data.set$class, 
                 k = 3, 
                 prob = TRUE) 

model.1NN <- knn(data.set[, 2:3], 
                 data.set[, 2:3], 
                 cl = data.set$class, 
                 k = 1, 
                 prob = TRUE) 

model.3NN

# Model 3NN with posterior probabilities

# Posterior probabilities
# A posterior probability, in Bayesian statistics, is the revised or updated probability
# of an event occurring after taking into consideration new information.
# The posterior probability is calculated by updating the prior probability using Bayes' theorem
attr(model.3NN, 'prob')

# 3NN resubs. error rate
100 * mean(model.3NN != data.set$class) 



# Initial grid of points: at each point of the grid we count predictions. 
# At the point that prediction is changing we have class boundary 

# Grid based on initial graph (x and y axis have the same limits), resolution 100:
x <- seq(4.5, 6.5, length = 100)
y <- seq(3.5, 6.5, length = 100)

# Apply points on the grid: 100 x 100 >>  10000 points
grid <- expand.grid(body = x, surface = y) 

# Build model
Z3 <- knn(# training set:
          data.set[, 2:3],
          # testing set > grid points:
          grid,
          cl = data.set$class,
          k = 3)


colors <- ifelse(Z3 == 'equake', 'grey80', 'mistyrose')

# Plot grid > non-linear border
plot(grid, col = colors, pch = 20, cex = 1.5) 


# Add border
        # x,y - define grid
contour(x, y,
        # matrix assigns feature in each point of the grid:
        matrix(as.numeric(Z3 == 'equake'), 100), 
        add = TRUE, 
        lwd = 2,
        col = 'grey30',
        # 0-1 > at which point the line will be drawn?
        levels = 0.5, 
        # Non-linear boundary
        drawlabels = FALSE) 

# Add original data points:
points(data.set[, 2:3], pch = 19, col = data.set$class, cex = 1.5)



# For command errorest()
# errorest: Estimators of Prediction Error - function calculates the classification
# error (defined as the percentage of misclassified objects) using various estimators
# of this error: CV (cross validation), boot (bootstrap) or 632plus (bias corrected
# bootstrap).

# Results can be different each time
# The lower misclassification error and sd the better

# Library for errorest
# https://www.rdocumentation.org/packages/ipred/versions/0.9-13/topics/errorest
library(ipred) 


# Bootstrap error est. for 1NN
          # Formula - in our case class depends on surface + body
errorest(formula = class ~ body + surface, 
         # Data
         data = data.set,
         # Name of the function which will create model
         model = ipredknn, 
         # Estimator of the misclassification error:
         estimator = 'boot',
         # Prediction method to be used
         predict = function(o, newdata) predict(o, newdata, type = 'class'),
         # nboot - number of bootstrap replications.
         # https://www.rdocumentation.org/packages/ipred/versions/0.9-13/topics/control.errorest
         est.para = control.errorest(nboot = 100),
         # Classifier hyperparameter:
         k = 1) 

# Bootstrap error est. for 3NN
errorest(class ~ ., 
         data = data.set, 
         model = ipredknn, 
         estimator = 'boot',
         predict = function(o, newdata) predict(o, newdata, 'class'), 
         est.para = control.errorest(nboot = 100), 
         k = 3)

# Cross validation - good for small model
errorest(class ~ ., 
         data = data.set, 
         model = ipredknn, 
         estimator = 'cv',
         predict = function(o, newdata) predict(o, newdata,  'class'), 
         est.para = control.errorest(k = nrow(data.set)))


# One below will return error:
# in control.errorest k = nrow(data.set) >> 29, later k = 3
errorest(class ~ ., 
         data = data.set, 
         model = ipredknn, 
         estimator = 'cv',
         predict = function(o, newdata) predict(o, newdata,  'class'), 
         est.para = control.errorest(k = nrow(data.set)),
         k = 3)

# Solution:
mymod <- function(formula, 
                  data, 
                  l = 3) {
  ipredknn(formula = formula, data = data, k = l)
  
}
errorest(class ~ ., 
         data = data.set, 
         model = mymod, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata,  'class'), 
         est.para = control.errorest(k = nrow(data.set)), 
         l = 3) #LOO error rate for 3NN



# Map miscalssification error, repeated 10 times
purrr::map_dbl(1:10, function(x) errorest(class ~ ., 
                                          data = data.set, 
                                          model = mymod, 
                                          estimator = 'cv', 
                                          predict = function(o, newdata) predict(o, newdata,  'class'), 
                                          est.para = control.errorest(k = nrow(data.set)), 
                                          l = x)$error)


##########################################
### Multivariate Gaussian distribution ###
##########################################

# Density of univariate Gaussian (bell curve)
curve(dnorm(x), xlim = c(-5, 5), lwd = 2) 

# For mvrnorm() function (generate multivariate normal data)
library(MASS) 
# For ellipse() function
library(ellipse) 

# 1st set of points (100 independent observations)
X1 <- mvrnorm(100, 
              # covariance matrix
              mu = c(0, 0), 
              Sigma = matrix(c(1, 0, 0, 1), 
              # N_2(mu, Sigma) - bivariate normal
              nrow = 2))

# Generated points
plot(X1, col = 1, pch = 20, xlim = c(-10, 10), ylim = c(-10, 10)) 

# Confidence ellipse (95 % of points should be inside)
lines(ellipse(matrix(c(1, 0, 0, 1), nrow = 2)), 
      type = 'l', 
      col = 1, 
      lwd = 2) 

# 2nd set of points:
X2 <- mvrnorm(100, 
              # center will be moved to (3,3)
              mu = c(3, 3), 
              Sigma = matrix(c(1, 0, 0, 1), 
                             nrow = 2))
points(X2, col = 2, pch = 20)
lines(ellipse(matrix(c(1, 0, 0, 1), nrow = 2), 
              centre = c(3, 3)), 
      type = 'l', 
      col = 2, 
      lwd = 2)

# 3rd set of points:
X3 <- mvrnorm(100,
              # center will be moved to (-4,-4)
              mu = c(-4, -4),
              # Bigger variance:
              Sigma = matrix(c(3, 0, 0, 3), 
                             nrow = 2))
points(X3, col = 3, pch = 20)
lines(ellipse(matrix(c(3, 0, 0, 3), nrow = 2), 
              centre = c(-4, -4)), 
      type = 'l', 
      col = 3, 
      lwd = 2)


# 4th set of points:
X4 <- mvrnorm(100, 
              # center will be moved to (5,-5)
              mu = c(5, -5), 
              # Now there is co-dependence:
              Sigma = matrix(c(1, 0.7, 0.7, 1), 
                             nrow = 2))
points(X4, col = 4, pch = 20)
lines(ellipse(matrix(c(1, 0.7, 0.7, 1), nrow = 2), centre = c(5, -5)), type = 'l', col = 4, lwd = 2)





##########################################
###                 LDA                ###
##########################################
# https://stats.stackexchange.com/questions/71489/three-versions-of-discriminant-analysis-differences-and-how-to-use-them


# Shumway (1988).
# Variables are the population (Nuclear explosion vs. Earthquake) and two seismological features 
# namely body wave magnitude and surface wave magnitude).
data.set <- read.table('http://www.drizzt.home.amu.edu.pl/images/DADA_AIPD/earthquake.txt', header = TRUE)


# For lda() and qda() commands
library(MASS) 

# LDA, QDA -  statistical learning methods (bayesian) used for classifying 
# observations to a class or category
# LDA - Linear Discriminant Analysis 
# QDA - Quadratic Discriminant Analysis

# LDA assumes the feature covariance matrices of both classes are the same,
# which results in a linear decision boundary. QDA is less strict and allows
# different feature covariance matrices for different classes,
# which leads to a quadratic decision boundary:
# https://scikit-learn.org/dev/modules/lda_qda.html

# LDA uses means and variances of each class in order to create a linear boundary
# (or separation) between them. This boundary is delimited by the coefficients.

# Prior estimated as fractions
(model.lda <- lda(class ~ ., data.set)) 

# Prior probabilities of groups - probabilities in training data
# (69% equake, 31% expolosion)

# Group means: average of each predictor within each class
# "body" has more influence on explosion than on equake (5.97 vs 5.24),
# "surface" has more influence on equake vs explosion (4.74 vs 4.24)

# Boundary between classes:
# y = 3.918916 * body - 1.865219 * surface


# Prior set by user - 50% - 50%
# Our training data set contains 69% of datapoints for equake and 31% for explosion:
# LDA tends to extend classification area in favor for more observations
# with setting prior to 50-50, we treat both datasets equaly 
(model.lda.equal.prior <- lda(class ~ ., data.set, prior = c(1, 1) / 2)) 


# Classification on trainng data set
(classification <- predict(model.lda, data.set)) 
# 1st observation: 33% chance for equake, 67% chance for explosion,
# 2nd observation: 96% chance for equake, 4% chance for explosion


# On diagonal correctly classified cases (confusion matrix)
(contingency.table <- table(classification$class, data.set$class)) 
# 19 earthquakes were correclty predicted, 1 earthquake was confused with explosion
# No explosion has been confused with an equake

# Errors >> 1 error
classification$class != data.set$class 

# Error rate (resubstitution) >> 3.45%
print(mean(classification$class != data.set$class) * 100, 3) 

# As above, but from contingency.table
print((1 - sum(diag(contingency.table) / sum(contingency.table))) * 100, 3) 

# Posterior probabilities
round(classification$posterior, 3) 

# Grid based on initial graph (x and y axis have the same limits), resolution 100:
x <- seq(4.2, 6.8, length = 100)
y <- seq(2.6, 6.5, length = 100)
grid <- expand.grid(body = x, surface = y)

# Grid
Z <- predict(model.lda, grid)
Z1 <- predict(model.lda.equal.prior, grid)

z <- Z$posterior[, 2] - Z$posterior[, 1]
z1 <- Z1$posterior[, 2] - Z1$posterior[, 1]

colors <- ifelse(Z$class == 'equake', 'grey80', 'mistyrose1')
plot(grid, col = colors, pch = 15)

text(data.set$body, data.set$surface, c(rep('Q', 20), rep('E', 9)), col =  c(rep(1, 20), rep(2, 9)))

# Decision boundary
contour(x, y, matrix(z, 100), 
        level = 0, 
        add = TRUE,
        lwd = 2, drawlabels = FALSE,
        col = 8) 

# Decision boundary (for prior)
contour(x, y, matrix(z1, 100), 
        level = 0, 
        add = TRUE,
        lty = 2,
        lwd = 2, drawlabels = FALSE) 



#############################
# Wines data - three classes
#############################

# Forina, M. et al. (1991)
# These data are the results of a chemical analysis of wines grown in the same
# region in Italy, but derived from three different cultivars. The analysis 
# determined the quantities of 13 constituents found in each of the three types of wines.

data.set <- read.table('http://www.drizzt.home.amu.edu.pl/images/DADA_AIPD/wine.txt', header = TRUE, sep = ',')
skimr::skim(data.set)

##########################################
###                 LDA                ###
##########################################

data.set$Class <- as.factor(data.set$Class)

model.lda <- lda(Class ~ Alcohol + Flavanoids, 
                 data = data.set)
classification.lda <- predict(model.lda)

# Confusion matrix:
(contingency.table.lda <- table(data.set$Class, classification.lda$class))
# Class 1 is mistaken 3 times with class 2
# Class 2 is mistaken 4 times with class 1 and 7 times with class 3
# Class 3 is predicted correctly


# Quality of classification - 92% (there is no default treshold)
# 1 - error rate (resubsitution)
100 * sum(diag(contingency.table.lda)) / sum(contingency.table.lda) 

# Sizes of classified groups
table(data.set$Class) 

# Scatterplot
plot(data.set[, c('Alcohol', 'Flavanoids')],
     type = 'n',
     xlab = 'Alcohol', ylab = 'Flavanoids')

with(data.set, text(Alcohol, Flavanoids, data.set$Class, 
                    cex = 0.7, 
                    col = as.numeric(data.set$Class)))

# Grid:
x <- seq(11, 15, length = 100)
y <- seq(0.0, 5.5, length = 100)
grid <- expand.grid(Alcohol = x, Flavanoids = y)

# Predict on a grid
Z.LDA <- predict(model.lda, grid)

# Boundary between class 1 and two others
z1.LDA <- Z.LDA$posterior[, 1] - pmax(Z.LDA$posterior[, 2], Z.LDA$posterior[, 3]) 

# Boundary between class 2 and two others
z2.LDA <- Z.LDA$posterior[, 2] - pmax(Z.LDA$posterior[, 1], Z.LDA$posterior[, 3]) 

# Boundary between class 3 and two others (not needed - two first are sufficient)
# z3.LDA <- Z.LDA$posterior[, 3] - pmax(Z.LDA$posterior[, 1], Z.LDA$posterior[, 2])

# Contours on the graph:
contour(x, y, matrix(z1.LDA, 100), level = 0, add = TRUE, lwd = 2, drawlabels = FALSE)
contour(x, y, matrix(z2.LDA, 100), level = 0, add = TRUE, lwd = 2, drawlabels = FALSE)
# contour(x, y, matrix(z3.LDA, 100), level = 0, add = T, labels = '', lwd = 2, col = 'red') # Additional line (covers up with two first)



##########################################
###                 QDA                ###
##########################################

model.qda <- qda(Class ~ Alcohol + Flavanoids, data = data.set)

classification.qda <- predict(model.qda)

(contingency.table.qda <- table(data.set$Class, classification.qda$class))
# Class 1 is mistaken 2 times with class 2
# Class 2 is mistaken 4 times with class 1 and 2 times with class 3
# Class 3 is mistaken 2 times with class 2

# For comparison LDA confusion matrix:
(contingency.table.lda <- table(data.set$Class, classification.lda$class))


# 1 - error rate (resubstitution) >> 93.82% vs 92.1% in LDA
100 * sum(diag(contingency.table.qda)) / sum(contingency.table.qda)

# Boundaries:
Z.QDA <- predict(model.qda, grid)
z1.QDA <- Z.QDA$posterior[, 1] - pmax(Z.QDA$posterior[, 2], Z.QDA$posterior[, 3])
z2.QDA <- Z.QDA$posterior[, 2] - pmax(Z.QDA$posterior[, 1], Z.QDA$posterior[, 3])
# z3.QDA <- Z.QDA$posterior[, 3] - pmax(Z.QDA$posterior[, 1], Z.QDA$posterior[, 2])

# Contours:
contour(x, y, matrix(z1.QDA, 100), level = 0, add = TRUE, labels = '', lwd = 2, lty = 3, col = 'blue', drawlabels = FALSE)
contour(x, y, matrix(z3.QDA, 100), level = 0, add = TRUE, labels = '', lwd = 2, lty = 3, col = 'blue', drawlabels = FALSE)
# contour(x, y, matrix(z3.QDA, 100), level = 0, add = T, labels = '', lwd = 2, lty = 2, col = 'blue')






data.set <- read.table('http://www.drizzt.home.amu.edu.pl/images/DADA_AIPD/brach.txt', 
                       header = TRUE)
# The data include 163 shells of brachiopods Glottidia collected from seven different
# localities in the North, Central, or South America.
# VENT - ventral valve length [mm]; 
# DORS - dorsal valve length [mm]; 
# LEFT  - left ventral septum [mm]; 
# RIGHT  - right ventral septum [mm]; 
# MEDIAN  - median dorsal septum [mm]; 
# WIDTH - shell width [mm]; 
# LOC - locality [sites: 1, 2, 3, 4, 5, 6, 7];
skimr::skim(data.set)
data.set$LOC <- as.factor(data.set$LOC)

table(data.set$LOC) 
# Seven classes - watchout! There are only 2 shells in location 5
# Too little for QDA, not a problem for LDA

model.lda <- lda(LOC ~ ., data = data.set)
classification.lda <- predict(model.lda)

# Confusion matrix
(contingency.table.lda <- table(data.set$LOC, classification.lda$class))

# Resub. error rate
100 - 100 * sum(diag(contingency.table.lda)) / sum(contingency.table.lda) 

par(mfrow = c(2, 1))

# LDA:
new.df <- as.matrix(data.set[, 1:6]) %*% model.lda$scaling[, 1:2]
plot(new.df, type = 'n') 
text(new.df, as.character(data.set$LOC), cex = 0.7, col = as.numeric(data.set$LOC))

# PCA:
new.df.pca <- prcomp(data.set[, 1:6], scale. = TRUE)
plot(new.df.pca$x, type = 'n') 
text(new.df.pca$x, as.character(data.set$LOC), cex = 0.7, col = as.numeric(data.set$LOC))

# Too little observations to estimate sigma_5
model.qda <- qda(LOC ~ ., data = data.set)  

# new dimension = min(p, K-1)
# p - number of variables, K - number of classes

# dim(Cov) = 6 x 6;
# par = 1 + 2 + ...+ 6 = 21
# LDA: param: 7 * 6 (mean) + 21 (var) = 42 + 21 = 63
# QDA: param: 7 * 6 (mean) + 7 * 21 (var) = 42 + 147 = 189

# We remove group with 2 observations
df <- data.set[data.set$LOC != '5', ]
df$LOC

# df$LOC  there is no 5 5 , but 5 is visible in Levels > drop Level
df$LOC <- droplevels(df$LOC)


model.qda <- qda(LOC ~ ., df)
classification.qda <- predict(model.qda)
(contingency.table.qda <- table(df$LOC, classification.qda$class))

# Resubstitution error (vs 11.65 in LDA:
100 - 100 * sum(diag(contingency.table.qda))/sum(contingency.table.qda) 



##########################################
###         Canonical variables        ###
##########################################

# Lubischew (1962).
# There are three species of flea-beetles: Ch. concinna, Ch. heptapotamica, and Ch. heikertingeri,
# and 6 measurements on each. 
# 1 = width of the first joint of the first tarsus in microns (the sum of measurements for both tarsi).
# 2 = the same for the second joint. 
# 3 = the maximal width of the head between the external edges of the eyes in 0.01 mm. 
# 4 = the maximal width of the aedeagus in the fore-part in microns.
# 5 = the front angle of the aedeagus (1 unit = 7.5 degrees). 
# 6 = the aedeagus width from the side in microns.

# 6 features, 3 classes  >> p = 6, k = 3, min(p,k-1) = min(6,2) = 2
# 2 dimensions >> we will be in 2D

data.set <- read.table('http://www.drizzt.home.amu.edu.pl/images/DADA_AIPD/flea.txt', 
                       header = TRUE)
data.set$species <- as.factor(data.set$species)

skimr::skim(data.set)

# min(p, K - 1) = min(6, 3 - 1) = 2
model.lda <- lda(species ~ ., data = data.set) 

# mean = 0 (all data points were classified correctly)
mean(predict(model.lda)$class != data.set$species)

# Proportion of trace - importance of new variables
model.lda 

plot(as.matrix(data.set[, -7]) %*% model.lda$scaling, 
     pch = as.numeric(data.set[, 7]) + 15, 
     col = as.numeric(data.set[, 7]))
legend('topright', 
       legend = c('Concinna', 'Heptapotamica', 'Heikertingeri'), 
       col = 1:3, pch = 16:18)

# PCA:
model.pca <- prcomp(data.set[, 1:6], scale. = TRUE)
plot(model.pca$x, 
     pch = as.numeric(data.set[, 7]) + 15, 
     col = as.numeric(data.set[, 7]))



##########################################
###    Kernel estimators of density    ###
##########################################
data.set <- read.table('http://www.drizzt.home.amu.edu.pl/images/DADA_AIPD/earthquake.txt', header = TRUE)

# Different method of selecitng bandwith (smoothness parameter)
d1 <- density(data.set$body, kernel = 'gaussian') 

d2 <- density(data.set$body, bw = 'nrd', kernel = 'gaussian')

# Recommend method to estimate kernel (with 2 mods)
d3 <- density(data.set$body, bw = 'SJ', kernel = 'gaussian') 

# Histogram of body variable
hist(data.set$body, breaks = 10, freq = FALSE, col = 2) 

# Kernel estimators plot
lines(d1, lwd = 2, col = 1) 
lines(d2, lwd = 2, col = 4)
lines(d3, lwd = 2, col = 5)

legend('topright', c('bw.nrd0', 'bw.nrd', 'bw.SJ'), col = c(1,4,5), lwd = 2)

# Kernel estimator for groups
d1 <- density(data.set[data.set$class == 'equake',]$body, from = 4.5, to= 6.6) 
d2 <- density(data.set[data.set$class == 'explosn',]$body, from = 4.5, to = 6.6)
plot(d2, lwd = 2, main = 'Body density')

# We assign new observation to the class where density is bigger
lines(d1, lwd = 2, col = 2) 


##########################################
###       Naive Bayes classifier       ###
##########################################
# Naive - because we assume that variables are not correlated
# Good with many variables
head(iris)
skimr::skim(iris)

# For NaiveBayes() command
library(klaR) 

# NB - naive bayes, by default normal distribution
# 3 (classes) x 4 (features) = 12 normal distributions,
# 12 x 2 = 24 parameters

model.nb.normal <- NaiveBayes(Species ~ ., 
                              # For normal densities
                              data = iris) 

# Confusion matrix:
(classification.nb.normal <- table(predict(model.nb.normal, iris)$class, 
                                   iris$Species))

# Kernel version
model.nb.kernel <- NaiveBayes(Species ~ ., 
                              data = iris, 
                              # For kernel densities
                              usekernel = TRUE) 
# Confusion matrix:
(classification.nb.kernel <- table(predict(model.nb.kernel, iris)$class, 
                                   iris$Species))

model.lda <- lda(Species ~ ., data = iris)
model.qda <- qda(Species ~ ., data = iris)
(classification.lda = table(predict(model.lda, iris)$class, iris$Species))
(classification.qda = table(predict(model.qda, iris)$class, iris$Species))

# Resub. error rates
# NB normal distribution
1 - sum(diag(classification.nb.normal)) / nrow(iris) # 4%
# NB kernel densities
1 - sum(diag(classification.nb.kernel)) / nrow(iris) # 4%
# LDA
1 - sum(diag(classification.lda)) / nrow(iris) # 2%
# QDA
1 - sum(diag(classification.qda)) / nrow(iris) # 2%

# CV - LOO error rate
# CV (LOO) from lda() command
model.lda.cv <- lda(Species ~ .,
                    data = iris,
                    CV = TRUE) 

# Prediction for LOO
model.lda.cv$class 

# LDA LOO error rate; 2% (as above)
100 * mean(model.lda.cv$class != iris$Species) 

# CV (LOO) from qda() command
model.qda.cv <- qda(Species ~ ., data = iris, CV = TRUE) 

# Prediction for LOO
model.qda.cv$class 

# QDA LOO error rate; 2.67% (as above)
100 * mean(model.qda.cv$class != iris$Species) 

# errorest() examples
#LOO for iris data set (LDA)
errorest(Species ~ ., 
         data = iris, 
         model = lda, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(k = nrow(iris))) 

# LOO for iris data set (QDA)
errorest(Species ~ .,
         data = iris, 
         model = qda, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(k = nrow(iris))) 

# LOO for iris data set (NB Kernel)
errorest(Species ~ ., 
         data = iris, 
         model = NaiveBayes, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(k = nrow(iris)), 
         use.kerrnel = TRUE) 

# LOO for iris data set (NB Normal)
errorest(Species ~ ., 
         data = iris, 
         model = NaiveBayes, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(k = nrow(iris)), 
         use.kerrnel = FALSE) 




##########################################
###         Classification tree        ###
##########################################

# For rpart() command
library(rpart) 

# For tree() & cv.tree() commands
library(tree) 
?kyphosis

head(kyphosis)
skimr::skim(kyphosis)

#
model.Gini <- rpart(Kyphosis ~ ., 
                    data = kyphosis, 
                    method = 'class', 
                    parms = list(split = 'gini'))


model.Cross_Entropy <- rpart(Kyphosis ~ ., 
                             data = kyphosis, 
                             method = 'class', 
                             parms = list(split = 'information'))
# Hard to see the tree
summary(model.Gini) 
plot(model.Gini)
text(model.Gini, use.n = TRUE, cex = 0.8)


model.Gini.tree <- tree(Kyphosis ~ ., 
                        data = kyphosis, 
                        # Could be different than form rpart package
                        split = 'gini') 

# Less info but easier to read
model.Gini.tree 

# Resub. error rate
summary(model.Gini.tree) 
plot(model.Gini.tree)
text(model.Gini.tree)

# LOO error rate of unpruned tree (Gini) from rpart package: 19.75%
errorest(Kyphosis ~ ., 
         data = kyphosis, 
         model = rpart, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata, type = 'class'),
         est.para = control.errorest(k = nrow(kyphosis)), 
         method = 'class', 
         parms = list(split = 'gini')) 

# LOO error rate of unpruned tree (Gini) from tree package: 20.99%
errorest(Kyphosis ~ ., data = kyphosis, 
         model = tree, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata, type = 'class'),
         est.para = control.errorest(k = nrow(kyphosis)), 
         split = 'gini') 

# LOO error rate of unpruned tree (Cross-entropy) from rpart package: 25.93%
errorest(Kyphosis ~ ., 
         data = kyphosis, 
         model = rpart, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata, type = 'class'),
         est.para = control.errorest(k = nrow(kyphosis)), 
         method = 'class', 
         parms = list(split = 'information')) 

# LOO error rate of unpruned tree (Cross-entropy) from tree package: 24.49%
errorest(Kyphosis ~ ., 
         data = kyphosis, 
         model = tree, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata, type = 'class'),
         est.para = control.errorest(k = nrow(kyphosis))) 

# Prunning; k corresponds to alpha in weakest-link pruning >>
# we remove some leafs w/o impact on prediction quality
(model.Gini.prune <- cv.tree(model.Gini.tree, 
                             FUN = prune.misclass)) 

# Size (number of nodes)
plot(model.Gini.prune$size, model.Gini.prune$dev, type = 'b', pch = 20)

# Alpha
plot(model.Gini.prune$k, model.Gini.prune$dev, type = 'b', pch = 20) 

# Optimal size
(best.size <- model.Gini.prune$size[which.min(model.Gini.prune$dev)]) 

# Optimum (prunned) tree
pruned.tree <- prune.misclass(model.Gini.tree, best = best.size) 

plot(pruned.tree)
text(pruned.tree)
summary(pruned.tree)


# Nice plot of tree
library(rattle) 
fancyRpartPlot(model.Gini)

# As above but with different colors
fancyRpartPlot(model.Gini, palettes = c('Greens', 'Oranges')) 

# For weather data set
library(rattle.data) 

# Weather in Canberra (Autralia)
head(weather)
skimr::skim(weather)

model.weather <- rpart(RainTomorrow ~ ., 
                       data = weather[, setdiff(names(weather), c('Date', 'Location', 'RISK_MM'))])
summary(model.weather)
fancyRpartPlot(model.weather)

#cp - complexity (by standard 0.03 - 0.05)
model.weather.pruned <- prune(model.weather, 
                              cp = 0.05)
fancyRpartPlot(model.weather.pruned)





##########################################
###            Random forest           ###
##########################################
# For randomForest() command
library(randomForest) 

#second method - boosting
model.bagging <- randomForest(Kyphosis ~ ., 
                              importance = TRUE, 
                              data = kyphosis, 
                              ntree = 10000, 
                              # Bagging (all the covariates at each split)
                              mtry = 3) 

# Confusion matrix and OOB (out of bag) error rate
model.bagging 

# Permutation variable importance (higher value mean bigger imprtance)
importance(model.bagging, type = 1) 

# Mean decrease variable importance (higher value mean bigger imprtance)
importance(model.bagging, type = 2) 

#plot of importances
varImpPlot(model.bagging, pch = 20) 
(model.rf <- randomForest(Kyphosis ~ ., importance = TRUE,
                          data = kyphosis, 
                          ntree = 10000, 
                          # Random forest with 2 covarietes at each split 
                          mtry = 2)) 

# Permutation variable importance (higher value mean bigger imprtance)
importance(model.rf, type = 1) 

# Mean decrease variable importance (higher value mean bigger imprtance)
importance(model.rf, type = 2) 

# Plot of importances
varImpPlot(model.rf, pch = 20) 





##########################################
###            Neural nets             ###
##########################################

# Only single hidden layer
library(nnet) 
#jak species (dataset iris) zalezy od 4 paramentrów (sepal/petal)
model.nn <- nnet(Species ~ ., 
                 data = iris, 
                 # Ile neuronów w warstwie ukrytej? Size of hidden layer = 5
                 size = 5, 
                 maxit = 1000) 
# 4 neurony (4 cechy) na wejściu
# na wyjściu - 3 klasy (species)
# w środku 5 neuronów

# 1-2: 4 * 5 weights
# 2-3: 5 * 3 weights
# bias: 5 + 3 weights
# Total: 20 + 15 + 8 = 43


# For command errorest()
library(ipred) 

errorest(Species ~ ., 
         data = iris, 
         model = nnet, 
         estimator = 'cv',
         predict = function(o, newdata) as.factor(predict(o, newdata, 'class')), size = 5, maxit = 1000)


# Function from GitHub to plot network
require(RCurl)
root.url <- 'https://gist.githubusercontent.com/fawda123'
raw.fun <- paste( root.url, '5086859/raw/cc1544804d5027d82b70e74b83b3941cd2184354/nnet_plot_fun.r', sep = '/')
script <- getURL(raw.fun, ssl.verifypeer = FALSE)
eval(parse(text = script))
# The function is now loaded in our workspace as plot.nnet. 
rm('script','raw.fun') 
# A standard illustration
plot(model.nn, nid = FALSE)
# A neural interpretation diagram. The black lines are
# positive weights and the grey lines are negative weights. 
# Line thickness is in proportion to magnitude of the weight relative to all others.
# B1 and B2 are bias layers that apply constant values to the nodes,
# similar to intercept terms in a regression model.
plot(model.nn, nid = TRUE)

plot(model.nn, pos.col = 'lightgreen', neg.col = 'lightblue', rel.rsc = 15, 
     circle.cex = 7, cex = 1.4, circle.col = 'darkred')



##########################################
###    SVM (Support Vector Machines)   ###
##########################################
library(e1071)
model.svm <- svm(Species ~ ., 
                 data = iris)

errorest(Species ~ ., 
         data = iris, 
         model = svm, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata),
         est.para = control.errorest(k = nrow(iris)))




##########################################
###         Tuning parameters          ###
##########################################
# for tune.*() commands
library(e1071) 
model.rf.tune <- tune.randomForest(Kyphosis ~ ., 
                                   data = kyphosis,
                                   # We are looking for the best m
                                   mtry = 1:3) 
# The best m
model.rf.tune$best.parameters 
plot(model.rf.tune)
# Error rate for the best m
model.rf.tune$best.performance 
# Errors of all models
model.rf.tune$performances 
model.knn.tune <- tune.knn(x = kyphosis[, 2:4], 
                           y = kyphosis$Kyphosis,
                           # We are looking for the best k
                           k = 1:10) 
plot(model.knn.tune)
model.knn.tune$performances

tune.svm(Species ~ ., 
         data = iris, 
         kernel, 
         cost = 10^(-1:2),
         # The best parameters C = 1, gamma = 0.5
         gamma = c(.5, 1, 2)) 
.Last.value$performances

model.svm <- svm(Species ~ ., 
                 data = iris, 
                 cost = 1, 
                 gamma = 0.5,
                 # SVM model for the best parameters
                 probability = TRUE) 

predict(model.svm, newdata = data.frame(Sepal.Length = 5.0, 
                                        Sepal.Width = 3.0,
                                        Petal.Length = 2.0, 
                                        Petal.Width = 0.5), 
        probability = TRUE)




##########################################
###             Deep learning          ###
##########################################
# Library for DL
library(h2o) 
# Cluster initialization
localH2O <- h2o.init(max_mem_size = '2g') 

# Data transform
dat_h2o <- as.h2o(iris) 
# pierwsze 4 kolumny - x, ostatnia - y
model.dl <- h2o.deeplearning(x = 1:4, 
                             y = 5, 
                             training_frame = dat_h2o, 
                             #tangens hiperboliczony
                             activation = 'Tanh', 
                             #3 warstwy neuronów
                             hidden = c(10, 10, 10), 
                             #500 epok
                             epochs = 500, 
                             variable_importances = TRUE)
# Prediction
predict.result <- as.data.frame(h2o.predict(model.dl, dat_h2o))$predict 
# Resubstitution error rate
sum(predict.result != iris$Species)/nrow(iris) 
plot(model.dl)
# Importance of variables
h2o.varimp_plot(model.dl) 



##########################################
###             Caret package          ###
##########################################
library(caret)
# For transparentTheme() command
library(AppliedPredictiveModeling) 

# Theme
transparentTheme(trans = 0.4) 

# Scatterplot Matrix
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = 'pairs', 
            auto.key = list(columns = 3))



# Scatterplot Matrix with Ellipses
featurePlot(x = iris[, 1:4], y = iris$Species, plot = 'ellipse', auto.key = list(columns = 3))


# Overlayed Density Plot
# Theme
transparentTheme(trans = 1) 
# gatunki różnią się głównie petal lenghts i petal width
featurePlot(x = iris[, 1:4], y = iris$Species, plot = 'density', 
            scales = list(x = list(relation = 'free'), y = list(relation = 'free')), 
            adjust = 1.5, pch = '|', layout = c(4, 1), auto.key = list(columns = 3))

# Box Plot
featurePlot(x = iris[, 1:4], y = iris$Species, plot = 'box', 
            scales = list(y = list(relation = 'free'), x = list(rot = 90)),  
            layout = c(4,1 ), auto.key = list(columns = 2))



# For data set Sonar
library(mlbench) 
data(Sonar)

# For reproducibility of the results
set.seed(1000) 

# A single 75% / 25% split of data set into two parts: trainig & testing
in.training <- createDataPartition(Sonar$Class, p = 0.75, list = FALSE)

# Basic Parameter Tuning
training <- Sonar[ in.training, ]
testing  <- Sonar[-in.training, ]

# fit.Control =
# trainControl(method = 'repeatedcv', number = 10, repeats = 10,
# classProbs = T, summaryFunction = twoClassSummary)

# metoda sprawdzania krzyżowego z powtórzeniami, 10cv, 10 repeats > 10 x 10CV

fit.control <- trainControl(method = 'repeatedcv', 
                            number = 10, 
                            repeats = 10, 
                            classProbs = TRUE)

# method - the resampling method: boot, boot632, cv, repeatedcv, LOOCV and others
# number - number of folds or number of resampling iterations
# repeats - for 'repeatedcv' only: the number of complete sets of folds to compute

set.seed(1000)

# uczenie modelu
# wnaszym zbiorze danym jest jeden factor - class, reszta to parametry
(svm.fit <- train(Class ~ ., 
                  data = training,
                  method = 'svmRadial', 
                  trControl = fit.control, 
                  preProc = c('center', 'scale'), 
                  tuneLength = 8, 
                  metric = 'Accuracy'))

# preProc - pre-processing of data: center, scale, pca, imputation and many others

# metric - summary metric: Accuracy (1 - error rate) & Kappa for classification 

# method - classifier; https://topepo.github.io/caret/available-models.html

# The Kappa statistic is a metric that compares an Observed Accuracy with an Expected Accuracy (random chance).
# Observed Accuracy is simply the number of instances that were classified correctly throughout the entire confusion matrix.
# The Expected Accuracy is defined as the accuracy that any random classifier would be expected to achieve based on the confusion matrix.
# Expected Accuracy = rowsum * colsum / N

# Kappa = (observed accuracy - expected accuracy) / (1 - expected accuracy)
# Kappa - im bliżej jedynki tym lepiej

# The kappa statistic is a measure of how closely the instances classified by the classifier matched 
# the data labeled as ground truth, controlling for the accuracy of a random classifier as measured by the expected accuracy.
# The kappa statistic for one model is directly comparable to the kappa statistic for any other model used for the same classification task.
# The kappa coefficient can range from -1 t0 1. A value of 0 indicated that the classification is no better 
# than a random classification. A negative number indicates the classification is significantly worse than random. 
# A value close to 1 indicates that the classification is significantly better than random.
# There is not a standardized interpretation of the kappa statistic.
# Classifiers built and evaluated on data sets of different class distributions can be compared 
# more reliably through the kappa statistic because of this scaling in relation to expected accuracy.


#Accuracy jest ok jeżeli grupy są zbalansowane, w przeciwnym przypadku dobrze małe grupy dołączyć do większych
# grup. 

# Confusion matrix
confusionMatrix(svm.fit) 
# Confusion matrix with additional measures (https://topepo.github.io/caret/premade/cm.jpg)
confusionMatrix(predict(svm.fit, testing), testing$Class) 

# Theme
trellis.par.set(caretTheme()) 
# Plotting the resampling profile
plot(svm.fit, pch = 20) 
# As above from ggplot2
ggplot(svm.fit) 
# Prediction
predict(svm.fit, newdata = testing) 
# Posterior probabilities
predict(svm.fit, newdata = testing, type = 'prob')


set.seed(1000)
# Regularized discriminant analysis
(rda.fit = train(Class ~ ., 
                 data = training, 
                 method = 'rda', 
                 trControl = fit.control, 
                 tuneLength = 4, 
                 metric = 'Accuracy'))

# Heatmap for parameters
plot(rda.fit, metric = 'Accuracy', plotType = 'level', scales = list(x = list(rot = 90)))
# The collection of models to compare them
(resamps = resamples(list(SVM = svm.fit, RDA = rda.fit))) 
summary(resamps)
# Comparison of classifiers
bwplot(resamps, layout = c(2, 1)) # Box plots
dotplot(resamps, metric = 'Accuracy') # Dot plots - SVM jest lepsza (statystycznie istotniejsza)

# Bland-Altman plot (difference plot, Tukey mean-difference plot)
xyplot(resamps, what = 'BlandAltman') 
# więcej punktów na górze - SVM jest lepsze

# Bland and Altman plots are extensively used to evaluate the agreement among two different methods.
# The coordinates of a given sample with values of S_1 and S_2:
# S(x, y) = ((S_1 + S_2) / 2; S_1 - S_2)
# Any two methods that are designed to measure the same parameter should have good correlation.

# Since models are fit on the same versions of the training data, it makes sense to make inferences 
# on the differences between models.
(diff.values = diff(resamps))
summary(diff.values) # t-test to compare models
bwplot(diff.values, layout = c(2, 1))
dotplot(diff.values)

# Parallel Processing
library(doMC) # For registerDoMC() function
registerDoMC(cores = 4)
# All subsequent models are then run in parallel

# Variable Importance
(svm.imp = varImp(svm.fit, scale = FALSE))
plot(svm.imp)
