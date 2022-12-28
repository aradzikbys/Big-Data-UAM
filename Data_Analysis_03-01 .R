# Data analysis - 03 - classification (LDA, QDA, 1NN, NB)

#########
# The Vehicle dataset from the mlbench package contains information (18 features,
# 846 observations) about silhouettes of cars (4 types).
#     (a) Construct classification models: LDA, QDA, 1NN & NB for these data.
#     (b) Estimate the classification error for the constructed methods.
#     (c) Which of the constructed classifiers is recommendable for this dataset?
#     Justify your answer.
#     (d*) Try to visualize the results in the space formed by the first two
#     principal components.

# Clear environment
rm(list = ls())


###############################################
# 01 Libraries
###############################################

# For data set
library(mlbench)

# For kNN
library(class)

# For errorest
library(ipred) 

# For LDA and QDA
library(MASS) 

# For NaiveBayes() command
library(klaR)

# For biplot
library(ggplot2)
library(ggbiplot)
library(devtools)
install_github("vqv/ggbiplot")
library(wesanderson)

# For confusion matrix
library(caret)
library(lattice)

###############################################
# 02 Data set
###############################################

# Assign data set to variable, check data set (missing values, distribution 
# of classes, types of attributes):
data(Vehicle)
dataset03 <- Vehicle

# Dimensions (how many instances (rows) and attributes (columns) the data contains?)
dim(dataset03)

# Example of data - first 5 rows:
head(dataset03)

# skimr - no missing values
skimr::skim(dataset03)

# 1 factor attribute - class (4 levels: bus, opel, saab, van), rest numerical
str(dataset03)
sapply(dataset03, class)
levels(dataset03$Class)

# Classes are distributed equally, enough data for QDA
table(dataset03$Class)

############

# Training and testing data sets (for 1NN):

# Indices for training:
n = nrow(dataset03)
train = sample(1:n, size = round(0.5*n), replace=FALSE)
train <- sort(train)
length(train)

# Indices for testing:
idx = seq(n)
test <- idx[!idx %in% train]
length(test)


###############################################
# 03 Models - 1NN, LDA, QDA, NB (normal and kernel)
###############################################

# Models
                # train data - index + numeric variables
model_1NN <- knn(dataset03[train, 1:18],
                # test data - index + numeric variables
                dataset03[test, 1:18],
                # train data classes 
                dataset03$Class[train],
                # number of neighbors
                k = 1)

model_LDA <- lda(Class ~ .,
                 data = dataset03)

model_QDA <- qda(Class ~ .,
                 data = dataset03)

model_nb_normal <- NaiveBayes(Class ~ ., 
                              data = dataset03) 

model_nb_kernel <- NaiveBayes(Class ~ ., 
                              data = dataset03,
                              usekernel = TRUE) 


###############################################
# 04 Prediction errors
###############################################

# Based on confusion matrix

# 1NN >> accuracy 0.6028, kappa = 0.5209
# Prediction error = 1 - 0.6028 = 0.3972 >> 40%
confusionMatrix(data = model_1NN, reference = dataset03$Class[test])

# LDA >> accuracy 0.7979, kappa = 0.7304
# Prediction error = 1 - 0.7979 = 0.2021 >> 20%
confusionMatrix(data = predict(model_LDA, dataset03)$class, reference = dataset03$Class)

# QDA >> accuracy 0.9161, kappa = 0.8881 
# Prediction error = 1 - 0.9161 = 0.0839 >> 8%
confusionMatrix(data = predict(model_QDA, dataset03)$class, reference = dataset03$Class) 

# NB Normal >> accuracy 0.4728, kappa = 0.3025
# Prediction error = 1 - 0.4728 = 0.5272 >> 53%
confusionMatrix(data = predict(model_nb_normal, dataset03)$class, reference = dataset03$Class)

# NB Kernel >> accuracy 0.6596, kappa = 0.5474
# Prediction error = 1 - 0.6596 = 0.3404 >> 34%
confusionMatrix(data = predict(model_nb_kernel, dataset03)$class, reference = dataset03$Class)


# Based on errorest (cross validation & bootstrap)

# 1NN CV >> 0.3475
mymod <- function(formula, 
                  data, 
                  l = 1) {
  ipredknn(formula = formula, data = data, k = l)
  
}
errorest(Class ~ ., 
         data = dataset03, 
         model = mymod, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata,  'class'), 
         est.para = control.errorest(k = nrow(dataset03)), 
         l = 1)

# 1NN bootstrap >> 0.3642
errorest(Class ~ ., 
         data = dataset03, 
         model = ipredknn, 
         estimator = 'boot', 
         predict = function(o, newdata) predict(o, newdata, 'class'), 
         est.para = control.errorest(nboot = 100), 
         k = 1)

# LDA CV>> 0.221
errorest(Class ~ ., 
         data = dataset03, 
         model = lda, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(k = nrow(dataset03)))

# LDA bootstrap >> 0.226
errorest(Class ~ ., 
         data = dataset03, 
         model = lda, 
         estimator = 'boot', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(nboot = 100))

# QDA CV >> 0.1442 
errorest(Class ~ ., 
         data = dataset03, 
         model = qda, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(k = nrow(dataset03)))

# QDA bootstrap >> 0.1593 
errorest(Class ~ ., 
         data = dataset03, 
         model = qda, 
         estimator = 'boot', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(nboot = 100))

# NB Normal CV >> 0.5414 
errorest(Class ~ ., 
         data = dataset03, 
         model = NaiveBayes, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(k = nrow(dataset03)), 
         use.kerrnel = FALSE)

# NB Normal bootstrap >> 0.5511 
errorest(Class ~ ., 
         data = dataset03, 
         model = NaiveBayes, 
         estimator = 'boot', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(nboot = 100), 
         use.kerrnel = FALSE)

# NB Kernel >> error for CV, with bootstrap: 0.5435
errorest(Class ~ ., 
         data = dataset03, 
         model = NaiveBayes, 
         estimator = 'boot', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(nboot = 100), 
         use.kerrnel = TRUE)

##############
# ANSWER:
##############
# Chosen model: QDA (lowest prediction error, kappa closest to 1).
# There is enough data in each class to correctly work with that method.


###############################################
# 05 Visualization
###############################################

# Two groups to compare data:
real <- c(dataset03$Class)
predicted <- c(predict(model_LDA, dataset03)$class)

# PCA:
dataset03_pca <- prcomp(dataset03[,-19], scale. = TRUE)

# Biplot:
ggbiplot(dataset03_pca, groups = real, labels = dataset03$Class) +
  geom_point(aes(shape = predicted, colour=real), size = 2.5) +
  labs(shape = 'Predicted', colour = 'Actual') +
  scale_color_manual(values= wes_palette('GrandBudapest1', n = 4))
