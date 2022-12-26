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

# for data set
library(mlbench)
# for CreateDataPartition
library(caret)
# for kNN
library(class)
# for errorest
library(ipred) 
# For LDA and QDA
library(MASS) 
# For NaiveBayes() command
library(klaR)
# Other:
library(dplyr)
library(ggplot2)
library(lattice)
# for biplot
library(ggbiplot)
library(devtools)
install_github("vqv/ggbiplot")


###############################################
# 03 Data set
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

# indexes for training:
n = nrow(dataset03)
train = sample(1:n, size = round(0.5*n), replace=FALSE)
train <- sort(train)
length(train)

# indexes for testing:
idx = seq(n)
test <- idx[!idx %in% train]
length(test)

###############################################
# 03 Models - 1NN, LDA, QDA, NB (normal and kernel)
###############################################

# Models
model_1NN <- knn(dataset03[train, 1:18],
                 dataset03[test, 1:18],
                 dataset03$Class[train],
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

# 1NN
(conf_1NN <- table(Class = dataset03$Class[test], Predict = model_1NN))
1 - sum(diag(conf_1NN)) / nrow(dataset03) # 68%

# LDA
(conf_LDA <- table(predict(model_LDA, dataset03)$class, dataset03$Class))
1 - sum(diag(conf_LDA)) / nrow(dataset03) # 20%

# QDA
(conf_QDA <- table(predict(model_QDA, dataset03)$class, dataset03$Class))
1 - sum(diag(conf_QDA)) / nrow(dataset03) # 8%

# NB Normal
(conf_nb_normal <- table(predict(model_nb_normal, dataset03)$class, dataset03$Class))
1 - sum(diag(conf_nb_normal)) / nrow(dataset03) # 53%

# NB Kernel
(conf_nb_kernel <- table(predict(model_nb_kernel, dataset03)$class, dataset03$Class))
1 - sum(diag(conf_nb_kernel)) / nrow(dataset03) # 34%


# Based on errorest:

# 1NN >> error(?)
mymod <- function(formula, 
                  data, 
                  l = 1) {
  ipredknn(formula = formula, data = data, k = l)
  
}
errorest(class ~ ., 
         data = dataset.03, 
         model = mymod, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata,  'class'), 
         est.para = control.errorest(k = nrow(data.set)), 
         l = 1)


# LDA >> 0.221
errorest(Class ~ ., 
         data = dataset03, 
         model = lda, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(k = nrow(dataset03))) 

# QDA >> 0.144
errorest(Class ~ ., 
         data = dataset03, 
         model = qda, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(k = nrow(dataset03))) 

# NB Normal >> 0.5414 
errorest(Class ~ ., 
         data = dataset03, 
         model = NaiveBayes, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(k = nrow(dataset03)), 
         use.kerrnel = FALSE)

# NB Kernel >> 0.5414 
errorest(Class ~ ., 
         data = dataset03, 
         model = NaiveBayes, 
         estimator = 'cv', 
         predict = function(o, newdata) predict(o, newdata)$class, 
         est.para = control.errorest(k = nrow(dataset03)), 
         use.kerrnel = TRUE)

##############
# ANSWER:
##############
# Chosen model: QDA (lowest prediction error), there is enough data in each
# class to correctly work with that method.


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
  geom_point(aes(colour=real, shape = predicted), size = 2.5)


