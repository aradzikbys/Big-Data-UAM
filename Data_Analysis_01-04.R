# Data analysis - 01

##########
# EX04
#########
# The UsingR homeprice dataset contains information on homes sold in New Jersey in 2001. 
# Did the number of toilets (half) affect price (sale)?
# Are the assumptions of the model met?

# Load package
library(UsingR)
?homeprice

# Assign data to the data set
dataset.04 <- homeprice
dataset.04

# Create scatter plot:
plot(data = dataset.04,
     sale ~ half, 
     pch = 20, cex = 1.5,
     xlab = 'Number of toilets', ylab = 'Sale (k $)')

# Linear model (Model 4A):
model.4a = lm(sale ~ half, data = dataset.04)
abline(model.4a, col = 'darkgreen', lwd = 2)

## Summary:
summary(model.4a)
# Multiple R-squared: 0.1554 (we aim for at least 60%) >> number of half toilets
# affects final price in 15%
# p-value: 0.03436 (less than 5% >> OK)

cor.test(~ sale + half, data = dataset.04) 
# cor = 0.3941621 >> weak positive correlation

# Assumptions of the model are met, although correlation between number of half-bathrooms
# and final house price is very weak (cor = 0.39). Supposedly, the bigger area of the house,
# the higher price (and usually in bigger houses there are more bathrooms).