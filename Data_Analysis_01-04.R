# Data analysis - 01 - linear models criteria validation

##########
# EX04
#########
# The UsingR homeprice dataset contains information on homes sold in New Jersey in 2001. 
# Did the number of toilets (half) affect price (sale)?
# Are the assumptions of the model met?

# Clear enviroment
rm(list = ls())

# Load package
# install.packages("UsingR")
library(UsingR)
?homeprice

# Assign data to the data set
dataset04 <- homeprice
head(dataset04)
  
# Create scatter plot:
plot(data = dataset04,
     sale ~ half, 
     pch = 20, cex = 1.5,
     xlab = 'Number of toilets', ylab = 'Sale (k $)')

# Linear model (Model 4A):
model_4a = lm(sale ~ half, data = dataset04)
abline(model_4a, col = 2, lwd = 2)


# With ggplot:
ggplot(dataset04, aes(x = half, y = sale)) + geom_point(size = 2) +
  geom_smooth(method = 'lm', se = FALSE, colour = 2) +
  labs(x = 'Number of toilets (half)', y = 'Sale price (k $)')

## Summary:
summary(model_4a)
# Multiple R-squared: 0.1554 (we aim for at least 60%)
# >> number of half toilets affects final price in 15%
# p-value: 0.03436 (less than 5% >> OK)

cor.test(~ sale + half, data = dataset04) 
# cor = 0.3941621 >> weak positive correlation


##############
# ANSWER:
##############
# Assumptions of the model are met, although correlation between number of half-bathrooms
# and final house price is very weak (cor = 0.39). Supposedly, the bigger area of the house,
# the higher price (and usually in bigger houses there are more bathrooms).
# Possibly, more variables explain price difference better.



########################
# Additional analysis:
#######################

# We can start with all variables (except 'list') within null hypothesis.
model_4b <- lm(sale ~ .-list, data = dataset04)

summary(model_4b)
# Adjusted R-squared: 0.8879 (much better than with previous model >> with more
# variables, R will be increasing). With significance level at 5% we should
# remove rooms and bedrooms from the model.

# Based on summary, most influential variables are neighborhood, number of toilets
# (half) and in some part number of bathrooms (full).
model_4c <- lm(sale ~ full + half + neighborhood, data = dataset04)

summary(model4c)
# R-squared: 0.8577 (worse than with model with more variables, but still
# pretty decent, more than 60%).
# With such model residuals are almost perfectly symmetrical (-90.5 to 90.3),
# p-value = 2.436e-11 (vs 3.686e-11 in model with all variables).

# Compare models with Akaike (AIC), Bayesian (BIC) and Anova:
AIC(model_4a, model_4b, model_4c)     # Akaike picks model with all variables,
                                      # although there is small difference between 4b anc 4c
BIC(model_4a, model_4b, model_4c)     # BIC as well (small diff. between 4b and 4c)
anova(model_4a, model_4b, model_4c)   # Anova choose model with all variables
