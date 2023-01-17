# Data analysis - 01 - linear models criteria validation

##########
# EX04
#########
# The UsingR homeprice dataset contains information on homes sold in New Jersey in 2001. 
# Did the number of toilets (half) affect price (sale)?
# Are the assumptions of the model met?

# Clear enviroment
rm(list = ls())

# Load libraries
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
ggplot(data = dataset04, aes(x = half, y = sale)) +
  
  geom_point(data = dataset04, aes(x = half, y = sale)) +
  
  geom_smooth(data = dataset04,
              method = 'lm',
              color = 2,
              aes(x = half, y = sale)) +
  
  labs(x = 'Number of toilets',
       y = 'Sale price (k$)')

## Summary:
summary(model_4a)
# Adjusted R-squared: 0.1241 (below 0.6) - number of toilets (half) explains
# variance in the price in 12%.
# p-value: 0.03436 (below, but very close to significance level = 0.05)
# Residuals are not symmetrical.

cor.test(~ sale + half, data = dataset04) 
# cor = 0.3941621 >> weak positive correlation


##############
# ANSWER:
##############
# Assumptions of the model are not met (p value is very close to 0.05 and adjusted
# R-squared is below 0.6), also correlation between number of half-bathrooms and
# final house price is very weak (cor = 0.39).
# Possibly, other variables explain price difference better.


########################
# Additional analysis:
#######################

# We can start with all variables (except 'list'):
model_4b <- lm(sale ~ .-list, data = dataset04)

summary(model_4b)
# Adjusted R-squared: 0.8879 (vs 0.1241 in model_4a)
# p-value: 3.686e-11 (vs 0.03436 in model_4a)

# Most influential variables are neighborhood***, half** and in some part full.
# With significance level at 5% we should remove rooms and bedrooms from the model.


model_4c <- lm(sale ~ full + half + neighborhood, data = dataset04)

summary(model4c)
# Adjusted R-squared: 0.8577 (vs 0.8879 in model_4b and 0.1241 in model_4a)
# p-value: 2.436e-11 (vs 3.686e-11 in model_4b and 0.03436 in model_4a)

# Compare models with Akaike (AIC), Bayesian (BIC) and Anova:
AIC(model_4a, model_4b, model_4c)     # Akaike picks model with all variables (beside list price),
                                      # although there is small difference between 4b anc 4c
BIC(model_4a, model_4b, model_4c)     # BIC as well (small diff. between 4b and 4c)

##############
# ANSWER:
##############
# The best model is model_4b (model with all variables, except listing price).
