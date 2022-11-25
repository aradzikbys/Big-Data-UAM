# Data analysis - 01

##########
# EX01
#########
# Using the cars dataset, draw a scatterplot where the first variable is
# speed and the second is braking distance.
# Fit a regression line to this and plot it on the graph.
# Try to match the square and cubic function. Add them to the chart.
# Suggest a model based on its quality.

?cars

dataset.01 <- cars

# Linear model (Model 1A):
model.1a = lm(formula = dist ~ speed, data = dataset.01)

# Square model (Model 1B):
model.1b = lm(dist ~ poly(speed, 2, raw = TRUE), data = dataset.01)

# Cubic model (Model 1C):
model.1c = lm(dist ~ poly(speed, 3, raw = TRUE), data = dataset.01)


# Create scatter plot with legend and models:
plot(dataset.01, pch = 20,
     xlab = 'Speed (mph)', 
     ylab = 'Distance (ft)') +

# Legend:
legend('topleft',
      legend = c('Model 1A (linear)', 'Model 1B (square)','Model 1C (cubic)'),
      lwd = 3,
      col = c('darkgreen', 'palegreen3','cornsilk3'))

# Lines:
abline(model.1a, col = 'darkgreen', lwd = 2) +
lines(dataset.01$speed, predict(model.1b), col = 'palegreen3', lwd = 2) +
lines(dataset.01$speed, predict(model.1c), col = 'cornsilk3', lwd = 2) 


# Summary:
summary(model.1a)
summary(model.1b)
summary(model.1c)
# Multiple R-squared: 1A = 0.6511, 1B = 0.6673, 1C = 0.6732. OK for all models,
# we aim for at least 60%. There is no significant difference between the models.

# p-value: 1A = 1.49e-12, 1B = 5.852e-12, 1C = 3.074e-11
# OK for all models (<5%), the less the better.


# MSE (Mean Squared Error)
# 1A = 227.0704, 1B = 216.4943, 1C = 212.6872(the less the better)
(MSE.model.1a <- mean(resid(model.1a)^2))
(MSE.model.1b <- mean(resid(model.1b)^2))
(MSE.model.1c <- mean(resid(model.1c)^2))


# Akaike
AIC(model.1a, model.1b, model.1c)
# AIC: 419.2, 418.8, 419.9 > > minor differences between models (<2)
#       > Akaike chooses 1B, but  differences between models are minor (<2)

# Bayesian criterion
BIC(model.1a, model.1b, model.1c)
# BIC: 424.9, 426.4, 429.4 > > minor differences between models (<2)
#       > Bayesian chooses 1A (simpler models are preffered)


# PROPOSED MODEL: Model 1A
# Simplest one and good enough comparing to square & cubic models.























