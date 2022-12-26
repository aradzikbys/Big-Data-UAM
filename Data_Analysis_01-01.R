# Data analysis - 01 - linear regression, models comparison

##########
# EX01
#########
# Using the cars dataset, draw a scatterplot where the first variable is
# speed and the second is braking distance.
# Fit a regression line to this and plot it on the graph.
# Try to match the square and cubic function. Add them to the chart.
# Suggest a model based on its quality.

# Clear enviroment
rm(list = ls())

# Check data set, assign to variable
?cars
dataset01 <- cars
head(dataset01)

# Linear model (Model 1A):
model_1a = lm(formula = dist ~ speed, data = dataset01)

# Square model (Model 1B):
model_1b = lm(dist ~ poly(speed, 2, raw = TRUE), data = dataset01)

# Cubic model (Model 1C):
model_1c = lm(dist ~ poly(speed, 3, raw = TRUE), data = dataset01)


# Scatter plot using basic R functions:
plot(dataset01, pch = 20,
     xlab = 'Speed (mph)', 
     ylab = 'Distance (ft)') +

# Legend:
legend('topleft',
      legend = c('Model 1A (linear)', 'Model 1B (square)','Model 1C (cubic)'),
      lwd = 3,
      col = c(2,3,4))

# Lines:
abline(model_1a, col = 2, lwd = 2) +
lines(dataset01$speed, predict(model_1b), col = 3, lwd = 2) +
lines(dataset01$speed, predict(model_1c), col = 4, lwd = 2) 



# With ggplot:
library(ggplot2)
ggplot(data = dataset01, aes(x = speed, y = dist)) +
  geom_point(aes(x = speed, y = dist)) +
  
  # Linear model
  geom_smooth(method = 'lm', se = FALSE,
              aes(colour = '01. Linear')) +
  
  # Square model
  geom_smooth(method = 'lm', se = FALSE, formula = y ~ poly(x, 2),
              aes(colour = '02. Square')) +
  
  # Cubic model
  geom_smooth(method = 'lm', se = FALSE, formula = y ~ poly(x, 3),
              aes(colour = '03. Cubic')) +
  
    # Axis labels
  labs(x = 'Speed (mph)', y = 'Distance (ft)') +
  
  # Legend
  scale_color_manual(name = 'Model', values = c(2,3,4))



# Summary:
summary(model_1a)
summary(model_1b)
summary(model_1c)
# Multiple R-squared >> 1C
# 1A = 0.6511, 1B = 0.6673, 1C = 0.6732. 
# There is no significant difference between the models (we aim for at least 60%). 

# p-value >> 1A
# 1A= 1.49e-12, 1B = 5.852e-12, 1C = 3.074e-11
# OK for all models (<5%), the less the better


# MSE (Mean Squared Error) >> 1C (the less the better)
# 1A = 227.0704, 1B = 216.4943, 1C = 212.6872
(MSE.model_1a <- mean(resid(model_1a)^2))
(MSE.model_1b <- mean(resid(model_1b)^2))
(MSE.model_1c <- mean(resid(model_1c)^2))


# Akaike >> 1B
AIC(model_1a, model_1b, model_1c)
# AIC: 419.2, 418.8, 419.9 > > minor differences between models (<2)
# Akaike chooses 1B, but differences between models are minor (<2)


# Bayesian criterion >> 1A
BIC(model.1a, model.1b, model.1c)
# BIC: 424.9, 426.4, 429.4 > > minor differences between models (<2)
# Bayesian chooses 1A (Bayesian prefers simpler model)


##############
# ANSWER:
##############
# PROPOSED MODEL: Model 1A
# Simplest one and good enough comparing to square & cubic models.
