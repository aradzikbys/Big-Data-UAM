# Data analysis - 01

##########
# EX02
#########
# Make a correlation diagram and find a regression line for the house price
# and number of rooms data. With significance level = 5%, does the number of rooms
# have an impact on the price? What will be the price of the apartment
# two-room apartment according to the estimated model?
# Are the assumptions of the model fulfilled?

# Data vectors
price <- c(300, 250, 400, 550, 317, 389, 425, 289, 389, 559) 
rooms <- c(3, 3, 4, 5, 4, 3, 6, 3, 4, 5)

dataset.02 <- data.frame(price,rooms) # Combine vectors to data frame
dataset.02

# Create scatter plot:
plot(data = dataset.02,
     price  ~ rooms, # Watchout: PRICE should be on y axis!
     pch = 20, cex = 1.5,
     xlab = 'Number of rooms', ylab = 'Price (k PLN)')

# Linear model (Model 2A):
model.2a = lm(price ~ rooms, data = dataset.02)
abline(model.2a, col = 'darkgreen', lwd = 2)

# Summary
summary(model.2a)
# Adjusted R-squared: 0.4846  (price of the room depends in 48% on number of rooms)
# p-value: 0.01521 (less than 0.05, significance level)

# Significance of Pearson's coeff
cor.test(~ price + rooms, data = dataset.02) 
# p-value = 0.01521 (less than 5%, so it is statistically significant)
# cor = 0.7361 (strong positive corellation)

# Shapiro test p-value = 0.5969
# p-value slightly higher than 0.05, so we can keep null hypotesis,
# but more observations would be better
shapiro.test((resid(model.2a))) 

# Residuals histogram - histogram suggests that the residuals are not
# normally distributed (right skewed) >> there are some outliers
resid(model.2a)
hist(resid(model.2a), col = 'palegreen4')

# Constant variance 
# Points are not symmetrically distributed, there are potential outliers
plot(model.2a, 1, pch = 20) 

# Normality >> OK in our case - symmetrical
plot(model.2a, 2, pch = 20) 

# Influential points >> obs. #7 highly influences the regression line
plot(model.2a, 5, pch = 20) 




# Divide data set into 2: normal (w/o #7) + outlier (#7)
dataset.02A <- dataset.02[-7,]
dataset.02B <- dataset.02[7,]

dataset.02A <- dataset.02A[,c(2,1)] # change column order, easier to plot later
dataset.02B <- dataset.02B[,c(2,1)]

# Create empty plot, based on initial data (>> extended axis to reflect outlier))
plot(data = dataset.02,
     price  ~ rooms,
     type = 'n',
     xlab = 'Number of rooms',
     ylab = 'Price (k PLN)') +
  
  legend('topleft',
         legend = c('Model 2B (w/o outlier)','Model 2A (all observations)'),
         lwd = 3,
         lty = c('solid','dotted'),
         col = c('darkgreen','cornsilk3'))

# Add data points:
points(dataset.02A, pch = 20, cex = 1.5) +
  points(dataset.02B, pch = 20, col = 'palegreen4', cex = 1.5)

# Linear model for data w/o outlier:
model.2b = lm(price ~ rooms, data = dataset.02A)

abline(model.2a, col = 'cornsilk3', lwd = 2, lty ='dotted') +
  abline(model.2b, col = 'darkgreen', lwd = 2)


# Summary
summary(model.2b)
# Adjusted R-squared: 0.7425 (vs 0.48 in Model 2A) >> improvement
# p-value: 0.001741 (vs 0.01521 in Model 2A) >> significant improvement

# Significance of Pearson's coeff
cor.test(~ price + rooms, data = dataset.02A) 
# p = 0.001741 (vs 0.015 in Model 2A)
# cor = 0.8802 (vs 0.7361 in Model 2A)

# Shapiro test p-value = 0.9398
# p-value close to 1 >> in model 2B price strongly depends on the number of rooms
shapiro.test((resid(model.2b))) 

# Residuals histogram - residuals are normally distributed
resid(model.2b)
hist(resid(model.2b), col = 'palegreen4')

# Constant variance 
# Points are rather symmetrically distributed, potential outliers: #5, #6
plot(model.2b, 1, pch = 20) 

# Normality
# Rather OK in our case - rather symmetrical, potential outliers: #5, #6
plot(model.2b, 2, pch = 20) 

# Influential points: #6 another potential outlier
plot(model.2b, 5, pch = 20) 



# Predict price of 2-room apt:
tworooms <- data.frame(rooms=c(2))

# All data (model.2a) >> 240.6 k PLN
# Equation: 73.1 * 2 + 94.4
# Predict function:
predict(model.2a,tworooms)

# w/o outlier #7 >> 175.8 k PLN
# Equation: 116.30 * 2 - 56.80
# Predict function:
predict(model.2b,tworooms)


# Predict price of #7 observation (actual data: 6 rooms / 425k PLN):
sixrooms <- data.frame(rooms=c(6))

# Model 1A >> price predicted: 533 k PLN
predict(model.2a,sixrooms)
# Model 1B >> price predicted: 641 k PLN
predict(model.2b,sixrooms)
