# Data analysis - 01


##########
# EX03
#########
# In the emissions dataset from the package UsingR (CO2 emissions vs. GDP level,
# 26 countries) there is at least one outlier. Draw a correlation diagram
# for variables GDP and CO2. Based on it, determine the outlier. 
# Accepting variable CO2 behind the dependent variable, find the regression
# line with an outlier like also without it.
# How have the results changed? Add both lines to the graph.

# Install & load packages
install.packages("UsingR")
library(UsingR)
library(dplyr)
library(tibble)

# Assign data to the data set
dataset.03 <- emissions
dataset.03

#Add country codes (for clear labeling on chart):
code <- c('USA',
          'JPN',
          'DEU',
          'FRA',
          'GBR',
          'ITA',
          'RUS',
          'CAN',
          'ESP',
          'AUS',
          'NLD',
          'POL',
          'BEL',
          'SWE',
          'AUT',
          'CHE',
          'PRT',
          'GRC',
          'UKR',
          'DNK',
          'NOR',
          'ROU',
          'CZE',
          'FIN',
          'HUN',
          'IRL')

# Dataset with country codes:
dataset.03.c <- add_column(dataset.03, code, .before = 1) 

# Create line model:
model.3a = lm(CO2 ~ GDP, data = dataset.03)

# Scatter plot with regression line:
plot(data = dataset.03,
     CO2 ~ GDP,
     type = 'n', #we will plot country labels instead of data points
     pch = 20, cex = 1.5, 
     xlab = 'GDP', ylab = 'CO2') +
  text(CO2 ~ GDP, data = dataset.03.c, labels = code, cex=0.9, font=2)

abline(model.3a, col = 'darkgreen', lwd = 2) 

# Outlier based on scatter plot: JAPAN (relatively far from the regression line,
# (Russia/USA/Germany - also far away, but they are all above regression line >>
# if we remove one of those countries, new model can have worse parameters.
# With JAPAN removal we expect regression line slope go more upwards.)

# Summary
summary(model.3a)
# Adjusted R-squared: 0.8988 (CO2 emission depends in 90% on country's GDP)
# (we take into consideration adjusted R-squared, since we will be removing data
# point from the model. With more observation points, R-squared is increasing anyway.)
# p-value: 1.197e-13 (with significance level = 0.05 >> ok)

# Residuals histogram >> symmetrical
hist(resid(model.3a), col = 'palegreen4')

# Constant variance >> not symmetrical, outliers: Russia***, Japan**, USA*
plot(model.3a, 1, pch = 20) 

# Normality >> not symmetrical, outliers: Russia***, Japan**, USA*
plot(model.3a, 2, pch = 20) 

# Influential points, outliers: USA***, Japan**, Russia*
plot(model.3a, 5, pch = 20) 



# Remove JAPAN from the model
# Create 2 separate data sets: USA (dataset_03A) + rest of the world
countries.remove <- c('Japan')
dataset.03JAP.c <- dataset.03.c[(row.names(dataset.03.c) %in% countries.remove),] # Japan only

dataset.03B <- dataset.03[!(row.names(dataset.03) %in% countries.remove),] # rest of the world
dataset.03B.c <- dataset.03.c[!(row.names(dataset.03.c) %in% countries.remove),]

# Create new model (w/o Japan)
model.3b = lm(CO2 ~ GDP, data = dataset.03B) 

# Create scatter plot + legend
plot(data = dataset.03,
     CO2 ~ GDP,
     type = 'n', #we will plot country labels instead of data points
     pch = 20, cex = 1.5, 
     xlab = 'GDP', ylab = 'CO2') +
  text(CO2 ~ GDP, data = dataset.03JAP.c, labels = code, cex=0.9, font=2, col = 'palegreen4') +
  text(CO2 ~ GDP, data = dataset.03B.c, labels = code, cex=0.9, font=2) +
  
  legend('topleft',
         legend = c('Model w/o Japan','All observations'),
         lwd = 3,
         lty = c('solid','dotted'),
         col = c('darkgreen','cornsilk4'))

# Add regression lines:
abline(model.3a, col = 'cornsilk4', lwd = 2, lty = 'dotted') +
  abline(model.3b, col = 'darkgreen', lwd = 2) # Russia will be new outlier?


# Summary
summary(model.3b)
# Adjusted R-squared: 0.9299 (vs 0.9028 in Model 3A) >> improvement even with less observations
# p-value: 5.495e-15 (vs 1.197e-13 in Model 3A) >> improvement

# Residuals histogram >> less symmetrical than in 3A, a little bit right skewed
hist(resid(model.3b), col = 'palegreen4')

# Constant variance >> not symmetrical, outliers: USA***, Russia**
plot(model.3b, 1, pch = 20) 

# Normality >> not symmetrical, outliers: USA***, Russia**
plot(model.3b, 2, pch = 20) 

# Influential points >> outliers: USA***, Russia**
plot(model.3b, 5, pch = 20) 


# With removing Japan from the model, general parameters (R-squared and p-value) 
# have improved. From the other side, now the impact from the other outliers spotted
# earlier (USA, Russia and Germany) is more visible on constant variance, normality
# and influential points plots. Supposedly, only by removing the whole group (USA,
# Russia and Germany) we can keep good model parameters.


# One more model: this time w/o USA, Russia and Germany. We keep Japan.
countries.remove <- c('UnitedStates','Russia','Germany')

dataset.03URG.c <- dataset.03.c[(row.names(dataset.03.c) %in% countries.remove),]

dataset.03C <- dataset.03[!(row.names(dataset.03) %in% countries.remove),]
dataset.03C.c <- dataset.03.c[!(row.names(dataset.03.c) %in% countries.remove),]

# Create new model (w/o USA, Russia and Germany)
model.3c = lm(CO2 ~ GDP, data = dataset.03C) 

# Create scatter plot + legend
plot(data = dataset.03,
     CO2 ~ GDP,
     type = 'n',
     pch = 20, cex = 1.5, 
     xlab = 'GDP', ylab = 'CO2') +
  
  text(CO2 ~ GDP, data = dataset.03URG.c, labels = code, cex=0.9, font=2, col = 'darkgreen') +
  text(CO2 ~ GDP, data = dataset.03C.c[-1,], labels = code, cex=0.9, font=2) +
  text(CO2 ~ GDP, data = dataset.03JAP.c, labels = code, cex=0.9, font=2, col = 'skyblue4') +
  
  legend('topleft',
         legend = c('Model w/o USA, Russia and Germany','Model w/o Japan','All observations'),
         lwd = 3,
         lty = c('solid','dashed','dotted'),
         col = c('darkgreen','skyblue4','cornsilk4'))

# Add regression lines:
abline(model.3c, col = 'darkgreen', lwd = 2) +
  abline(model.3b, col = 'skyblue4', lwd = 2, lty = 'dashed') +
  abline(model.3a, col = 'cornsilk4', lwd = 2, lty = 'dotted')



# Zoom in for new data set (w/o USA, Russia and Germany):
plot(data = dataset.03C,
     CO2 ~ GDP,
     type = 'n',
     pch = 20, cex = 1.5, 
     xlab = 'GDP', ylab = 'CO2') +
  text(CO2 ~ GDP, data = dataset.03C.c, labels = code, cex=0.9, font=2)
abline(model.3c, col = 'darkgreen', lwd = 2) 


# Summary
summary(model.3c)
# Adjusted R-squared: 0.8254 (vs 0.9329 in Model 3B and 0.9028 in Model 3A) >> worse, but still valid
# p-value: 1.265e-0 (vs 5.495e-15 in Model 3B and 1.197e-13 in Model 3A) >> worse, but still valid

# Residuals histogram >> not symmetrical, right skewed
hist(resid(model.3c), col = 'palegreen4')

# Constant variance >> not symmetrical, outliers: Canada***, Ukraine**, Australia*
plot(model.3c, 1, pch = 20) 

# Normality >> not symmetrical, outliers: Canada***, Ukraine**, Australia*
plot(model.3c, 2, pch = 20) 

# Influential points >> outliers: Japan* (but now all points are within Cook's distance)
plot(model.3c, 5, pch = 20)