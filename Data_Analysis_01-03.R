# Data analysis - 01 - linear regression, working with outliers


##########
# EX03
#########
# In the emissions dataset from the package UsingR (CO2 emissions vs. GDP level,
# 26 countries) there is at least one outlier. Draw a correlation diagram
# for variables GDP and CO2. Based on it, determine the outlier. 
# Accepting variable CO2 behind the dependent variable, find the regression
# line with an outlier like also without it.
# How have the results changed? Add both lines to the graph.

# Clear enviroment
rm(list = ls())

# Install & load packages
install.packages("UsingR")
library(UsingR)
library(dplyr)
library(tibble)

# Assign data to the data set
?emissions
dataset03 <- emissions
head(dataset03)

# Add country codes (for clear labeling on chart):
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

# Data set with country codes:
dataset03_c <- add_column(dataset03, code, .before = 1) 

# Create linear model:
model_3a = lm(CO2 ~ GDP, data = dataset03)

# Scatter plot with regression line:
plot(data = dataset03,
     CO2 ~ GDP,
     # we will plot country labels instead of data points
     type = 'n', 
     pch = 20, cex = 1.5, 
     xlab = 'GDP', ylab = 'CO2') +
  text(CO2 ~ GDP, data = dataset03_c, labels = code, cex=0.9, font=2)

abline(model_3a, col = 2, lwd = 2) 


# With ggplot:
ggplot(dataset03_c, aes(x = GDP, y = CO2, label = code)) +
  geom_point(size = 2) +
  
  # Labels above data points:
  geom_text(vjust = -0.5, hjust = 0.5) +
  
  # Regression:
  geom_smooth(method = 'lm', colour = 2) +
  
  # Axis labels:
  labs(x = 'GDP', y = 'CO2 emission')

# Outliers based on scatter plot: RUSSIA, JAPAN (relatively far from the regression
# line. USA - far from all the countries clustered close to center of coordinate
# system. With ggplot it's visible, that USA remains within 95% confidence interval.

# Outlier chosen: JAPAN (with data point removal we expect regression line slope
# go more upwards)

# Summary
summary(model_3a)
# Adjusted R-squared: 0.8988 (CO2 emission depends in 90% on country's GDP)
# (we take into consideration adjusted R-squared, since we will be removing data
# point from the model. With more observation points, R-squared is increasing.)
# p-value: 1.197e-13 (with significance level = 0.05 >> OK)

# Residuals histogram >> symmetrical
hist(resid(model_3a), col = 2)

# Constant variance >> not symmetrical, outliers: Russia***, Japan**, USA*
plot(model_3a, 1, pch = 20) 

# Normality >> not symmetrical, outliers: Russia***, Japan**, USA*
plot(model_3a, 2, pch = 20) 

# Influential points, outliers: USA***, Japan**, Russia*
plot(model_3a, 5, pch = 20) 
# USA is outlier in terms of distance from other countries clustered close to the
# bottom left part of the chart.



###############################
# Remove JAPAN from the model
##############################

# Create 2 separate data sets: Japan (dataset03JAP_c) + rest of the world
countries_remove <- c('Japan')

# Japan only
dataset03JAP_c <- dataset03_c[(row.names(dataset03_c) %in% countries_remove),]

# Rest of the world
dataset03B <- dataset03[!(row.names(dataset03) %in% countries_remove),] 
dataset03B_c <- dataset03_c[!(row.names(dataset03_c) %in% countries_remove),]

# Create new model (w/o Japan)
model_3b = lm(CO2 ~ GDP, data = dataset03B) 

# Create scatter plot + legend
plot(data = dataset03,
     CO2 ~ GDP,
     # we will plot country labels instead of data points
     type = 'n', 
     pch = 20, cex = 1.5, 
     xlab = 'GDP', ylab = 'CO2') +
  
  text(CO2 ~ GDP, data = dataset03JAP_c, labels = code,
       cex=0.9, font=2, col = 'darkgrey') +
  text(CO2 ~ GDP, data = dataset03B_c, labels = code,
       cex=0.9, font=2) +
  
  legend('topleft',
         legend = c('Model w/o Japan','All observations'),
         lwd = 3,
         lty = c('solid','dotted'),
         col = c(2,'darkgrey'))

# Add regression lines:
abline(model_3a, col = 'darkgrey', lwd = 2, lty = 'dotted') +
  abline(model_3b, col = 2, lwd = 2) # Russia will be new outlier?


# With ggplot:
ggplot(dataset03B_c, aes(x = GDP, y = CO2)) + 
  
  geom_point (size = 2) +
  geom_text(data = dataset03B_c, aes(label = code), hjust = 0.5, vjust = -0.5) +
  
  # Outlier:
  geom_point(data = dataset03JAP_c, colour = 'darkgrey', size = 2) +
  geom_text(data = dataset03JAP_c, aes(label = code), colour = 'darkgrey', hjust = 0.5, vjust = -0.5) +
  
  # Regression models:
  geom_smooth(data = dataset03B_c, method = 'lm', se = FALSE,
              aes(x = GDP, y = CO2,
                  colour = 'w/o Japan',
                  linetype = 'w/o Japan')) +

  geom_smooth(data = dataset03, method = 'lm', se = FALSE,
              aes(x = GDP, y = CO2,
                  colour = 'All data points',
                  linetype = 'All data points')) +
  
  # Legend
  scale_color_manual(name = 'Data', values=c('darkgrey',2))+
  scale_linetype_manual(name = 'Data', values=c(3,1)) +
  
  # Labels:
  labs(x = 'GDP', y = 'CO2 emmission')
  

# Summary
summary(model_3b)
# Adjusted R-squared: 0.9299 (vs 0.9028 in Model 3A) >> improvement
# p-value: 5.495e-15 (vs 1.197e-13 in Model 3A) >> improvement

# Residuals histogram >> less symmetrical than in 3A, a little bit right skewed
hist(resid(model_3b), col = 2)

# Constant variance >> not symmetrical, outliers: USA***, Russia**
plot(model_3b, 1, pch = 20) 

# Normality >> not symmetrical, outliers: USA***, Russia**
plot(model_3b, 2, pch = 20) 

# Influential points >> outliers: USA***, Russia**
plot(model_3b, 5, pch = 20) 


##############
# ANSWER:
##############
# With removing Japan from the model, general parameters (R-squared and p-value) 
# have improved. On the other side, w/o Japan impact from the other outliers spotted
# earlier (USA, Russia and Germany) is more visible on constant variance, normality
# and influential points plots. 



########################
# Additional analysis:
#######################

# One more model: this time w/o USA, Russia and Germany. We keep Japan.
countries_remove <- c('UnitedStates','Russia','Germany')

dataset03URG_c <- dataset03_c[(row.names(dataset03_c) %in% countries_remove),]

dataset03C <- dataset03[!(row.names(dataset03) %in% countries_remove),]
dataset03C_c <- dataset03_c[!(row.names(dataset03_c) %in% countries_remove),]

# Create new model (w/o USA, Russia and Germany)
model_3c = lm(CO2 ~ GDP, data = dataset03C) 

# Create scatter plot + legend
plot(data = dataset03,
     CO2 ~ GDP,
     type = 'n',
     pch = 20, cex = 1.5, 
     xlab = 'GDP', ylab = 'CO2') +
  
  text(CO2 ~ GDP, data = dataset03URG_c, labels = code, cex=0.9, font=2, col = 8) +
  text(CO2 ~ GDP, data = dataset03C_c, labels = code, cex=0.9, font=2, col = 1)

# Add regression lines:
abline(model_3c, col = 2, lwd = 2) +
  abline(model_3a, col = 8, lwd = 2, lty = 'dotted')

# Legend:
legend('topleft',
       legend = c('All observations','Model w/o USA, Russia and Germany'),
       lwd = 3,
       lty = c(3,1),
       col = c(8,2))

# Zoom in to see how data set (w/o USA, Russia and Germany) fits to the regression line:
plot(data = dataset03C,
     CO2 ~ GDP,
     xlim = c(0,4000000), ylim = c(0,2250),
     type = 'n',
     pch = 20, cex = 1.5, 
     xlab = 'GDP', ylab = 'CO2') +
  
  text(CO2 ~ GDP, data = dataset03URG_c, labels = code, cex=0.9, font=2, col = 8) +
  text(CO2 ~ GDP, data = dataset03C_c, labels = code, cex=0.9, font=2)

abline(model_3c, col = 2, lwd = 2)  


# With ggplot (assign graph to variable graph03:
graph03 <- ggplot(dataset03C_c, aes(x = GDP, y = CO2)) + 
  
  geom_point (data = dataset03C_c, size = 2) +
  geom_text(data = dataset03C_c, aes(label = code), hjust = 0.5, vjust = -0.5) +
  
  # Outlier:
  geom_point(data = dataset03URG_c, colour = 8, size = 2) +
  geom_text(data = dataset03URG_c, aes(label = code), colour = 8, hjust = 0.5, vjust = -0.5) +
  
  # Regression models:
  geom_smooth(data = dataset03C_c, method = 'lm', se = FALSE, fullrange = TRUE,
              aes(x = GDP, y = CO2,
                  colour = 'w/o USA, Russia, Germany',
                  linetype = 'w/o USA, Russia, Germany')) +
  
  geom_smooth(data = dataset03, method = 'lm', se = FALSE, fullrange = TRUE,
              aes(x = GDP, y = CO2,
                  colour = 'All observations',
                  linetype = 'All observations'))+
  
  # Legend
  scale_color_manual(name = 'Data', values=c(8,2))+
  scale_linetype_manual(name = 'Data', values=c(3,1)) +
  
  # Labels:
  labs(x = 'GDP', y = 'CO2 emission')

graph03

# Zoom in With ggplot > set limits to x and y axis:
graph03 + xlim(NA,4000000) + ylim(NA,2250) 
  

# Summary
summary(model_3c)
# Adjusted R-squared: 0.8254 (vs 0.9028 in Model 3A and 0.9329 in Model 3B)
#   >> worse, but still valid
# p-value: 1.265e-0 (vs 1.197e-13 in Model 3A and 5.495e-15 in Model 3B)
#   >> worse, but still valid

# Residuals histogram >> not symmetrical, right skewed
hist(resid(model_3c), col = 2)

# Constant variance >> not symmetrical, outliers: Canada***, Ukraine**, Australia*
plot(model_3c, 1, pch = 20) 

# Normality >> not symmetrical, outliers: Canada***, Ukraine**, Australia*
plot(model_3c, 2, pch = 20) 

# Influential points >> outliers: Japan(?),
# but now all points are within Cook's distance)
plot(model_3c, 5, pch = 20)