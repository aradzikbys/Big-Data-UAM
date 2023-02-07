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

# Load libraries
library(UsingR)
library(dplyr)
library(tibble)
library(magrittr)

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

# Outliers based on scatter plot: USA, Russia and Japan. According to Practical
# Statistics for Data Scientists, “an outlier is any value that is very distant
# from the other values in a data set”.
# Although USA falls within 95% confidence interval, its GDP and CO2 emission
# are much higher than median values for whole data set.
# Unlike the mean, median (or trimmed mean, which is calculated after removing 
# extreme values) is not affected by outliers and allows us spot influential
# observations.

# USA values
dataset03['UnitedStates',]

# Median for data set
skimr::skim(emissions) %>% dplyr::select(numeric.p50)


# Outlier chosen: USA.

# Summary
summary(model_3a)
# Adjusted R-squared: 0.8988
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
# Remove USA from the model
##############################

# Create 2 separate data sets: Japan (dataset03JAP_c) + rest of the world
countries_remove <- c('UnitedStates')

# w/o USA:
dataset03_A <- dataset03[!(rownames(dataset03) %in% countries_remove),]

# USA only:
dataset03_USA <- dataset03[(rownames(dataset03) %in% countries_remove),]


# With ggplot:
(g <- ggplot(data = dataset03, aes(x = GDP, y=CO2)) +
    # Data points:
    geom_point(data = dataset03_A, color = 1, size = 2, aes(x = GDP, y=CO2)) +
    # Outlier:
    geom_point(data = dataset03_USA, color = 2, size = 2, aes(x = GDP, y=CO2)) +
    # Point labels:
    geom_text(label = rownames(dataset03), hjust = 0.5, vjust = -0.5) +
    # Linear model w/o outlier:
    geom_smooth(method = 'lm', data = dataset03_A, fullrange = TRUE, se = FALSE,
                aes(x = GDP, y=CO2,
                    color = 'Model w/o outlier',
                    linetype = 'Model w/o outlier')) +
    # Linear model with all data points:
    geom_smooth(method = 'lm', data = dataset03, fullrange = TRUE, se = FALSE,
                aes(x = GDP, y=CO2,
                    color = 'Model with all data points',
                    linetype = 'Model with all data points')) +
    # Lines color & type:
    scale_color_manual(name = 'Model', values=c(2,'darkgrey')) +
    scale_linetype_manual(name = 'Model', values=c(1,2)) +
    # Axis labels:
    labs(x = 'GDP', y = 'CO2 emmission'))

# Set limits on axis w/o removing data points outside the range (USA):
g + coord_cartesian(xlim = c(NA,3200000), ylim = c(NA,2100)) 


# Summary
model_3b <- lm(data = dataset03_A, CO2 ~ GDP)
summary(model_3b)

# Adjusted R-squared: 0.4679 (vs 0.8988 in model_3a)
# p-value: 9.802e-05 (vs 1.197e-13 in model_3a)

# Residuals histogram: not symmetrical, right skewed
hist(resid(model_3b), col = 2)

# Residuals vs fitted: line is closer to the center,
# there are still extreme values (Russia, Germany)
plot(model_3b, 1, pch = 20) 

# Normality: a little bit more symmetrical, higher impact from outliers: Russia, Germany
plot(model_3b, 2, pch = 20) 

# Influential points >> outliers: USA***, Russia**
plot(model_3b, 5, pch = 20) 


##############
# ANSWER:
##############
# With removing USA from the model, general parameters (R-squared and p-value)
# have worsen. Even though USA was far away from other points, it was relatively 
# close to regression line. Now, impact from other outliers (Germany and Russia)
# is more visible.