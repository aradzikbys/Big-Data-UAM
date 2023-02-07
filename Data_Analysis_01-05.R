# Data analysis - 01 - logistic model

##########
# EX05
#########
# Match the classic model of population growth (logistic model: y = a / (1 + e ^ (b-x)/c)) with the USPop data
# from the carData package containing information about the US population from 1790 to 2000.
# Draw a fitted regression function. Are the assumptions of the model met?

# Clear enviroment
rm(list = ls())

# Install & load package
# install.packages("carData")
library(carData)
library(nlstools)
?USPop
  
# Assign data to the data set
dataset05 <- USPop
head(dataset05)

# Logistic model:
model_5log <- nls(population ~ SSlogis(year, a, b, c), dataset05)

# Summary (values of parameters and tests of significance)
summary(model_5log)
# We have 3 parameters (a = 440.83, b = 1976.63, c = 46.28),
# all of them are relevant to the model (***).
# 19 degrees of freedom = number of observation (22) - the number of variables (3)


# Logistic model: y = a / (1 + e ^ (b-x)/c)
e <- exp(1)           # Euler number
a <- coef(model_5log)[1] # Estimate a from model.5
b <- coef(model_5log)[2] # Estimate b from model.5
c <- coef(model_5log)[3] # Estimate c from model.5


# Create plot
plot(data = dataset05,
     population ~ year,
     pch = 20, cex = 1.5,
     xlab = 'Year', ylab = 'Population')

# Add logistic model:
curve(SSlogis(x,a,b,c), add = TRUE, col = 2, lwd = 2)

# Add logistic model - option with using formula:
# curve(a / (1+ e^((b-x)/c)), add = TRUE, col = 2, lwd = 2)


# With ggplot:
# Create data frame with predictions (based on logistic model)
predictions <- predict(model_5log, newdata = data.frame(year = USPop$year), type = "response")
dataset05_a <- data.frame(year = USPop$year, Pop = USPop$population, Predictions = predictions)

ggplot(data = dataset05, aes(x = year, y = population)) +
  geom_point(aes(x = year, y = population)) +
  geom_line(data = dataset05_a, size = 1, color = 2,
            aes(y = predictions), fullrange = TRUE) +
  xlab('Year') + ylab('Population')


# Residuals are not normally distributed:
hist(resid(model_5log), col = 2)

# Variance is not homogeneous (points are not symmetrically distributed)
# and show positive correlation:
plot(nlsResiduals(model_5log), 1)

# Normality test (SW test) and test of randomness of residuals
test.nlsResiduals(nlsResiduals(model_5log))
# Shapiro-Wilk normality test is used to check normality of residuals.
# Null hypothesis is that residuals are normally distributed. If p-value is
# lower than 0.05 we can reject null hypothesis.
# p-value = 0.01252 <0.05, we reject null hypothesis,
# residuals are not normally distributed.

# The Runs Test is a test for randomness of residuals.
# Low p-value (below 0.05) suggests that the residuals are not randomly distributed.
# p-value = 0.000896 < 0.05, residuals are not normally distributed.

##############
# ANSWER:
##############
# Residuals indicates that assumptions of logistic model are not met (there is
# correlation between residuals and they are not normally distributed).