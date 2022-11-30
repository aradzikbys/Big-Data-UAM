# Data analysis - 01

##########
# EX05
#########
# Match the classic model of population growth (logistic model: y = a / (1 + e ^ (b-x)/c)) with the USPop data
# from the carData package containing information about the US population from 1790 to 2000.
# Draw a fitted regression function. Are the assumptions of the model met?


# Install & load package
install.packages("carData")
library(carData)
?USPop
  
# Assign data to the data set
dataset.05 <- USPop
dataset.05



# Logistic model:
model.5 <- nls(population ~ SSlogis(year, a, b, c), dataset.05)

# Summary (values of parameters and tests of significance)
summary(model.5)
# We have 3 parameters (a = 440.83, b = 1976.63, c = 46.28), all of them are relevant to the model (***).
# 19 degrees of freedom = number of observation (22) - the number of variables (3: a,b,c)

# Logistic model: y = a / (1 + e ^ (b-x)/c)
e <- exp(1) # Euler number
a <- coef(model.5)[1] # Estimate a from model.5
b <- coef(model.5)[2] # Estimate b from model.5
c <- coef(model.5)[3] # Estimate c from model.5

# Create plot
plot(data = dataset.05,
     population ~ year,
     pch = 20, cex = 1.5,
     xlab = 'Year', ylab = 'Population')

# Add logistic model:
curve(a / (1+ e^((b-x)/c)),
      add = TRUE, 
      col = 'darkgreen', lwd = 2)


# Load package nlstools - for non-linear regression analysis
library(nlstools)

# Diagnostic plots
plot(nlsResiduals(model.5))

# Residuals histogram - residuals are not normally distributed
hist(resid(model.5), col = 'palegreen4')

# Normality test (SW test) and test of randomness of residuals
test.nlsResiduals(nlsResiduals(model.5))
# Shapiro-Wilk normality test:
# p-value = 0.01252 (less than 0.05 >> we reject null hypothesis,
# order of our data IS NOT random and model is non-linear.

# Assumptions of model are not met.