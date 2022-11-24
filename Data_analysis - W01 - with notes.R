##############################
# Linear regression #
##############################

##############
## Example 0
##############

# A common observation in ecology is that species diversity decreases as you get further
# from the equator. To see whether this pattern could be seen on a small scale, 
# we used data from the Audubon Society's Christmas Bird Count, 
# in which birders try to count all the birds in a 15-mile diameter area during one winter day.
# We looked at the total number of species seen in each area on the Delmarva Peninsula during 
# the 2005 count. Latitude and number of bird species are the two measurement variables;
# location is the hidden nominal variable.
Input <- ("Town                 State  Latitude  Species
  'Bombay Hook'          DE     39.217    128
  'Cape Henlopen'        DE     38.800    137
  'Middletown'           DE     39.467    108
  'Milford'              DE     38.958    118
  'Rehoboth'             DE     38.600    135
  'Seaford-Nanticoke'    DE     38.583     94
  'Wilmington'           DE     39.733    113
  'Crisfield'            MD     38.033    118
  'Denton'               MD     38.900     96
  'Elkton'               MD     39.533     98
  'Lower Kent County'    MD     39.133    121
  'Ocean City'           MD     38.317    152
  'Salisbury'            MD     38.333    108
  'S Dorchester County'  MD     38.367    118
  'Cape Charles'         VA     37.200    157
  'Chincoteague'         VA     37.967    125
  'Wachapreague'         VA     37.667    114")
(data.set <- read.table(textConnection(Input), header = TRUE))

# Scatterplot:
plot(Species ~ Latitude, data = data.set, pch = 20)


# Significance of Pearson's coeff
cor.test(~ Species + Latitude, data = data.set) 

  # cor = -0.462: negative correlation; in line with initial assumption
  # (the further north > the less species)
  #       Pearson's Coeff possible values:
  #       0 - 1: positive correlation, the closer to 1 the stronger the correlation;
  #             (0 - 0.3 weak, 0.3 - 0.5 moderate, >0.5 - strong)  
  #       0: no correlation at all
  #       -1 - 0: negative correlation
  # p - value = 0.06
  #       A p-value less than 0.05 (typically ≤ 0.05) is statistically significant.
  #       0.05 - standard statistical significance (probability of rejecting the null 
  #       hypothesis when it is true). The less the better

# Significancy of Spearman's coeff 
cor.test(~ Species + Latitude, data = data.set, method = 'spearman') 

  # The Spearman correlation coefficient is more general than the Pearson 
  # correlation coefficient, which only measures a linear relationship. 
  # e.g. If our X and Y data had a relationship Y = X^{2} with a Pearson 
  # coefficient close to 0 and a Spearman coefficient close to 1.

  # Pearson works with raw data values of the variables
  # whereas Spearman works with rank-ordered variables.


# (name-of-your-model).lm - create linear model (REGRESSION):
model.lm <- lm(Species ~ Latitude, data = data.set)

summary(model.lm)
  # Estimate Std. / (Intercept) = 585.145: data for x=0,
  #                             This case: number of species on Equator
  # Estimate Std. / Latitude = -12.039: how the data changes?
  #                           This case: with one degree of latitude,
  #                           the number of species decreases by 12
  # IMPORTANT: Pr(>|t|) / Latitude = 0.0613: p-value 
  # IMPORTANT: Multiple R-squared = 0.2143: change in number of species
  #                                 depends in 21% on the change of latitude  


  # CONCLUSION: there should be more data, but we can already calculate egression.
  # The trend is in the expected direction, but it is not quite significant.
  # The equation of the regression line is:
  # number of species = (Estimate Std./Latitude) * latitude + (Estimate Std./(Intercept))
  # number of species = −12.039 * latitude + 585.145

# Regression:
abline(model.lm, lwd = 2, col = 'red')
 

# Residuals
# A residual is a measure of how well a line fits an individual data point?
#     
#     WATCHOUT: The ERROR of an observation is the deviation of the observed value
#     from the TRUE value of a quantity of interest. The RESIDUAL is the difference
#     between the observed value and the ESTIMADED value of the quantity of interest.
resid(model.lm)

# Normality / histogram (OK in our case):
hist(resid(model.lm), col = 'lightyellow')

# Constant variance  (OK in our case) - points are evenly distributed. 
# When you plot the individual error against the predicted value, the variance 
# of the error predicted value should be constant.
plot(model.lm, 1, pch = 20) 

# Normality  (OK in our case)
# Normality test is used to determine whether sample data has been drawn from a normally
# distributed population (within some tolerance).
# Normal: If the line is relatively straight.
plot(model.lm, 2, pch = 20) 

# Shapiro test
# The Shapiro-Wilks test for normality - is data normally distributed>
# The test rejects the hypothesis of normality when the p-value is <= 0.05
# p-value = 0.899 >> OK in our case
shapiro.test((resid(model.lm))) 

# Influential points (outliers that greatly affects the slope of the regression line)
# Any points crossing "1" line
plot(model.lm, 5, pch = 20) 


# Box Cox Transformation
# A Box Cox transformation is a transformation of non-normal dependent variables into a normal shape.
# 1 is inside confidence level (above 95 confidence interval) - OK in our case
MASS::boxcox(model.lm, lambda = seq(-4, 3))


##############
## Example 1 
##############

# Males of the magnificent frigatebird (Fregata magnificens) have a large red throat pouch. 
# They visually display this pouch and use it to make a drumming sound when seeking mates.
# Madsen et al. (2004) wanted to know whether females, who presumably choose mates based 
# on their pouch size, could use the pitch of the drumming sound as an indicator of pouch size. 
# The authors estimated the volume of the pouch and the fundamental frequency 
# of the drumming sound in 18 males.
Input <- ('Volume  Pitch 
  1760    529 
  2040    566
  2440    473
  2550    461
  2730    465
  2740    532
  3010    484
  3080    527
  3370    488
  3740    485
  4910    478
  5090    434
  5090    468
  5380    449
  5850    425
  6730    389
  6990    421
  7960    416')
(data.set <- read.table(textConnection(Input), header = TRUE))

cor.test( ~ Pitch + Volume,
          data = data.set,
          method = 'spearman')
# p-value = 0.0002302 >> 

# There are two measurement variables, pouch size and pitch.
# The authors analyzed the data using Spearman rank correlation, which converts 
# the measurement variables to ranks, and the relationship between the variables 
# is significant (p.value = 0.0002302). The authors do not explain why they used 
# Spearman rank correlation; if they had used regular correlation, they would
# have obtained p.value = 3.056e-05.
cor.test( ~ Pitch + Volume, data = data.set)

# Don't put a regression line on the graph, however; 
# it would be misleading to put a linear regression line on a graph 
# when you've analyzed it with rank correlation.
plot(Pitch ~ Volume, data = data.set, pch = 20)


##############
## Example 2 
##############

?cars

# remove duplicate values > now we have 19 data points instead of 50
data.set <- cars[!duplicated(cars$speed),]
dim(data.set)


# Scatterplot
plot(data.set, type = 'n')
 

# Training - all observation besides the last one (outlier) 
training <- data.set[-19,]
# Outlier:
test <- cars[19,]

# Reflect points on the plot: training + test (red)
points(training, pch = 20, cex = 2) 
points(test, pch = 20, col = 'red', cex = 2)

# Model 1 (linear model)
model1 <- lm(dist ~ speed, data = training) 
# Plot
abline(model1, col = 'black', lwd = 2) 

# Summary
summary(model1)
#   Residuals - 5 summary points should be symmetrically distributed.
#   (ok w/o outlier (from -13.6 to 14.1) - our model takes "training' data)

#   Estimate / speed = 3.113: for every 1 mph increase in the speed of a car,
#   the required distance to stop goes up by 3.113 feet.

#  Std. Error / speed = 0.3076: 
#   required distance for a car to stop can vary by 0.3076 feet.

#  t value = 10.12: how many standard deviations our coeff. estimate is far 
#   away from 0 (We want it to be far away from zero)

#   Pr(>t) (or p-value) = 2.33e-08: probability of observing any value equal or
#   larger than t. Typically, a p-value of 5% or less is a good cut-off point

#   ‘signif. Codes’ - *** represent a highly significant p-value

#   Residual standard error: 7.347 = quality of a linear regression fit.
#   average amount that the response (dist) will deviate from the true regression line

#   Multiple R-squared = 0.8649: 86,5% of the variance found in the response variable
#   (distance) can be explained by the predictor variable (speed)

#   F-statistic = 102.3: indicator of whether there is a relationship between
#   our predictor and the response variables. The further the F-statistic is
#   from 1 the better it is. 

#   Full interpretation:
#   https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R



# MSE (mean squared error) - measures the average of the squares of the errors:
# the average squared difference between the estimated values and the actual value.
(MSE.model1 <- mean(resid(model1)^2))

# Cubic model (3rd degree polynomial):
model2 <- lm(dist ~ poly(speed, 3, raw = TRUE), data = training)
lines(training$speed, predict(model2), col = 'red', lwd = 2) 
summary(model2)
(MSE.model2 <- mean(resid(model2)^2)) # MSE model2

# 10th degree polynomial:
model3 <- lm(dist ~ poly(speed, 10, raw = TRUE), data = training)
lines(training$speed, predict(model3), col = 'blue', lwd = 2) 
summary(model3)
(MSE.model3 <- mean(resid(model3)^2)) # MSE model3

# 100th polynomial:
model4 <- lm(dist ~ poly(speed, 100, raw = TRUE), data = training)
lines(training$speed, predict(model4), col = 'green', lwd = 2) 
summary(model4)
(MSE.model4 <- mean(resid(model4)^2)) # MSE model4


# Add legen to the plot:
legend('topleft', legend = c('Model 1', 'Model 2', 'Model 3', 'Model 4'), lwd = 2,
       col = c('black', 'red', 'blue', 'green'))

# Predict outlier values with models:
predict(model1,test) # 23.87982 
predict(model2,test) # 21.14853 
predict(model3,test) # 21.33368 
predict(model4,test) # 26.00006  


# Compare models:

# Akaike criterion picks model3 (lowest AIC value)
AIC(model1, model2, model3, model4) 

# Bayesian criterion picks model2 (lowest BIC value) > Bayesian prefers simpler models
BIC(model1, model2, model3, model4)

# ANOVA picks model2 (little dot at the end of the line)
# More details on Anova test:
# https://bookdown.org/ndphillips/YaRrr/comparing-regression-models-with-anova.html
anova(model1, model2, model3) 



##############
## Example 3 
##############

?mtcars
View(mtcars)
# This data set contains 11 measurements on each of 32 model cars that were available in 1973–1974.
# Some characteristics could be considered design features that might not be readily 
# observed unless one were to open the hood, or perhaps dismantle an assembly 
# and count the teeth on the various gears. 
# These design features include the number of cylinders, displacement, rear axle ratio, 
# 'V' arrange - ment of cylinders, number of forward gears, and the number of carburetors.
# Other features in this list are empirical and readily experienced by a driver. 
# This list includes the miles per gallon, horsepower, weight, and quarter mile time.


# NULL HYPOTESIS: MPG (range) depends on number of cylinders (cyl), weight (wt),
# transmission (am) and number of carburetors (carb)
model.lm <- lm(mpg ~ cyl + wt + am + carb, data = mtcars)

summary(model.lm)
# In this table of parameter estimates, we see the number of cyl. (cyl) and weight (wt)
# provide a great amount of explanatory value in describing mpg values (* = 0.05). 
# The number of carburetors (carb) has limited value (. = 0.1), and the 
# transmission type (am: automatic or manual) makes a minimal contribution.

# The estimated coefficients for cylinders and weight are negative because, 
# intuitively, larger engines and heavier cars will get fewer miles per gallon.

plot(model.lm, 1, pch = 20) # Constant variance assumption
plot(model.lm, 2, pch = 20) # Normality assumption
shapiro.test(resid(model.lm)) # Shapiro-Wilk test for normality
plot(model.lm, 5, pch = 20) # Outliers

# We remove non-significant variable (am) by hand:
model1.lm <- lm(mpg ~ cyl + wt + carb, data = mtcars) 
summary(model1.lm) # carb now is non-significant

# We remove another non-significant variable (carb) by hand:
# model1.lm = update(model.lm, . ~ . -am)
model2.lm <- update(model1.lm, . ~ . -carb)
summary(model2.lm)
# Now all parameters are significant, R^2 = 81.85%

AIC(model.lm, model1.lm, model2.lm)     # Very similar models
BIC(model.lm, model1.lm, model2.lm)     # BIC picks model2.lm
anova(model2.lm, model1.lm, model.lm)   # Models are not significantly different




##############
## Example 4 
##############
# I extracted some data from the Maryland Biological Stream Survey. The dependent variable
# is the number of longnose dace (Rhinichthys cataractae) per 75-meter section of stream.
# The independent variables are the area (in acres) drained by the stream; 
# the dissolved oxygen (in mg/liter); the maximum depth (in cm) of the 75-meter segment 
# of stream; nitrate concentration (mg/liter); sulfate concentration (mg/liter);
# and the water temperature on the sampling date (in degrees C). One biological goal might be
# to measure the physical and chemical characteristics of a stream and be able to predict
# the abundance of longnose dace; another goal might be to generate hypotheses about
# the causes of variation in longnose dace abundance.

Input <- ('Stream                   Longnose  Acerage  DO2   Maxdepth  NO3   SO4     Temp 
  BASIN_RUN                  13         2528    9.6  80        2.28  16.75   15.3
  BEAR_BR                    12         3333    8.5  83        5.34   7.74   19.4
  BEAR_CR                    54        19611    8.3  96        0.99  10.92   19.5
  BEAVER_DAM_CR              19         3570    9.2  56        5.44  16.53   17
  BEAVER_RUN                 37         1722    8.1  43        5.66   5.91   19.3
  BENNETT_CR                  2          583    9.2  51        2.26   8.81   12.9
  BIG_BR                     72         4790    9.4  91        4.1    5.65   16.7
  BIG_ELK_CR                164        35971   10.2  81        3.2   17.53   13.8
  BIG_PIPE_CR                18        25440    7.5  120       3.53   8.2    13.7
  BLUE_LICK_RUN               1         2217    8.5  46        1.2   10.85   14.3
  BROAD_RUN                  53         1971   11.9  56        3.25  11.12   22.2
  BUFFALO_RUN                16        12620    8.3  37        0.61  18.87   16.8
  BUSH_CR                    32        19046    8.3  120       2.93  11.31   18
  CABIN_JOHN_CR              21         8612    8.2  103       1.57  16.09   15
  CARROLL_BR                 23         3896   10.4  105       2.77  12.79   18.4
  COLLIER_RUN                18         6298    8.6  42        0.26  17.63   18.2
  CONOWINGO_CR              112        27350    8.5  65        6.95  14.94   24.1
  DEAD_RUN                   25         4145    8.7  51        0.34  44.93   23
  DEEP_RUN                    5         1175    7.7  57        1.3   21.68   21.8
  DEER_CR                    26         8297    9.9  60        5.26  6.36    19.1
  DORSEY_RUN                  8         7814    6.8  160       0.44  20.24   22.6
  FALLS_RUN                  15         1745    9.4  48        2.19  10.27   14.3
  FISHING_CR                 11         5046    7.6  109       0.73   7.1    19
  FLINTSTONE_CR              11        18943    9.2  50        0.25  14.21   18.5
  GREAT_SENECA_CR            87         8624    8.6  78        3.37   7.51   21.3
  GREENE_BR                  33         2225    9.1  41        2.3    9.72   20.5
  GUNPOWDER_FALLS            22        12659    9.7  65        3.3    5.98   18
  HAINES_BR                  98         1967    8.6  50        7.71  26.44   16.8
  HAWLINGS_R                  1         1172    8.3  73        2.62   4.64   20.5
  HAY_MEADOW_BR               5          639    9.5  26        3.53   4.46   20.1
  HERRINGTON_RUN              1         7056    6.4  60        0.25   9.82   24.5
  HOLLANDS_BR                38         1934   10.5  85        2.34  11.44   12
  ISRAEL_CR                  30         6260    9.5  133       2.41  13.77   21
  LIBERTY_RES                12          424    8.3  62        3.49   5.82   20.2
  LITTLE_ANTIETAM_CR         24         3488    9.3  44        2.11  13.37   24
  LITTLE_BEAR_CR              6         3330    9.1  67        0.81   8.16   14.9
  LITTLE_CONOCOCHEAGUE_CR    15         2227    6.8  54        0.33   7.6    24
  LITTLE_DEER_CR             38         8115    9.6  110       3.4    9.22   20.5
  LITTLE_FALLS               84         1600   10.2  56        3.54   5.69   19.5
  LITTLE_GUNPOWDER_R          3        15305    9.7  85        2.6    6.96   17.5
  LITTLE_HUNTING_CR          18         7121    9.5  58        0.51   7.41   16
  LITTLE_PAINT_BR            63         5794    9.4  34        1.19  12.27   17.5
  MAINSTEM_PATUXENT_R       239         8636    8.4  150       3.31   5.95   18.1
  MEADOW_BR                 234         4803    8.5  93        5.01  10.98   24.3
  MILL_CR                     6         1097    8.3  53        1.71  15.77   13.1
  MORGAN_RUN                 76         9765    9.3  130       4.38   5.74   16.9
  MUDDY_BR                   25         4266    8.9  68        2.05  12.77   17
  MUDLICK_RUN                 8         1507    7.4  51        0.84  16.3    21
  NORTH_BR                   23         3836    8.3  121       1.32   7.36   18.5
  NORTH_BR_CASSELMAN_R       16        17419    7.4  48        0.29   2.5    18
  NORTHWEST_BR                6         8735    8.2  63        1.56  13.22   20.8
  NORTHWEST_BR_ANACOSTIA_R  100        22550    8.4  107       1.41  14.45   23
  OWENS_CR                   80         9961    8.6  79        1.02   9.07   21.8
  PATAPSCO_R                 28         4706    8.9  61        4.06   9.9    19.7
  PINEY_BR                   48         4011    8.3  52        4.7    5.38   18.9
  PINEY_CR                   18         6949    9.3  100       4.57  17.84   18.6
  PINEY_RUN                  36        11405    9.2  70        2.17  10.17   23.6
  PRETTYBOY_BR               19          904    9.8  39        6.81   9.2    19.2
  RED_RUN                    32         3332    8.4  73        2.09   5.5    17.7
  ROCK_CR                     3          575    6.8  33        2.47   7.61   18
  SAVAGE_R                  106        29708    7.7  73        0.63  12.28   21.4
  SECOND_MINE_BR             62         2511   10.2  60        4.17  10.75   17.7
  SENECA_CR                  23        18422    9.9  45        1.58   8.37   20.1
  SOUTH_BR_CASSELMAN_R        2         6311    7.6  46        0.64  21.16   18.5
  SOUTH_BR_PATAPSCO          26         1450    7.9  60        2.96   8.84   18.6
  SOUTH_FORK_LINGANORE_CR    20         4106   10.0  96        2.62   5.45   15.4
  TUSCARORA_CR               38        10274    9.3  90        5.45  24.76   15
  WATTS_BR                   19          510    6.7  82        5.25  14.19   26.5')
(data.set <- read.table(textConnection(Input), header = TRUE))

# Create a new data frame with only the numeric variables.
# This is required for corr.test
data.set.numeric <- dplyr::select(data.set, Longnose, Acerage, DO2, Maxdepth, NO3, SO4, Temp)
data.set.numeric

# For corr.test() function
library(psych) 

# Correlation matrix - table which displays the correlation coefficients for different variables
corr.test(data.set.numeric)

# More user friendly correlation between parameters > corrplot
# CORRPLOT explained:
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
install.packages("corrplot")
library(corrplot)

# first create correlation matrix:
M <- cor(data.set)
# create plot:
corrplot(M,order = 'AOE', type = 'upper', diag = FALSE)
corrplot.mixed(M, order = 'AOE')


# Scatterplots between each pairwise combination of variables
pairs(~ Longnose + Acerage + DO2 + Maxdepth + NO3 + SO4 + Temp, data = data.set, pch = 20)

# For chart.Correlation() function / more detailed
library(PerformanceAnalytics)
chart.Correlation(data.set.numeric, method = 'pearson', histogram = TRUE, pch = 20)

# We create model > NULL HYPOTESIS: number of fish depends on all factors (Longnose ~ .)
# All below Pr(>|t| is valid, the less the stronger correlation
# p-value: 0.0005905 >> model is bettern than mean
model.lm <- lm(Longnose ~ ., data = data.set.numeric)
summary(model.lm)

# Removing non relevant factors, step by step (stepwise multiple regression)
model.lm.final <- step(model.lm)

# In final model  p-value: 9.717e-05 (comparing to 0.0005905 in initial one)
# >> significant improvement
summary(model.lm.final)


# The results of a stepwise multiple regression, is that acreage, nitrate and 
# maximum depth contribute to the multiple regression equation. 
# The adjusted R^2 of the model including these three terms is 0.2461,
# which isn't very high.

plot(model.lm.final, 1, pch = 20)

# Log transform: 1 is outside confidence level > we will transform model
MASS::boxcox(model.lm.final, lambda = seq(-1, 1)) 

# Transformation:
model.lm.log <- update(model.lm.final, log(.) ~ .)

# All parameters are valid
summary(model.lm.log)

plot(model.lm.log, 1, pch = 20)
plot(model.lm.log, 2, pch = 20)
shapiro.test(resid(model.lm.log))
plot(model.lm.log, 5, pch = 20)




##############################
# Non-linear regresion #
##############################

##############
## Example 1 
##############

# Enzyme kinetics, non-linear behavior
# Load data from the file + create plot
load('L.minor.rda')
L.minor
plot(L.minor, ylim = c(10, 140), pch = 20) 

# Nonlinear model
model.nlm <- nls(rate ~ SSmicmen(conc, a, b), L.minor)

# Add fitted polyline do the plot (not a curve)
lines(L.minor$conc, fitted(model.nlm)) 

# Add true model to the plot (smooth curve)
curve(coef(model.nlm)[1] * x / (coef(model.nlm)[2] + x), 
      add = TRUE, 
      col = 'red', 
      lwd = 2) 

# Legend:
legend('bottomright', lwd = 2, col = c('black', 'red'), 
       legend = c('Polyline', 'Curve'))


# Values of parameters and tests of significance
summary(model.nlm) 


# y = ax / (b + x)
# 1/y = (b + x) / ax
# 1/y = b/(ax) + 1/a
# 1/y = (b/a) * (1/x) + 1/a
# y' = a' * x' + b'
# y = 1/y; a' = b/a; x' = 1/x; b' = 1/a


model.lm <- lm(I(1/rate) ~ I(1/conc), data = L.minor)
summary(model.lm)
# a = 1/b' # 1 / coef(model.lm)[1]
# b = a' * a = a'/b' # coef(model.lm)[2] / coef(model.lm)[1]
a <- 1 / coef(model.lm)[1]
b <- coef(model.lm)[2] / coef(model.lm)[1]

# Add true model to the plot (smooth curve)
curve(a * x / (b + x), 
      add = TRUE, 
      col = 'green', 
      lwd = 2) 

# Prediction
predict(model.nlm) 

# MSE non-linear model
mean((L.minor$rate - predict(model.nlm))^2)
# MSE for linearized model
mean((L.minor$rate - a * L.minor$conc / (b + L.minor$conc))^2) 

# Akaike criterion
AIC(model.nlm) 

# For nlsResiduals() & test.nlsResiduals() functions
library(nlstools) 

# Contour plot for parameters
plot(nlsContourRSS(model.nlm), nlev = 10) 

# Diagnostic plots
plot(nlsResiduals(model.nlm))

# Normality test (SW test) and test of randomness of residuals
test.nlsResiduals(nlsResiduals(model.nlm)) 


##############
## Example 2 
##############

# Ashton et al. (2007) measured the carapace length (in mm) of 18 female gopher 
# tortoises (Gopherus polyphemus) in Okeeheelee County Park, Florida, 
# and X-rayed them to count the number of eggs in each.
Input <- ('Length  Clutch
  284      3  
  290      2  
  290      7
  290      7  
  298     11  
  299     12
  302     10  
  306      8  
  306      8
  309      9  
  310     10  
  311     13
  317      7  
  317      9  
  320      6
  323     13  
  334      2  
  334      8')

(data.set <- read.table(textConnection(Input), header = TRUE))

# Linear model
model1 <- lm(Clutch ~ Length, data = data.set) 
# Quadratic model
model2 <- lm(Clutch ~ Length + I(Length^2), data = data.set) 
# Cubic model
model3 <- lm(Clutch ~ Length + I(Length^2) + I(Length^3), data = data.set)

# Check with Anova method
# Quadratic model is the best (** next to 2)
anova(model1, model2, model3) 

# Compare all models at once with different methods:
library(rcompanion)

compareLM(model1, model2, model3)
summary(model2)

# Create scatterplot:
plot(data.set, pch = 20, xlim = c(280, 380))

# Regression line (linear model)
abline(model1, lwd = 2)

# Quadratic model:
curve(coef(model2)[1] + coef(model2)[2] * x + coef(model2)[3] * x^2, 
      add = TRUE, 
      col = 'red', 
      lwd = 2)

# Cubic model:
curve(coef(model3)[1] + coef(model3)[2] * x + coef(model3)[3] * x^2 + coef(model3)[4] * x^3, 
      add = TRUE, 
      col = 'blue', 
      lwd = 2)

# Mean-based:
abline(h = mean(data.set$Clutch), col = 'green', lwd = 2)

# Legend
legend('bottomright', legend = c('Linear model', 'Quadratic model', 'Cubic model', 'Mean'), 
       lwd = 2,
       col = c('black', 'red', 'blue', 'green'))

# The linear regression is not significant (p.value = 0.6308),
# but the quadratic is significant (p.value = 0.01403). 
# The best-fit quadratic equation is Ŷ = −899.9 + 5.857 X − 0.009425 X^2. 
# The first part of the graph is not surprising; it's easy to imagine why bigger  
# tortoises would have more eggs. The decline in egg number above 310 mm carapace 
# length  is the interesting result. It suggests that egg production declines 
# in thesetortoises as they get old and big.

# Ashton, K.G., R.L. Burke, and J.N. Layne. 2007. Geographic variation in body and clutch size of gopher tortoises. Copeia 2007: 355-363.
# Madsen, V., T.J.S. Balsby, T. Dabelsteen, and J.L. Osorno. 2004. Bimodal signaling of a sexually selected trait: gular pouch drumming in the magnificent frigatebird. Condor 106: 156-160.