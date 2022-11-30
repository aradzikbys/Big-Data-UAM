# Data analysis - 01

##########
# EX06
#########
# A skydiver jumps with a parachute from a hot air balloon. Below are his velocities [m/s]
# in successive moments of time [s], starting from 1s. Fit the Michaelis-Menten model to this data.
# Draw a scatter plot with a fitted regression curve.
# What speed the jumper will reach in the 17th second of the flight?

input <- ('t	v
          1	10
          2	16.3
          3	23
          4	27.5
          5	31
          6	35.6
          7	39
          8	41.5
          9	42.9
          10	45
          11	46
          12	45.5
          13	46
          14	49
          15	50')

dataset.06 <- read.table(textConnection(input), header = TRUE)
dataset.06

# Michaelis-Menten:
model.6 <- nls(v ~ SSmicmen(t, a, b), dataset.06)

# Summary (values of parameters and tests of significance)
summary(model.6)
# We have 2 parameters (a = 70.1793, b = 6.0109), both are relevant to the model (***).
# 13 degrees of freedom = number of observation (15) - the number of variables (2: a,b)

# Michaelis-Menten model: y = a*x / (b+x)
a <- coef(model.6)[1] # Estimate a from model.6
b <- coef(model.6)[2] # Estimate b from model.6

# Create plot
plot(data = dataset.06,
     v ~ t,
     pch = 20, cex = 1.5,
     xlim = c(0, 20), ylim = c(0, 55),
     xlab = 'Time [s]', ylab = 'Velocity [m/s]')

# Add Michaelis-Menten curve:
curve((a*x / (b+x)),
      add = TRUE, 
      col = 'darkgreen', lwd = 2)

# Predict speed of the jumper in 17th second of the flight:
seventeenth <- data.frame(t=c(17))
predict(model.6,seventeenth)

#Predicted speed: 51.85 m/s