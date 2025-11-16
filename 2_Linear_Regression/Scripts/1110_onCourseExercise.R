set.seed(85)
# simulate x and y
x <-  rnorm(n=200, mean=10, sd=2)
y <-  0.4*x + rnorm(200, 0, 1)  # rnorm(200, 0, 1) is simulating a random noise to the linear relationship between x and y

plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")

# linear regression analysis
m = lm(y~x)  # m is a list consisting of 12 objects
cf = m$coef  # extract specific object coefficients
# coefficients includes intercept and all βs(slope)

predvals = cf[1] + cf[2]*x  # predicted equation: y = cf[1] + cf[2]x
par(mfrow=c(1,2))  # configure layout
# plot(m)  Run this directly to generate 4 diagonize figure for the linear model: Residuals vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage

plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
abline(m)  # draw regression line
segments(x, y, x, predvals)  # draw segment between 2 coordinates
hist(residuals(m), xlab="", las=1)  # from here can see residuals are assumed to be normal distribution

newx = seq(min(x), max(x), length.out=200)  # use seq to generate a series of x values within the x scale, then apply the simulated linear equation
predy = cf[1] + cf[2]*newx
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
lines(newx, predy)

summary(m)

## Important parameters in the model, not just P-value
# report the slope
cov(y,x)/var(x)
# report how much the y changes for a one standard deviation change in x
(cf[2]*(mean(x) + sd(x))) - (cf[2]*mean(x))  # mean(x) can also change to a random x
# the change of y is unrelated to the starting point
# report the coefficient of determination, γ^2
cor(x,y)^2  # the proportion of the variance of y is explained by x
# 2nd way to compute γ^2
y_hat = cf[1] + cf[2]*x
var(y_hat)/var(y)
# 3rd way to compute γ^2

