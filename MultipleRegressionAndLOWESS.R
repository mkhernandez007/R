NOxData = read.csv(file="https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/robustbase/NOxEmissions.csv", header = TRUE, sep = ",")
head(NOxData)
hist(NOxData$LNOx, freq=F, xlab = "hourly mean of NOx concentrations [ppb]",  main = "Histogram of the hourly mean of NOx concentrations")
hist(NOxData$LNOxEm, freq=F, xlab = "hourly sum of NOx car emissions [ppb]",  main = "Histogram of the hourly sum of NOx car emissions")
hist(NOxData$sqrtWS, freq=F, xlab = "square root of winds [m/s]", main = "Histogram of the square root of winds")

# Single Linear Regressions on LNOxEm
## LNOx
plot(NOxData$LNOxEm, NOxData$LNOx, ylab="hourly mean of NOx concentrations [ppb]", xlab = "hourly sum of NOx car emissions [ppb]", main = "Linear Regression")
abline(lm(NOxData$LNOx~NOxData$LNOxEm), col="red")
summary(lm(NOxData$LNOx~NOxData$LNOxEm))
cor(NOxData$LNOx, NOxData$LNOxEm)

## sqrtWS
plot(NOxData$sqrtWS, NOxData$LNOx, ylab="hourly mean of NOx concentrations [ppb]", xlab = "square root of winds [m/s]", main = "Linear Regression")
abline(lm(NOxData$LNOx~NOxData$sqrtWS), col="red")
summary(lm(NOxData$LNOx~NOxData$sqrtWS))
cor(NOxData$LNOx, NOxData$sqrtWS)

# Multiple Linear Regression on both LNOxEM and sqrtWS variables on LNOx 
RegressionModel = lm(NOxData$LNOx~ NOxData$LNOxEm + NOxData$sqrtWS) 
summary(RegressionModel)      
plot(RegressionModel)

# Pearson's Correlation between independent variables
cor(NOxData$LNOxEm, NOxData$sqrtWS)
# 95% Confidence Intervals on the regression model
confint(RegressionModel, conf.level=0.95)

# LOWESS MODEL
LowessModel = lowess(NOxData$LNOx~ NOxData$LNOxEm + NOxData$sqrtWS, f=2/3)
LowessModel2 = lowess(NOxData$LNOx~ NOxData$LNOxEm + NOxData$sqrtWS, f=0.01)
LowessModel3 = lowess(NOxData$LNOx~ NOxData$LNOxEm + NOxData$sqrtWS, f=1)
plot(LowessModel,type="l",col="blue", main="LOWESS Regression: green is f=1, blue is f=2/3, & red is f=0.01")
lines(LowessModel2, col="red")
lines(LowessModel3, col="green")



### REFERENCES
### Marin, M. (2013). Importing Data and Working with Data in R (R Tutorial 1.4). Retrieved from https://www.youtube.com/watch?v=U4-RnTW5dfw
### Marin, M. (2013). Multiple Linear Regression in R (R Tutorial 5.3). Retrieved from https://www.youtube.com/watch?v=q1RD5ECsSB0
### R (n.d.). Add Connected Line Segments to a Plot. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/lines.html
### R (n.d.). Generic X-Y Plotting. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/plot.html
### R (n.d.). Fitting Linear Models. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html
### R (n.d.). Plot Histograms. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/plothistogram.html
### R (n.d.). Scatter Plot Smoothing. Retreived from https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lowess.html