#
## Use R to analyze the faithful dataset. 
## This is a version of the eruption data from the "Old Faithful" geyser in Yellowstone National Park, Wyoming. 
##  .	X (primary key) 
##  .	eruptions (eruption time [mins]) 
##  .	waiting (wait time for this eruptions [mins]) 
#
fateful = read.csv(file="https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/datasets/faithful.csv", header = TRUE, sep = ",")
head(fateful)

# Produce density histograms of eruption times and of waiting times. 
hist(fateful$eruptions, freq=F, xlab = "eruptions time [mins]",  main = "Histogram of the eruptions time")
hist(fateful$waiting, freq=F, xlab = "eruptions waiting time [mins]",  main = "Histogram of the eruptions waiting time")
summary(fateful$eruptions) # min, 1q, med, mean, 3q, max
sd(fateful$eruptions)      # standard deviation
summary(fateful$waiting)
sd(fateful$waiting)
# Produce a smoothed density histogram from local polynomial regression. 
install.packages("locfit")
library(locfit)
plot(locfit(~lp(fateful$eruptions),data=fateful), xlab = "eruptions time [mins]",  main = "Histogram of the eruptions time")
plot(locfit(~lp(fateful$waiting),data=fateful), xlab = "eruptions waiting time [mins]",  main = "Histogram of the eruptions waiting time")

#
## Is the waiting time to the next eruption dependented on the magnitude (eruption time)?
# Compare local polynomial regression to regular regression. 
lowessRegression = lowess(fateful$waiting, faithful$eruptions, f=2/3)
polynomialRegression = locfit(fateful$eruptions~lp(fateful$waiting))
linearRegression = lm(fateful$eruptions~fateful$waiting)
# Graphing the data
plot(fateful$waiting, fateful$eruptions, main = "Eruption Times", xlab="eruption time [min]", ylab = "Waiting time to next eruption [min]")
lines(lowessRegression, col="red")
abline(linearRegression, col="blue")
lines(polynomialRegression, col="green")
# summary on the regressions
summary(linearRegression)
summary(lowessRegression)
summary(polynomialRegression)
# correlations on the regressions
cor(fateful$eruptions,fateful$waiting)
cor(lowessRegression$x, lowessRegression$y)
# Plotting residuals
plot(residuals(linearRegression), main = "residuals for the linear regression", ylab = "residuals")
plot(residuals(polynomialRegression), main = "residuals for the polynomial regression", ylab="residuals")

### References
### GitHub. (n.d.). Rdatasets. Retrieved from http://vincentarelbundock.github.io/Rdatasets/ 
### Loader, C. (2013). locfitt: Local Regression, Likelihood and Density Estimation. Retrieved from https://cran.r-project.org/web/packages/locfit/locfit.pdf
### R (n.d.). Plot Histograms. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/plothistogram.html
### R (n.d.). Scatter Plot Smoothing. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lowess.html
