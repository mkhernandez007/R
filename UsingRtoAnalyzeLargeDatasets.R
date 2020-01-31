#
## Use R to analyze the Birth dataset. 
## The Birth dataset is in the Nutshell library. 
##  .	SEX and APGAR5 (SEX and Apgar score) 
##  .	DPLURAL (single or multiple birth) 
##  .	WTGAIN (weight gain of mother) 
##  .	ESTGEST (estimated gestation in weeks) 
##  .	DOB_MM, DOB_WK (month and day of week of birth) 
##  .	BWT (birth weight) 
##  .	DMETH_REC (method of delivery)
#
install.packages("nutshell")
library(nutshell)
data(births2006.smpl)

# First, list the data for the first 5 births. 
head(births2006.smpl)

# Next, show a bar chart of the frequencies of births according to the day of the week of the birth.
births.dayofweek = table(births2006.smpl$DOB_WK) #Goal of this variable is to speed up the calculations
barplot(births.dayofweek, ylab="frequency", xlab="Day of week", col = "darkred", main= "Number of births in 2006 per day of the week")

# Obtain frequencies for two-way classifications of birth according to the day of the week and the method of delivery.
births.methodsVdaysofweek = table(births2006.smpl$DOB_WK,births2006.smpl$DMETH_REC) 
head(births.methodsVdaysofweek,7)
barplot(births.methodsVdaysofweek[,-2], col=heat.colors(length(rownames(births.methodsVdaysofweek))), width=2, beside=TRUE, main = "bar plot of births per method per day of the week")
legend ("topleft", fill=heat.colors(length(rownames(births.methodsVdaysofweek))),legend=rownames(births.methodsVdaysofweek))

# Use lattice (trellis) graphs (R package lattice) to condition density histograms on the values of a third variable. 
library(lattice)

# The variable for multiple births and the method of delivery are conditioning variables. 
# Separate the histogram of birth weight according to these variable.
histogram(~DBWT|DPLURAL,data=births2006.smpl,layout=c(1,5),col="black", xlab = "birth weight", main = "trellis plot of birth weight vs birth number")
histogram(~DBWT|DMETH_REC,data=births2006.smpl,layout=c(1,3),col="black", xlab = "birth weight", main = "trellis plot of birth weight vs birth method")

# Do a box plot of birth weight against Apgar score and box plots of birth weight by day of week of delivery. 
boxplot(DBWT~APGAR5,data=births2006.smpl,ylab="birth weight",xlab="AGPAR5", main="Boxplot of birthweight per Apgar score")
boxplot(DBWT~DOB_WK,data=births2006.smpl,ylab="birth weight",xlab="Day of Week", main="Boxplot of birthweight per day of week")

# Calculate the average birth weight as a function of multiple births for males and females separately. 
# Use the "tapply" function, and for missing values use the "option nz.rm=TRUE." 
listed = list(births2006.smpl$DPLURAL,births2006.smpl$SEX)
tapplication=tapply(births2006.smpl$DBWT,listed,mean,na.rm=TRUE)
barplot(tapplication,ylab="DBWT", beside=TRUE, legend=TRUE)

### References
### CRAN (n.d.). Using lattice's historgram (). Retrieved from https://cran.r-project.org/web/packages/tigerstats/vignettes/histogram.html
### R (n.d.). Add legends to plots. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/legend.html
### R (n.d.). Apply a function over a ragged array. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/base/html/tapply.html
### R (n.d.). Bar plots. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/barplot.html
### R (n.d.). List-Generic and dotted pairs. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/base/html/list.html
### R (n.d.). Produce box-and-wisker plot(s) of a given (grouped) values.  Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/boxplot.html
### R (n.d.). Return the first or last part of an object. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/utils/html/head.html
