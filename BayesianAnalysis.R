#
## Locate the data, filter out the data, and pull it into R from the computer (R, n.d.b.)
#

setwd("C:/Users/fj998d/Documents/R/dataSets")
airplaneData=read.csv("022016DC2NYC_1022370032_T_ONTIME.csv", header = T, sep = ",")

#
## The Delayed Airplanes Dataset consists of airplane flights from Washington D.C. into New York City.
## DATE RANGE: February 2016
### ----------------------------------------------------------------------------------------------------------
##  Data Source: http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
##	Dependent:   Departure Delay Indicator, 15 minutes or more (Dep_Del15)
##	Independent: Arrival airports of Newark-EWR, Kennedy-JFK, and LaGuardia-LGA (Origin)
##	Independent: Departure airports of Baltimore-BWI, Dulles-IAD, and Reagan-DCA (Dest)
##	Independent: Carriers (Carrier)
##	Independent: Hours of departure (Dep_Time)
##	Independent: Weather conditions (Weather_Delay)
##	Independent: Monday = 1, Tuesday = 2, ...Sunday = 7 (Day_Of_Week)
### ----------------------------------------------------------------------------------------------------------
##  bayes theory => p(theta|y)= p(theta)*P(y|theta)/(SUM(P(theta)*P(y|theta))) (Cowles, Kass, & O'Hagan, 2009)
### ----------------------------------------------------------------------------------------------------------
##
#

## Create a data.frame
delay = data.frame(airplaneData)

## Factoring and labeling the variables (Taddy, n.d.)
delay$DEP_TIME = factor(floor(delay$DEP_TIME/100))
delay$DAY_OF_WEEK = factor(delay$DAY_OF_WEEK, labels = c("M", "T", "W", "R", "F", "S", "U"))
delay$DEP_DEL15 = factor(delay$DEP_DEL15)
delay$WEATHER_DELAY= factor(ifelse(delay$WEATHER_DELAY>=1,1,0)) # (R, n.d.a.)
delay$CARRIER = factor(delay$CARRIER, levels = c("AA","B6","DL","EV","UA"))
levels(delay$CARRIER) = c("American", "JetBlue", "Delta", "ExpressJet", "UnitedAir")

## Quick understanding the data
summary(delay)
delayed15 = as.numeric(levels(delay$DEP_DEL15)[delay$DEP_DEL15])
hist(delayed15, freq=F, main = "Histogram of Delays of 15 mins or longer", xlab = "time >= 15 mins (1) or time < 15 (0)")

### Create the training and testing data (60/40%)
### Training data, not randomly selected because the goal is to predict the next 40% of the airflight delays potentials
ntotal=length(delay$DAY_OF_WEEK)    # Total number of datapoints assigned dynamically
ntrain = sample(1:ntotal,floor(ntotal*(0.6))) # Take values 1 - n*0.6 
ntest = ntotal-floor(ntotal*(0.6))       # The number of test cases (40% of the data)
trainingData = cbind(delay$DAY_OF_WEEK[ntrain], delay$CARRIER[ntrain],delay$ORIGIN[ntrain],delay$DEST[ntrain],delay$DEP_TIME[ntrain],delay$WEATHER_DELAY[ntrain],delayed15[ntrain])
testingData  = cbind(delay$DAY_OF_WEEK[-ntrain], delay$CARRIER[-ntrain],delay$ORIGIN[-ntrain],delay$DEST[-ntrain],delay$DEP_TIME[-ntrain],delay$WEATHER_DELAY[-ntrain],delayed15[-ntrain])

## Partitioning the train data by half
trainFirst= trainingData[trainingData[,7]<0.5,]
trainSecond= trainingData[trainingData[,7]>0.5,]

### Prior probabilities = p(theta) (Cowles, Kass, & O'Hagan, 2009)
## Dependent variable: time delayed >= 15
## 77.2973% of the training model didn't have a delay, but 22.7027% did have a delay of 15 or greater minutes
tdelay=table(delayed15[ntrain])/sum(table(delayed15[ntrain]))

### Prior probabilities between the partitioned training data
## Independent variable: Day of the week (% flights occured in which day of the week)
tday1=table(trainFirst[,1])/sum(table(trainFirst[,1]))
tday2=table(trainSecond[,1])/sum(table(trainSecond[,1]))

## Independent variable: Carrier (% flights occured in which carrier)
tcarrier1=table(trainFirst[,2])/sum(table(trainFirst[,2]))
tcarrier2=table(trainSecond[,2])/sum(table(trainSecond[,2]))

## Independent variable: Origin (% flights occured in which originating airport)
tOrigin1=table(trainFirst[,3])/sum(table(trainFirst[,3]))
tOrigin2=table(trainSecond[,3])/sum(table(trainSecond[,3]))

## Independent variable: Destination (% flights occured in which destinateion airport)
tdest1=table(trainFirst[,4])/sum(table(trainFirst[,4]))
tdest2=table(trainSecond[,4])/sum(table(trainSecond[,4]))

## Independent variable: Department Time (% flights occured in which time of the day)
tTime1=table(trainFirst[,5])/sum(table(trainFirst[,5]))
tTime2=table(trainSecond[,5])/sum(table(trainSecond[,5]))

## Independent variable: Weather (% flights delayed because of adverse weather conditions)
twx1=table(trainFirst[,6])/sum(table(trainFirst[,6]))
twx2=table(trainSecond[,6])/sum(table(trainSecond[,6]))

### likelihoods = p(y|theta) (Cowles, Kass, & O'Hagan, 2009)
likelihood1=tday1[testingData[,1]]*tcarrier1[testingData[,2]]*tOrigin1[testingData[,3]]*tdest1[testingData[,4]]*tTime1[testingData[,5]]*twx1[testingData[,6]]
likelihood2=tday2[testingData[,1]]*tcarrier2[testingData[,2]]*tOrigin2[testingData[,3]]*tdest2[testingData[,4]]*tTime2[testingData[,5]]*twx2[testingData[,6]]

### Predictions using bayes theory = p(theta|y)= p(theta)*P(y|theta)/(SUM(P(theta)*P(y|theta))) (Cowles, Kass, & O'Hagan, 2009)
Bayes=(likelihood2*tdelay[2])/(likelihood2*tdelay[2]+likelihood1*tdelay[1])
hist(Bayes, freq=F, main="Bayesian Analysis of flight delay data")
plot(delayed15[-ntrain]~Bayes, main="Bayes results versus actual results for flights delayed >= 15 mins", xlab="Bayes Analysis Prediction of which cases will be delayed", ylab="Actual results from test data showing delayed cases")

## The probability of 0.5 or larger
densityMeasure = table(delayed15[-ntrain],floor(Bayes+0.5))
probabilityOfXlarger=(densityMeasure[1,2]+densityMeasure[2,1])/ntest
probabilityOfXlarger

## The probability of 0.3 or larger
densityMeasure = table(delayed15[-ntrain],floor(Bayes+0.7))
probabilityOfXlarger=(densityMeasure[1,2]+densityMeasure[2,1])/ntest
probabilityOfXlarger

### References
### Cowles, K., Kass, R., & O'Hagan, T. (2009) What is Bayesian Analysis? Retrieved from https://bayesian.org/Bayes-Explained
### R (n.d.a) Conditional Element Selection. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/base/html/ifelse.html
### R (n.d.b) Get or set working directory. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/base/html/getwd.html
### Taddy, M. (n.d.). credit.R: German Credit Data. Retrieved from http://faculty.chicagobooth.edu/matt.taddy/teaching/credit.R

