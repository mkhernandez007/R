#
## The German credit data contains attributes and outcomes on 1,000 loan applications. 
## .	You need to use random selection for 900 cases to train the program, and then the other 100 cases will be used for testing. 
## .	Use duration, amount, installment, and age in this analysis, along with loan history, purpose, and rent. 
### ----------------------------------------------------------------------------------------------------------
## Data source: https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data
## Metadata file: https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.doc
#

#
## Reading the data from source and displaying the top five entries.
#
credits=read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data", header = F, sep = " ")
head(credits)

#
## Defining the variables (Taddy, n.d.)
#

default = credits$V21 - 1 # set default true when = 2
duration = credits$V2
amount = credits$V5
installment = credits$V8
age = credits$V13
history = factor(credits$V3, levels = c("A30", "A31", "A32", "A33", "A34"))
purpose = factor(credits$V4, levels = c("A40","A41","A42","A43","A44","A45","A46","A48","A49","A410"))
rent = factor(credits$V15=="A151") # renting status only
# rent = factor(credits$V15 , levels = c("A151","A152","153")) # full property status

#
## Re-leveling the variables (Taddy, n.d.)
#
levels(history) = c("great", "good", "ok", "poor", "horrible")
levels(purpose) = c("newcar", "usedcar", "furniture/equip", "radio/TV", "apps", "repairs", "edu", "retraining", "biz", "other")
# levels(rent) = c("rent", "own", "free") # full property status

#
## Create a new matrix called "cred" with the 8 defined variables (Taddy, n.d.)
#
credits$default = default
credits$duration= duration
credits$amount  = amount
credits$installment = installment
credits$age     = age
credits$history = history
credits$purpose = purpose
credits$rent    = rent
cred = credits[,c("default","duration","amount","installment","age","history","purpose","rent")]

#
##  Plotting & reading to make sure the data was transfered correctly into this dataset and present summary stats (Taddy, n.d.)
#

plot(cred)
cred[1:3,]
summary(cred[,])

#
## Create a design matrix, such that factor variables are turned into indicator variables
#

Xcred = model.matrix(default~., data=cred)[,-1]
Xcred[1:3,]

#
## Creating training and prediction datasets: Select 900 rows for esitmation and 100 for testing
#

set.seed(1)
train = sample(1:1000,900)

## Defining which x and y values in the design matrix will be for training and for testing
xtrain = Xcred[train,]
xnew = Xcred[-train,]
ytrain = cred$default[train]
ynew = cred$default[-train]

#
## logistic regresion
#

datas=data.frame(default=ytrain,xtrain)
creditglm=glm(default~., family=binomial, data=datas)
summary(creditglm)

#
## Confidence Intervals (UCLA: Statistical Consulting Group, 2007)
#

confint(creditglm)
confint.default(creditglm)

#
## Overall effect of the rank using the wald.test function from the aod library (UCLA: Statistical Consulting Group, 2007)
#

install.packages("aod")
library(aod)
wald.test(b=coef(creditglm), Sigma = vcov(creditglm), Terms = 6:9) # for all ranked terms for history
wald.test(b=coef(creditglm), Sigma = vcov(creditglm), Terms = 10:18) # for all ranked terms for purpose
wald.test(b=coef(creditglm), Sigma = vcov(creditglm), Terms = 19) # for the ranked term for rent

#
## Odds Ratio and ANOVA for model analysis (UCLA: Statistical Consulting Group, 2007)
#

exp(coef(creditglm)) 
exp(cbind(OR=coef(creditglm), confint(creditglm))) # odds ration next to the 95% confidence interval for odds ratios
anova(creditglm, test="Chisq") # the wider the gap between the Null Residual and model residual the better

#
## Predicting default from the test data (Alice, 2015; UCLA: Statistical Consulting Group., 2007)
#

newdatas=data.frame(default=ynew,xnew)
newestdata=newdatas[,2:19] #removing the variable default from the data matrix
newestdata$defaultPrediction = predict(creditglm, newdata=newestdata, type = "response")
summary(newdatas)

#
## Plotting and comparing prediction with reality 
#

plot(newdatas$default, newdatas$defaultPrediction) #, newdatas$defaultPrediction)
summary(newdatas$default)
summary(newdatas$defaultPrediction)
## Plotting the true positive rate against the false positive rate (ROC Curve) (Alice, 2015)
install.packages("ROCR")
library(ROCR)
pr  = prediction(newestdata$defaultPrediction, newdatas$default)
prf = performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
## Area under the ROC curve (Alice, 2015)
auc= performance(pr, measure = "auc")
auc= auc@y.values[[1]]
auc # The closer this value is to 1 the better, much better than to 0.5


### References
### Alice, M. (2015). How to perform a logistic regression in R. Retrieved from http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
### Taddy, M. (n.d.). credit.R: German Credit Data. Retrieved from http://faculty.chicagobooth.edu/matt.taddy/teaching/credit.R
### UCLA: Statistical Consulting Group. (2007). R data analysis examples: Logit Regression. Retrieved from http://www.ats.ucla.edu/stat/r/dae/logit.htm
