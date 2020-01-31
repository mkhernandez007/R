#
### ----------------------------------------------------------------------------------------------------------
## Use the prostate cancer dataset available in R, in which biopsy results are given for 97 men. 
## Goal:  Predict tumor spread in this dataset of 97 men who had undergone a biopsy.
## The measures to be used for prediction are BPH=lbhp, PSA=lpsa, Gleason Score=gleason, CP=lcp, 
## and size of prostate=lcavol. 
### ----------------------------------------------------------------------------------------------------------
## 

install.packages("lasso2")
library(lasso2)
data("Prostate")

install.packages("rpart")
library(rpart)

## Grow a classification tree
classification = rpart(svi~lbph+lpsa+gleason+lcp+lcavol, data=Prostate, method="class")
printcp(classification) # display the results
plotcp(classification)  # visualization cross-validation results
plot(classification, uniform = T, main="Classification Tree for prostate cancer") # plot tree
text(classification, use.n = T, all = T, cex=.8)                                  # create text on the tree

## Grow a regression tree
Regression = rpart(svi~lbph+lpsa+gleason+lcp+lcavol, data=Prostate, method="anova")
printcp(Regression) # display the results
plotcp(Regression)  # visualization cross-validation results
plot(Regression, uniform = T, main="Regression Tree for prostate cancer") # plot tree
text(Regression, use.n = T, all = T, cex=.8)                              # create text on the tree


install.packages("party")
library(party)

## Grow a conditional inference tree 
conditional = ctree(svi~lbph+lpsa+gleason+lcp+lcavol, data=Prostate)
conditional # display the results
plot(conditional, main="Conditional inference tree for prostate cancer")

### REFERENCES
### lasso2 (n.d.) Prostate Canceer Data. Retrieved from http://www.biostat.jhsph.edu/~ririzarr/Teaching/649/prostate.html
### Quick-R (n.d.) Tree-Based Models. Retrieved from http://www.statmethods.net/advstats/cart.html