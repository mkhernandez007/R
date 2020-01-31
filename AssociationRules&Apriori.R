setwd("C:/Users/fj998d/Documents/R/dataSets")
LastFM=read.csv("lastfm.csv", header = F, sep = ",") ## (Celma, 2009)

#
##
###----------------------------------------------------------------------------------------------------------------
## Variables: UserID = V1; ArtistID = V2; ArtistName = V3; PlayCount = V4
###----------------------------------------------------------------------------------------------------------------
## Apriori info(Hahsler, Grun, Hornic, & Buchta, n.d.):
##   Constraints for apriori are known as support and confidence, the lower the confidence or supprot the more rules.
##     * Support is the proportion (%) of transactions in the data set with that exact item. 
##     * Confidence is the proportion (%) of transaction where the rule is correct.
##   The greater the lift, the stronger the assocition rule, thus lift is a deviation measure of the total rule 
##   support from the support expected under independence.
##   Other Contraints used
##     * Max length defines the maximum size of mined frequent item rules.
###----------------------------------------------------------------------------------------------------------------
##
#

head(LastFM)
length(LastFM$V1)

summary(levels(LastFM$V1))
summary(levels(LastFM$V2))

## a-rules package for asociation rules
install.packages("arules")
library(arules)

## Computational enviroment for mining association rules and frequent item sets 
## we need to manpulate the data a bit before using arules, we split the data in the vector
## x into groups defined in vector f. (Hahsler, Grun, Hornic, & Buchta, n.d.)

playlists = split(x=LastFM[,"V2"],f=LastFM$V1) # Convert the data to a matrix so that each fan is a row for artists across the clmns (R, n.d.c.)
playlists = lapply(playlists,unique)           # Find unique attributes in playlist, and create a list of those in playlists (R, n.d.a.; R, n.d.b.)
playtrans = as(playlists,"transactions")       # Converts data and produce rule sets


## Create association rules with a support of 0.01 and confidence of 0.5, with a max length of 3
## which will show the support that listening to one artist gives to other artists; in other words, 
## providing lift to an associated artist. 
musicrules = apriori(playtrans, parameter=list(support=0.01, confidence=0.5, maxlen=3)) # filter the data for rules
musicrules
inspect(musicrules[1:5])

## Choose any subset
inspect(subset(musicrules, subset=lift>5))                        # tell me all the rules with a lift > 5
inspect(subset(musicrules, subset=confidence>0.6))                # tell me all the rules with a confidence of 0.6 or greater
inspect(subset(musicrules, subset=support>0.02& confidence >0.6)) # tell me the rules within a particular CI
inspect(subset(musicrules, subset=rhs%in%"rihanna"))              # tell me all the rules with rihanna in the left hand side
inspect(head(musicrules, n=10, by="lift"))                        # tell me the top 10 rules with the largest lift

## Create association rules with a support of 0.001 and confidence of 0.1, with a max length of 2
artrules = apriori(playtrans, parameter=list(support=0.001, confidence=0.5, maxlen=2)) # filter the data for rules
artrules
inspect(artrules[1:5])

## Choose any subset
inspect(subset(artrules, subset=lift>5))
inspect(subset(artrules, subset=confidence>0.6))
inspect(subset(artrules, subset=support>0.02& confidence >0.6))
inspect(subset(artrules, subset=rhs%in%"rihanna"))
inspect(head(artrules, n=10, by="lift"))

## Write down all the rules into a CSV file for co
write(musicrules, file="musicRulesFromApriori.csv", sep = ",", col.names = NA)
write(artrules, file="artistRulesFromApriori.csv", sep = ",", col.names = NA)

### Reference
### Celma, O. (2009). Music recommendation datasets for research. Retrieved from http://www.dtic.upf.edu/~ocelma/MusicRecommendationDataset/lastfm-360K.html 
### R (n.d.a.) Apply a function over a list or vector. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/base/html/lapply.html
### R (n.d.b.) Extract unique elements. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/base/html/unique.html
### R (n.d.c.) Divide into groups and reassemble. Retrieved from https://stat.ethz.ch/R-manual/R-devel/library/base/html/split.html
### Hahsler, M., Grun, B., Hornic, K., & Buchta, C. (n.d.) Introduction to arules -- A computational enviromentmnt for mining association rules and frequent item sets. Retrieved from https://cran.r-project.org/web/packages/arules/vignettes/arules.pdf
