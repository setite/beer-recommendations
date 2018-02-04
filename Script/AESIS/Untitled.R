## NET - Nested Models and Equivalence


library(lavaan)
library(qgraph)
library(QuantPsyc)

## NET procedure as applied to LISREL ex8.2 ... 

AESISmodel1 <- '
<- '
# regressions using ~ to relate variables
GRADES ~ INTELLNC + SIBLINGS + FATHEDUC + FATHOCCU
EDUCEXP ~ GRADES + INTELLNC + SIBLINGS + FATHEDUC + FATHOCCU
OCCUASP ~ EDUCEXP + GRADES + INTELLNC + SIBLINGS + FATHEDUC + FATHOCCU

'
'

str(AESIS_data)

norm(AESIS_data$SES)

str(multTestNPV)
# contains a list of 3 things ... 
# mult.test, Dsq (mahalanobis distances), CriticalDsq (a cut using some default setting)
# Dsq larger than CriticalDsq are multivariate outliers (similar to z scores larger than 3)

multTestNPV$mult.test		

## which individual variable is skewed - if any?
for (i in 1:ncol(npv)) print(norm(npv[,i]))
apply(npv, 2, norm) #this command runs normality against each variable I think

# Just how skewed is ...
eda.uni(npv[,3])
plotNormX(npv[,3])

cov(npv)	# if you have missing data, you need to specify the use= argument
cor(npv)	# just think of as standardized covariances

# Brief tangent in notes ... 
library(lavaan)

myCov <- cov(npv)

lower <- '
1 
-.1 1 
.277 -.152 1
.25 -.108 .611 1 
.572 -.105 .294 .248 1
.489 -.213 .446 .41 .597 1 
.335 -.153 .303 .331 .478 .651 1'



EX_45.cov <- getCov(lower, 
                    names=c('INTELLNC', 'SIBLINGS', 'FATHEDUC', 'FATHOCCU', 'GRADES', 'EDUCEXP', 'OCCUASP'))

EX_45.cov	# This is the format that lavaan is looking for if inputting cor/cov

## Illustrating Polychoric and Polyserials ... 

library(polycor)
hetcor(npv)	 # all are Pearson since these variables are considered continuous

cnpv <- hetcor(npv)
str(cnpv)
cnpv$correlations

which(cnpv$correlations/cnpv$std.errors > 2) #see what Z are over 2
cnpv$correlations/cnpv$std.errors > 2 #see what Z are over 2


#### 
## But, what if we have data treating each as ordinal ... 

Ex71dat <- read.csv('DATA/EX71.csv', header=FALSE, colClasses = c(rep('factor', 8)))
#classes <- sapply(Ex71dat, class)
#classes <- edit(classes)
names(Ex71dat) <- c('HUMRGHTS', 'EQUALCON', 'RACEPROB', 'EQUALVAL', 'EUTHANAS', 'CRIMEPUN', 'CONSCOBJ', 'GUILT')

classes <- sapply(Ex71dat, class)
classes <- edit(classes)


Ex71cor <- hetcor(Ex71dat)
Ex71cor$correlations

library(psych)
cor.test(Ex71cor)
test <- data.frame(Ex71dat)
cor.test(test)

# challenge ... 
Ex71datInt <- read.csv('DATA/EX71.csv', header=FALSE)
names(Ex71datInd) <- c('HUMRGHTS', 'EQUALCON', 'RACEPROB', 'EQUALVAL', 'EUTHANAS', 'CRIMEPUN', 'CONSCOBJ', 'GUILT')
cor(Ex71datInt)	
# Ex71cor$correlations - cor(Ex71datInt)




##### Transformations ... 
# creating a copy, because I want to keep npv intact ... 
npvT <- npv

npvT$V3t <- sqrt(npvT$V3)
plotNormX(npvT$V3)
#x11()
plotNormX(npvT$V3t)
# refer to Tabacknick and Fidell (Multivariate) text on common transformations

# Normalizing ... (QuantPsyc package)
npvT$V3n <- Normalize(npvT$V3)	# note, this is not useful, if the specific scores are meaningful
plotNormX(npvT$V3n)


### EFA 

npv.FA.3varimax <- factanal (npv, factors = 3, rotation = 'varimax')

npv.FA.3promax <- factanal (npv, factors = 3, rotation = 'promax')

# what is the RMSEA for this model ... (NOTE - typo in notes.doc)
RMSEA <- function(chisq, df, N)
{
  chidf <- ifelse( chisq - df > 0, chisq - df, 0)
  sqrt( chidf / (N*df) )
}

nrow(npv) # 145
RMSEA(9.38, 12, 145)
# Note, as an EFA, this allows for all possible cross-loadings, hence a pretty good fit

# suppose we didn't fit a 3 factor model initially, what if we tried only 1 factor 
npv.FA.1promax <- factanal (npv, factors = 1, rotation = 'promax')
# very small p-value, large chisq
RMSEA(175.49, 27, 145) # large, much too large.

npv.FA.2promax <- factanal (npv, factors = 2, rotation = 'promax')
# again a sig. chisq
RMSEA(61.7, 19, 145) # again, too large (looking for < .08, preferably, < .05)

## To obtain factor scores, add the scores = argument ... 

npv.FA.3promax  <- factanal (npv, factors = 3, rotation = 'promax', scores = 'regression')

head(npv.FA.3promax$scores)



###############
library(psych)
library(GPArotation)
#This is pretty much the same thing as factanal(), except it gives you RMSEA
#and a few others.
fa(npv, nfactors = 3, rotate = "oblimin", fm="ml")

#This generates a scree plot and also some output recommending the # of factors
#to use. It's actually a parallel analysis, which is the same thing.
scree <- fa.parallel(npv, fm="ml", fa="fa")
