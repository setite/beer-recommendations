library(foreign)
library(haven)
library(readr)
library(tidyverse)
library(dplyr)
library(broom)
##################################################
###############   Data Importation    ############
##################################################

#beer_df <- read.csv("data/AESIS2.CSV", header = T, row.names = 1, na = "NA") # uses first row as row name
#beer_df_slow <- read.csv("data/beer_reviews.csv", header = T, na = "NA")

beer_df <- read_csv("data/beer_reviews.csv", col_names = T, na = "NA")
str(beer_df)
tbl_df(beer_df)
glimpse(beer_df)
beer_df

beer_df_core <- beer_df
beer_df_core <- na.omit(beer_df_core) #not always best to listwise delete but there is a lot of data here

#beer_df_core$brewery_id <- as.factor(beer_df_core$brewery_id)
#beer_df_core$brewery_name <- as.factor(beer_df_core$brewery_name)
#beer_df_core$beer_beerid <- as.factor(beer_df_core$beer_beerid)
#beer_df_core$beer_name <- as.factor(beer_df_core$beer_name)

str(as.factor(beer_df$beer_name))
str(as.factor(beer_df$beer_beerid))
str(beer_df_core)

count_abv = count(beer_df_core$beer_abv)
count_abv

# aggregate data of beer_abv for each beer, returning means doesn't have any effect on the ABV value
# for numeric variables
aggdata <-aggregate(beer_df, by=list(beer_df$beer_name,beer_df$beer_abv), 
                    FUN=mean, na.rm=TRUE)
print(aggdata)
df_abv <- aggdata[c(1:2)]
cor(aggdata$Group.1, aggdata$Group.2)
#beer_df_core <- beer_df[-c(3,7)]
#beer_df_core <- na.omit(beer_df_core)
#beer_df_min  <- beer_df_core[!(beer_df_core$ETHNICITY==1),]

#beer_df_short <- beer_df_core[c(3:5,7:8,10)]
beer_over_20 <- dplyr::filter(beer_df_core, beer_abv > 20)

# take a random sample of size 10000 from a dataset mydata 
# sample without replacement
beer_df_10k <- beer_df_core[sample(1:nrow(beer_df_core), 10000, replace=FALSE),]
beer_df_100k <- beer_df_core[sample(1:nrow(beer_df_core), 100000, replace=FALSE),]
beer_df_500k <- beer_df_core[sample(1:nrow(beer_df_core), 500000, replace=FALSE),]
##################################################
#############   End Data Importation    ##########
##################################################


# Three examples for doing the same computations

mydata$sum <- mydata$x1 + mydata$x2
mydata$mean <- (mydata$x1 + mydata$x2)/2

attach(beer_df)
beer_df$sum <- x1 + x2
beer_df$mean <- (brewery_name + x2)/2
detach(beer_df)

mydata <- transform( mydata,
                     sum = x1 + x2,
                     mean = (x1 + x2)/2 
                    )


##################################################
################   Normalization   ###############
##################################################
sink(file = "sink/jsmod_summary.txt", type = "output")
library(QuantPsyc)
beer_df_norm <- beer_df_core
beer_df_norm$n_review_overall     <- Normalize(beer_df_core$review_overall)
beer_df_norm$n_review_aroma       <- Normalize(beer_df_core$review_aroma)
beer_df_norm$n_review_appearance  <- Normalize(beer_df_core$review_appearance)
beer_df_norm$n_review_palate      <- Normalize(beer_df_core$review_palate)
beer_df_norm$n_review_taste       <- Normalize(beer_df_core$review_taste)
str(beer_df_norm)
sink()
##################################################
##############   End Normalization   #############
##################################################

##################################################
###############   Normality Check   ##############
##################################################
library(QuantPsyc)
plotNormX(beer_df_core$beer_abv) #check plot after each one to make sure it's normalized

# Basic Scatterplot Matrix
pairs(~beer_abv + brewery_name,
      data=beer_df_core, 
      main="Simple Scatterplot Matrix")
pairs(~BDHI_TOTAL + BDHI_SUS_TOTAL + BDHI_RES_TOTAL + BDHI_IRR_TOTAL,
      data=beer_df_core, 
      main="Simple Scatterplot Matrix")

library(QuantPsyc)	# if you do not have this yet, use install.packages(QuantPsyc) or the pull down menu 
#require(graphics)
norm(beer_df_core$BDHI_SUS_TOTAL)
multTest_beer_df_core <- mult.norm(beer_df_core)
plotNormX(beer_df_core$BDHI_SUS_TOTAL)
##################################################
#############   End Normality Check   ############
##################################################


##################################################
#################   Imputation   #################
##################################################
#omit NAs so imputation can be done
beer_df_core2 <- beer_df_core[c(4:15)]
beer_df_aesis <- 
  beer_df_core[!is.na(beer_df_core)]
beer_df_eis <- beer_df_eis[!is.na(beer_df_eis)]
View(beer_df_eis)
#beer_df_aesisna <- max(na.omit(beer_df_aesis))
#beer_df_eisna <- max(na.omit(beer_df_eis))

#Missing Data and Imputation
# Perform quick amputation
library(mice)
is.na.data.frame(beer_df_amp)
beer_df_amped <- ampute(data = beer_df_amp)
result4 <- ampute(data = complete.data, type = c("RIGHT", "TAIL", "LEFT"))
str(beer_df_amp)
beer_df_core[4:16] <- lapply(beer_df_core[4:16], as.numeric)
beer_df_core[1:6] <- lapply(beer_df_core[1:6], as.integer)
str(beer_df_core)

# do default multiple imputation on a numeric matrix
imp <- mice(beer_df_amp)
imp

# list the actual imputations for BMI
imp$imputations$bmi

# first completed data matrix
complete(imp)
beer_df_temp <- AESIS
beer_df_temp <- AESIS[c(5,13:15,144:155)]
beer_df_temp <- na.omit(beer_df_temp)
beer_df_core$GENDER <- beer_df_temp$GENDER
beer_df_core[17] <- lapply(beer_df_core[17], as.factor)

str(beer_df_core)
##################################################
###############   End Imputation   ###############
##################################################

##################################################
##################   Outliers    #################
##################################################
options(width=1000)

library(QuantPsyc)	# Fletchers package made for Quant Class 

norm(Test1CFA_data$X10)
multTest_beer_df_core <- mult.norm(beer_df_norm[,4:15],chicrit=.001)
multTest_beer_df_core <- mult.norm(beer_df_core[c(19:50,58:63,72:80)],chicrit=.001)
multTest_beer_df_core
#multTest_eis <- mult.norm(beer_df_core[,61:81],chicrit=.001)
#multTest_eis
# contains a list of 3 things ... 
# mult.test, Dsq (mahalanobis distances), CriticalDsq (a cut using some default setting)
# Dsq larger than CriticalDsq are multivariate outliers (similar to z scores larger than 3)



library(stargazer)
multTest_beer_df_core$mult.test

write.csv(multTest_beer_df_core$Dsq, file = "sink/multi_norm.csv")
write.csv(multTest_beer_df_core$Dsq, file = "sink/multi_norm_2.csv")
write.csv(multTest_beer_df_core$Dsq, file = "sink/multi_norm_3.csv")
write.csv(multTest_beer_df_core$Dsq, file = "sink/multi_norm_4.csv")
write.csv(multTest_beer_df_core$Dsq, file = "sink/multi_norm_5.csv")

#Sx <- cov(beer_df_core)
#D2<-mahalanobis(beer_df_core,colMeans(beer_df_core), Sx)
multTest_beer_df_core$Dsq > multTest_beer_df_core$CriticalDsq
multTest_beer_df_core
aesis_path_model_1.sem

###Remove Outliers Identified
# Round 1 15 Outliers
beer_df_core <- beer_df_core[-c(33,	59,	134,	137,	166,	172,	184,	218,	229,	271,	272,	303,	323,	425,	441), ]
beer_df_core <- beer_df_core[-c(11,	38,	59,	242,	274,	317,	420), ]
beer_df_core <- beer_df_core[-c(8,	112,	129,	179,	180,	278,	301), ]
beer_df_core <- beer_df_core[-c(207,	209,	348), ]



####

# Assessing Outliers
outlierTest(aesismodel_sensitive2.sem, data = beer_df_core) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(beer_df_core)-length(aesismodel_sensitive2.sem$coefficients)-2)) 
plot(aesismodel_sensitive2.sem, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
##################################################
################   End Outliers    ###############
##################################################
MCAR(beer_df_core, perc.miss = 0.3, setseed = 13)
LittleMCAR(beer_df_core)
LittleMCAR()