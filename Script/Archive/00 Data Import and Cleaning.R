library(foreign)
library(haven)
##################################################
###############   Data Importation    ############
##################################################
AESIS_Final_Database_with_finalized_scale_USE_THIS_ONE_all_race_ <- read_sav('data/AESIS Final Database with finalized scale_USE THIS ONE (all race).sav')
#writing to CSV for better reimportation
write.csv(AESIS_Final_Database_with_finalized_scale_USE_THIS_ONE_all_race_,file = "data/AESIS2.CSV", na = "NA", row.names = F)

#AESIS_data <- read.csv("data/AESIS2.CSV", header = T, row.names = 1, na = "NA") # uses first row as row name
AESIS_data <- read.csv("data/AESIS2.CSV", header = T, na = "NA")

aesis_data_core <- AESIS_data[c(4,5,13:155)]
aesis_data_core <- na.omit(aesis_data_core)
  aesis_data_min  <- aesis_data_core[!(aesis_data_core$ETHNICITY==1),]

aesis_data_short <- AESIS_data[c(13:15,146:155)]
aesis_data_short <- na.omit(aesis_data_short)
##################################################
#############   End Data Importation    ##########
##################################################

##################################################
###############   Ordered Variables   ############
##################################################
library(car)

AESIS_data$ETHNICITY <- recode(AESIS_data$ETHNICITY, "1 = 1; 2 = 5; 3 = 4;  4 = 2; 5 = 3; 6 = 3; 7 = 3; as.factor.result = TRUE")
AESIS_data$ETHNICITY <- factor(AESIS_data$ETHNICITY,
                                        levels = c(1,2,3,4,5),
                                        labels = c("White", "Asian", "Misc", "Hispanic", "Black"))
AESIS_data$GENDER <- factor(AESIS_data$GENDER,
                                        levels = c(1,2),
                                        labels = c("Male", "Female"))
AESIS_data$SubjectID <- lapply(AESIS_data$SubjectID,
                                        as.character)
str(AESIS_data[c(2:15)])

#### Original Nonsensical Order
AESIS_data$ETHNICITY <- factor(AESIS_data$ETHNICITY,
                               levels = c(1,2,3,4,5,6,7),
                               labels = c("White", "Black", "Hispanic", "Asian", "Native American", "Other", "Multiracial"))
####
#### Core Set
aesis_data_core$ETHNICITY_ord <- recode(aesis_data_core$ETHNICITY, "1 = 1; 2 = 5; 3 = 4;  4 = 2; 5 = 3; 6 = 3; 7 = 3; as.factor.result = TRUE")  #should recode already - kinda doesn't work
# variable v1 is coded 1, 2 or 3
aesis_data_core$ETHNICITY_ord <- factor(aesis_data_core$ETHNICITY_ord,
                                        levels = c(1,2,3,4,5),
                                        labels = c("White", "Asian", "Misc", "Hispanic", "Black"))
aesis_data_core$ETHNICITY_ord <- lapply(aesis_data_core$ETHNICITY_ord,
                                        as.integer,
                                        ordered)
str(aesis_data_core)
aesis_data_core$GENDER <- factor(aesis_data_core$GENDER,
                                 levels = c(1,2),
                                 labels = c("Male", "Female"))
str(aesis_data_core$GENDER)

##################################################
#############   End Ordered Variables   ##########
##################################################

sink(file = "sink/jsmod_summary.txt", type = "output")
##################################################
################   Normalization   ###############
##################################################
library(QuantPsyc)
aesis_data_norm <- aesis_data_core
aesis_data_norm$n_RMAS_TOTAL    <- Normalize(aesis_data_core$RMAS_TOTAL)
aesis_data_norm$n_EIS_TOTAL     <- Normalize(aesis_data_core$EIS_TOTAL)
aesis_data_norm$n_RALESB_TOTAL  <- Normalize(aesis_data_core$RALESB_TOTAL)
aesis_data_norm$n_CES_TOTAL     <- Normalize(aesis_data_core$CES_TOTAL)
aesis_data_norm$n_PSS_TOTAL     <- Normalize(aesis_data_core$PSS_TOTAL)
aesis_data_norm$n_BDHI_TOTAL    <- Normalize(aesis_data_core$BDHI_TOTAL)
aesis_data_norm$n_AESIS_Total   <- Normalize(aesis_data_core$AESIS_Total)

str(aesis_data_norm)
##################################################
##############   End Normalization   #############
##################################################

##################################################
###############   Normality Check   ##############
##################################################
plotNormX(aesis_data_core$BF3) #check plot after each one to make sure it's normalized
#data for imputatin test
#aesis_data_amp <- AESIS[c(144:155)]
write.csv("AESIS.csv", na = "NA", row.names = T)

# Basic Scatterplot Matrix
pairs(~AESIS_Total + RMAS_TOTAL + BDHI_SUS_TOTAL + EIS_TOTAL + RALESB_TOTAL
      #ETHNICITY_ord + MaritalStatus + SES + 
      ,
      data=aesis_data_core, 
      main="Simple Scatterplot Matrix")
pairs(~BDHI_TOTAL + BDHI_SUS_TOTAL + BDHI_RES_TOTAL + BDHI_IRR_TOTAL
      #ETHNICITY_ord + MaritalStatus + SES + 
      ,
      data=aesis_data_core, 
      main="Simple Scatterplot Matrix")

library(QuantPsyc)	# if you do not have this yet, use install.packages(QuantPsyc) or the pull down menu 

norm(aesis_data_core$BDHI_SUS_TOTAL)
multTest_aesis_data_core <- mult.norm(aesis_data_core)
plotNormX(aesis_data_core$BDHI_SUS_TOTAL)
##################################################
#############   End Normality Check   ############
##################################################


##################################################
#################   Imputation   #################
##################################################
#omit NAs so imputation can be done
aesis_data_core2 <- aesis_data_core[c(4:15)]
aesis_data_aesis <- 
  aesis_data_core[!is.na(aesis_data_core)]
aesis_data_eis <- aesis_data_eis[!is.na(aesis_data_eis)]
View(aesis_data_eis)
#aesis_data_aesisna <- max(na.omit(aesis_data_aesis))
#aesis_data_eisna <- max(na.omit(aesis_data_eis))

#Missing Data and Imputation
# Perform quick amputation
library(mice)
is.na.data.frame(aesis_data_amp)
aesis_data_amped <- ampute(data = aesis_data_amp)
result4 <- ampute(data = complete.data, type = c("RIGHT", "TAIL", "LEFT"))
str(aesis_data_amp)
aesis_data_core[4:16] <- lapply(aesis_data_core[4:16], as.numeric)
aesis_data_core[1:6] <- lapply(aesis_data_core[1:6], as.integer)
str(aesis_data_core)

# do default multiple imputation on a numeric matrix
imp <- mice(aesis_data_amp)
imp

# list the actual imputations for BMI
imp$imputations$bmi

# first completed data matrix
complete(imp)
aesis_data_temp <- AESIS
aesis_data_temp <- AESIS[c(5,13:15,144:155)]
aesis_data_temp <- na.omit(aesis_data_temp)
aesis_data_core$GENDER <- aesis_data_temp$GENDER
aesis_data_core[17] <- lapply(aesis_data_core[17], as.factor)

str(aesis_data_core)
##################################################
###############   End Imputation   ###############
##################################################

##################################################
##################   Outliers    #################
##################################################
options(width=1000)

library(QuantPsyc)	# Fletchers package made for Quant Class 

norm(Test1CFA_data$X10)
multTest_aesis_data_core <- mult.norm(aesis_data_norm[,4:15],chicrit=.001)
multTest_aesis_data_core <- mult.norm(aesis_data_core[c(19:50,58:63,72:80)],chicrit=.001)
multTest_aesis_data_core
#multTest_eis <- mult.norm(aesis_data_core[,61:81],chicrit=.001)
#multTest_eis
# contains a list of 3 things ... 
# mult.test, Dsq (mahalanobis distances), CriticalDsq (a cut using some default setting)
# Dsq larger than CriticalDsq are multivariate outliers (similar to z scores larger than 3)



library(stargazer)
multTest_aesis_data_core$mult.test

write.csv(multTest_aesis_data_core$Dsq, file = "sink/multi_norm.csv")
write.csv(multTest_aesis_data_core$Dsq, file = "sink/multi_norm_2.csv")
write.csv(multTest_aesis_data_core$Dsq, file = "sink/multi_norm_3.csv")
write.csv(multTest_aesis_data_core$Dsq, file = "sink/multi_norm_4.csv")
write.csv(multTest_aesis_data_core$Dsq, file = "sink/multi_norm_5.csv")

#Sx <- cov(aesis_data_core)
#D2<-mahalanobis(aesis_data_core,colMeans(aesis_data_core), Sx)
multTest_aesis_data_core$Dsq > multTest_aesis_data_core$CriticalDsq
multTest_aesis_data_core
aesis_path_model_1.sem

###Remove Outliers Identified
# Round 1 15 Outliers
aesis_data_core <- aesis_data_core[-c(33,	59,	134,	137,	166,	172,	184,	218,	229,	271,	272,	303,	323,	425,	441), ]
aesis_data_core <- aesis_data_core[-c(11,	38,	59,	242,	274,	317,	420), ]
aesis_data_core <- aesis_data_core[-c(8,	112,	129,	179,	180,	278,	301), ]
aesis_data_core <- aesis_data_core[-c(207,	209,	348), ]



####

# Assessing Outliers
outlierTest(aesismodel_sensitive2.sem, data = aesis_data_core) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(aesis_data_core)-length(aesismodel_sensitive2.sem$coefficients)-2)) 
plot(aesismodel_sensitive2.sem, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
##################################################
################   End Outliers    ###############
##################################################
MCAR(aesis_data_core, perc.miss = 0.3, setseed = 13)
LittleMCAR(aesis_data_core)
LittleMCAR()
