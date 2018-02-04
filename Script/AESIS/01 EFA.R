library(lavaan)
library(qgraph)
library(sem)
library(semPlot)
library(semTools)
library(ggplot2)
library(ISLR)


library(psych)

omega(aesis_data_core[c(29:60,68:73,82:90)], nfactors=3)

png(filename="images/EIS_Omega.png", width = 893, height = 1200, res = 1200)
omega(AESIS_data[ , paste0("EIS_", 8:13)], nfactors=3)
dev.off()

##################################################
########   Coefficient Alpha Reliability   #######
##################################################
'EIS     =~ EIS_8 + EIS_9 + EIS_10 + EIS_11 + EIS_12 + EIS_13
RALESB  =~ RALESB_1 + RALESB_2 + RALESB_3 + RALESB_4 + RALESB_5 + RALESB_6 + RALESB_7 + RALESB_8 + RALESB_9
RMAS    =~ RMAS_1 +	RMAS_2 + RMAS_3 + RMAS_4 + RMAS_5 + RMAS_6 + RMAS_7 + RMAS_8 + RMAS_9 + RMAS_10 + 
  RMAS_11 + RMAS_12 + RMAS_13 + RMAS_14 + RMAS_15 + RMAS_16 + RMAS_17 + RMAS_18 + RMAS_19 + RMAS_20 + 
  RMAS_21 + RMAS_22 + RMAS_23 + RMAS_24 + RMAS_25 + RMAS_26 + RMAS_27 + RMAS_28 + RMAS_29 + RMAS_30 + RMAS_31 + RMAS_32'

sink(file = "sink/alpha.txt", type = c("output"))
str(AESIS_data[ , paste0("EIS_", 8:13)])
str(AESIS_data[ , paste0("RALESB_", 1:9)])
str(AESIS_data[ , paste0("RMAS_", 1:32)])
str(aesis_data_core[ , paste0("EIS_", 8:13)])
str(aesis_data_core[ , paste0("RALESB_", 1:9)])
str(aesis_data_core[ , paste0("RMAS_", 1:32)])

str(aesis_data_core[c(19:50,58:63,72:80)])

str(AESIS_data[29:60]) # RMAS 1 to 32
str(AESIS_data[68:73]) # EIS 8 to 13
str(AESIS_data[82:90]) # RALESB 1 to 9
str(aesis_data_core[19:50]) # RMAS 1 to 32
str(aesis_data_core[58:63]) # EIS 8 to 13
str(aesis_data_core[72:80]) # RALESB 1 to 9

alpha(AESIS_data[ , paste0("EIS_", 8:13)])
alpha(AESIS_data[ , paste0("RALESB_", 1:9)])
alpha(AESIS_data[ , paste0("RMAS_", 1:32)])
alpha(aesis_data_core[ , paste0("EIS_", 8:13)])
alpha(aesis_data_core[ , paste0("RALESB_", 1:9)])
alpha(aesis_data_core[ , paste0("RMAS_", 1:32)])

omega(AESIS_data[ , paste0("EIS_", 8:13)])
omega(AESIS_data[ , paste0("RALESB_", 1:9)])
omega(AESIS_data[ , paste0("RMAS_", 1:32)])
sink()

#alpha(AESIS_data[c(68:73)]) #EIS
#alpha(AESIS_data[c(29:60)]) #RMAS
#alpha(AESIS_data[c(82:90)]) #RALESB

##################################################
######   End Coefficient Alpha Reliability   #####
##################################################

omega(AESIS_data[ , paste0("EIS_", 8:13)])
omega(AESIS_data[ , paste0("RALESB_", 1:9)])
omega(AESIS_data[ , paste0("RMAS_", 1:32)])

omega(AESIS_data[ , paste0("EIS_", 8:13)], nfactors=2)
omega(AESIS_data[ , paste0("RALESB_", 1:9)], nfactors=2)
omega(AESIS_data[ , paste0("RMAS_", 1:32)], nfactors=2)

omega(aesis_data_core[c(6:9,14:15)], nfactors=2)
omega(aesis_data_core[c(6:9,14:15)], nfactors=3)

##################################################
########   Principal Components Analysis   #######
##################################################
# Pricipal Components Analysis  is UNROTATED
# entering raw data and extracting PCs 
# from the correlation matrix 
fit_cor <- princomp(na.omit(AESIS_data[ , paste0("EIS_", 8:13)]), cor=TRUE)
summary(fit_cor) # print variance accounted for 
loadings(fit_cor) # pc loadings 
plot(fit_cor,type="lines") # scree plot 
fit_cor$scores # the principal components
biplot(fit_cor)
# from the covariance matrix 
fit_cov <- princomp(na.omit(AESIS_data[ , paste0("RALESB_", 1:9)]), cor=FALSE)
summary(fit_cov) # print variance accounted for 
loadings(fit_cov) # pc loadings 
plot(fit_cov,type="lines") # scree plot 
fit_cov$scores # the principal components
biplot(fit_cov)
# Varimax Rotated Principal Components
# retaining 5 components 
library(psych)
fit <- principal(na.omit(AESIS_data[ , paste0("EIS_", 8:13)]), nfactors=3, rotate="varimax")
fit # print results


########################################
##########   RMSEA Function   ##########
########################################
RMSEA <- function(chisq, df, N)
{
  chidf <- ifelse( chisq - df > 0, chisq - df, 0)
  sqrt( chidf / (N*df) )
}
# suppose we didn't fit a 3 factor model initially, what if we tried only 1 factor 
aesis_data_core.FA.2varimax <- factanal (AESIS_data[ , paste0("EIS_", 8:13)], factors = 1, rotation = 'varimax')
RMSEA(aesis_data_core.FA.2varimax$STATISTIC, aesis_data_core.FA.2varimax$dof, aesis_data_core.FA.2varimax$n.obs)
#promax rotation
aesis_data_core.FA.2promax <- factanal (AESIS_data[ , paste0("EIS_", 8:13)], factors = 3, rotation = 'promax')
RMSEA(aesis_data_core.FA.2promax$STATISTIC, aesis_data_core.FA.2promax$dof, aesis_data_core.FA.2promax$n.obs)

aesis_data_core.FA.3promax <- factanal (AESIS_data[ , paste0("EIS_", 8:13)], factors = 3, rotation = 'promax')
RMSEA(aesis_data_core.FA.3promax$STATISTIC, aesis_data_core.FA.3promax$dof, aesis_data_core.FA.3promax$n.obs)
# very small p-value, large chisq
#RMSEA(2138.445, 90, 500) # large, much too large.

#Principal Components Analysis
fit_cor <- princomp(na.omit(AESIS_data[ , paste0("EIS_", 8:13)]), cor=TRUE)

aesis.eis.princomp <- prcomp(na.omit(AESIS_data[ , paste0("EIS_", 8:13)]),scale. = T,center = T)
plot(aesis.eis.princomp)
summary(aesis.eis.princomp)
biplot(aesis.eis.princomp)
screeplot(aesis.eis.princomp)
#scores  <-  as.data.frame(aesis.eis.princomp$x)
#pcaplot<-ggplot(scores,(aes(PC1,PC2,color=Carseats1$ShelveLoc)))+geom_point()
#pcaplot
##################################################
######   End Principal Components Analysis   #####
##################################################
summary(AESIS_data[c(4:15,156)])
##################################################
########   Exploratory Factor Analysis   #########
##################################################
library(psych)
library(GPArotation)
# Exploratory Factor analysis using MinRes (minimum residual) as well as EFA
# by Principal Axis, Weighted Least Squares or Maximum Likelihood

# This is pretty much the same thing as factanal(), except it gives you RMSEA
# and a few others.
fa <- fa(AESIS_data[ , paste0("EIS_", 8:13)], nfactors = 3, rotate = "oblimin", fm="ml")
fa
omega(AESIS_data[ , paste0("EIS_", 8:13)], nfactors=3)

# This generates a scree plot and also some output recommending the # of factors
# to use. It's actually a parallel analysis, which is the same thing.
scree <- fa.parallel(AESIS_data[ , paste0("EIS_", 8:13)], fm="ml", fa="fa") # Recommends 5 factors  for EIS
sink(file = "sink/factanal_eis.txt", type = c("output"))
#######   Promax Rotation   #######
factanal(na.omit(AESIS_data[ , paste0("EIS_", 8:13)]), factors = 3, scores = "regression", rotation = "promax")
#######   Varimax Rotation   ######
factanal(na.omit(AESIS_data[ , paste0("EIS_", 8:13)]), factors = 3, scores = "regression", rotation = "varimax")
#####   Oblique Min Rotation   #####
factanal(na.omit(AESIS_data[ , paste0("EIS_", 8:13)]), factors = 3, scores = "regression", rotation = "oblimin")
sink()
######

#####   Determine Number of Factors to Extract #####
library(nFactors)
ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(AESIS_data[ , paste0("EIS_", 8:13)]),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

###### PCA Variable Factor Map ######
library(FactoMineR)
result <- PCA(na.omit(AESIS_data[ , paste0("EIS_", 8:13)])) # graphs generated automatically
PCA(na.omit(AESIS_data[c(141:155)])) # graphs generated automatically


##################################################
######   End Exploratory Factor Analysis   #######
##################################################

##################################################
#######   Confirmatory Factor Analysis   #########
##################################################

##################################################
#####   End Confirmatory Factor Analysis   #######
##################################################
