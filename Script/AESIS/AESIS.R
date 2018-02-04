library(lavaan)
library(qgraph)
library(semPlot)
library(semTools)
library(psych)
library(ggplot2)
library(ISLR)

#Principal Components Analysis

aesis.eis.cfa <- prcomp(aesis_data_eis,scale. = T,center = T)
summary(aesis.eis.cfa)
screeplot(aesis_data_core)
#scores<-as.data.frame(Carseats.pca$x)
#pcaplot<-ggplot(scores,(aes(PC1,PC2,color=Carseats1$ShelveLoc)))+geom_point()
#pcaplot

aesis.cfa<-prcomp(aesis_data,scale. = T,center = T)
summary(aesis.eis.cfa)

###############
library(psych)
#library(GPArotation)
#This is pretty much the same thing as factanal(), except it gives you RMSEA
#and a few others.
fa(aesis_data_core, nfactors = 3, rotate = "oblimin", fm="ml")

#This generates a scree plot and also some output recommending the # of factors
#to use. It's actually a parallel analysis, which is the same thing.

aesis_data_ralesb <- aesis_data_cfa[c("RALESB_1", "RALESB_2", "RALESB_3", "RALESB_4", "RALESB_5", "RALESB_6", "RALESB_7", "RALESB_8")]

scree <- fa.parallel(aesis_data_ralesb, fm="ml", fa="fa")

######

##2nd Order MODEL No Latents
secmod2 <-'
AESIS =~ AESIS_stereotypeAnticipation + AESIS_postinteractRum
#EIS =~ EIS_TOTAL
#AESIS ~~ EIS
#General =~ AESIS + EIS
#General ~~ 1*General
AESIS ~ BDHI_RES_TOTAL + BDHI_SUS_TOTAL + EIS_TOTAL + RALESB_TOTAL + RMAS_TOTAL
#AESIS_SA ~ BDHI_RES_TOTAL + BDHI_SUS_TOTAL + EIS_TOTAL + RALESB_TOTAL + RMAS_TOTAL
#AESIS_PI ~ BDHI_RES_TOTAL + BDHI_SUS_TOTAL + EIS_TOTAL + RALESB_TOTAL + RMAS_TOTAL


'
secmod2.sem <- sem(secmod2, data = aesis_data_core)
secmod2.cfa <- cfa(secmod2, data = aesis_data_core)
semPaths(secmod2, data = aesis_data_core)
summary(secmod2.sem, fit.measures = TRUE)
summary(secmod2.cfa, fit.measures = TRUE)

fitMeasures(secmod2.sem, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
fitMeasures(secmod2.cfa, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

mod_ind_mod2 <- modificationIndices(secmod2.sem, power = FALSE)
#Spoting the top 10
head(mod_ind_mod2[order(mod_ind_mod2$mi, decreasing=TRUE), ], 10)
#And the bigger than 5	
subset(mod_ind_mod2[order(mod_ind_mod2$mi, decreasing=TRUE), ], mi > 5)


omega(aesis_data_core, nfactors=2)

anova(secmod1.sem,secmod2.sem)

semPaths(aesismodel1.sem, layout='circle2', rotation=1, nCharNodes = 4)
##################
aesismodel_sensitive <- '
RacialPressure =~ AESIS_TOTAL + PSS_TOTAL + CES_TOTAL
#General Factor
#RacialPressure  ~~  1*RacialPressure
PSS_TOTAL ~~ CES_TOTAL + AESIS_postinteractRum
# regressions using ~ to relate variables

RacialPressure  ~ AESIS_Total + ETHNICITY_ord + RALESB_TOTAL + RMAS_TOTAL + EIS_TOTAL 

##Modifications
'
semPaths(aesismodel_sensitive, data = aesis_data_core)

aesismodel_sensitive.cfa <- cfa(aesismodel_sensitive, data = aesis_data_core)
aesismodel_sensitive.sem <- sem(aesismodel_sensitive, data = aesis_data_core)

summary(aesismodel_sensitive.sem)
fitMeasures(aesismodel_sensitive.sem, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
fitMeasures(aesismodel_sensitive.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea"))


semPaths(aesismodel_sensitive, data = aesis_data_core)

mod_ind_sens <- modificationIndices(aesismodel_sensitive.sem, power = FALSE)
#Spoting the top 10
head(mod_ind_sens[order(mod_ind_sens$mi, decreasing=TRUE), ], 10)
#And the bigger than 5	
subset(mod_ind_sens[order(mod_ind_sens$mi, decreasing=TRUE), ], mi > 5)
semPaths(aesismodel_sensitive,
         layout='tree2',
         rotation=2,
         nCharNodes = 4)

summary(aesismodel_sensitive.sem)

aesismodel_sensitive2 <- '
RacialPressure =~ AESIS_postinteractRum + PSS_TOTAL + CES_TOTAL
#General Factor
#RacialPressure  ~~  1*RacialPressure
PSS_TOTAL ~~ CES_TOTAL
# regressions using ~ to relate variables

RacialPressure  ~ AESIS_Total + ETHNICITY_ord + RALESB_TOTAL + RMAS_TOTAL + EIS_TOTAL 

##Modifications
'
semPaths(aesismodel_sensitive2, data = aesis_data_core)

aesismodel_sensitive2.cfa <- cfa(aesismodel_sensitive2, data = aesis_data_core)
aesismodel_sensitive2.sem <- sem(aesismodel_sensitive2, data = aesis_data_core)

summary(aesismodel_sensitive2.sem)
fitMeasures(aesismodel_sensitive2.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
fitMeasures(aesismodel_sensitive2.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea"))


semPaths(aesismodel_sensitive2, data = aesis_data_core)
mod_ind_sens2 <- modificationIndices(aesismodel_sensitive2.sem, power = FALSE)
#Spoting the top 10
head(mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], 10)
#And the bigger than 5	
subset(mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], mi > 5)

anova(aesismodel_sensitive.sem,aesismodel_sensitive2.sem)
