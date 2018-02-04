sink(file = "sink/sink_02_sem.txt", type = c("output"))
library(qgraph)
library(semTools)
library(psych)
library(ggplot2)
library(ISLR)

##################################################
########   Confirmatory Factor Analysis   ########
##################################################
library(lavaan)
library(semPlot)
library(sem)
## EIS CFA
eis_mod <-
  '
EIS_EXP =~ EIS_1 + EIS_2 + EIS_3 + EIS_4 + EIS_5 + EIS_6 + EIS_7
EIS_AFF =~ EIS_8 + EIS_9 + EIS_10 + EIS_11 + EIS_12 + EIS_13
EIS_RES =~ EIS_14 + EIS_15 + EIS_16 + EIS_17 + EIS_18 + EIS_19 + EIS_20 + EIS_21
'
eis_mod.cfa <- cfa(eis_mod, data = aesis_data_core, std.lv=TRUE)
summary(eis_mod.cfa, fit.measures = TRUE)

png(filename="images/EIS_CFA.png", width = 900, height = 1200, res = 1200)
semPaths(eis_mod.cfa, data = aesis_data_core, layout = "tree2",
         #width = 600, height = 800,
         rotation = 2, residuals=F,
         what = "col",
         whatLabels = "std", ask = FALSE, curve = 1.0,
         groups = "latents", curveAdjacent = TRUE, curvePivot = TRUE,
         nCharNodes = 0, nCharEdges = 5,
         #sizeMan  = 10,  sizeLat  = 10, sizeInt = 10,
         #sizeMan2 = 4,   sizeLat2 = 5,  sizeInt2 = 10,
         thresholds = TRUE,
         #edge.label.cex=0.8,
         style = "lisrel", shapeLat = "circle",
         #mar = c(bottom, left, top, right)
         mar = c(2, 4, 2, 2),
         title = TRUE
)
dev.off()
newDatLVS <- predict(eis_mod.cfa, aesis_data_core)
newDatLVS

##2nd Order MODEL No Latents
secmod1 <-'
AESIS =~ AESIS_stereotypeAnticipation + AESIS_postinteractRum
EIS_EXP =~ EIS_1 + EIS_2 + EIS_3 + EIS_4 + EIS_5 + EIS_6 + EIS_7
EIS_AFF =~ EIS_8 + EIS_9 + EIS_10 + EIS_11 + EIS_12 + EIS_13
EIS_RES =~ EIS_14 + EIS_15 + EIS_16 + EIS_17 + EIS_18 + EIS_19 + EIS_20 + EIS_21
AESIS_SA  =~ AESIS_1 + AESIS_2 + AESIS_3 + AESIS_4 + AESIS_5 + AESIS_6 + AESIS_7 + AESIS_8 + AESIS_9 + AESIS_10 + AESIS_11
AESIS_PIR =~ AESIS_1 + AESIS_2 + AESIS_3 + AESIS_4 + AESIS_5 + AESIS_6 + AESIS_7 + AESIS_8 + AESIS_9 + AESIS_10 + AESIS_11

#EIS =~ EIS_TOTAL
#AESIS ~~ EIS
#General =~ AESIS + EIS
#General ~~ 1*General

EIS =~ EIS_1 + EIS_2 + EIS_3 + EIS_4 + EIS_5 + EIS_6 + EIS_7 + EIS_8 + EIS_9 + EIS_10 + EIS_11 + EIS_12 + EIS_13 + EIS_14 + EIS_15 + EIS_16 + EIS_17 + EIS_18 + EIS_19 + EIS_20 + EIS_21
AESIS_SA =~ AESIS_1 + AESIS_2 + AESIS_3 + AESIS_4 + AESIS_5 + AESIS_6 + AESIS_7 + AESIS_8 + AESIS_9 + AESIS_10 + AESIS_11

'
secmod1.sem <- sem(secmod1, data = aesis_data_core)
secmod1.cfa <- cfa(secmod1, data = aesis_data_core)
semPaths(secmod1, data = aesis_data_core)
summary(secmod1.sem, fit.measures = TRUE)
summary(secmod1.cfa, fit.measures = TRUE)

fitMeasures(secmod1.sem, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
fitMeasures(secmod1.cfa, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

mod_ind_mod2 <- modificationIndices(secmod1.sem, power = TRUE)
#Spotting the top 10
head(mod_ind_mod2[order(mod_ind_mod2$mi, decreasing=TRUE), ], 10)
#And those bigger than 5	
subset(mod_ind_mod2[order(mod_ind_mod2$mi, decreasing=TRUE), ], mi > 5)

anova(secmod1.sem,secmod1.sem)

semPaths(aesismodel1.sem, layout='circle2', rotation=1, nCharNodes = 4)
##################################################
######   End Confirmatory Factor Analysis   ######
##################################################

##################################################
#############   Largent Model SEM    #############
##################################################
##2nd Order MODEL No Latents
secmod1 <-'
AESIS =~ AESIS_stereotypeAnticipation + AESIS_postinteractRum
EIS =~ EIS_1 + EIS_2 + EIS_3 + EIS_4 + EIS_5 + EIS_6 + EIS_7 + EIS_8 + EIS_9 + EIS_10 + EIS_11 + EIS_12 + EIS_13 + EIS_14 + EIS_15 + EIS_16 + EIS_17 + EIS_18 + EIS_19 + EIS_20 + EIS_21
AESIS_SA  =~ AESIS_1 + AESIS_2 + AESIS_3 + AESIS_4 + AESIS_5 + AESIS_6 + AESIS_7 + AESIS_8 + AESIS_9 + AESIS_10 + AESIS_11
AESIS_PIR =~ AESIS_1 + AESIS_2 + AESIS_3 + AESIS_4 + AESIS_5 + AESIS_6 + AESIS_7 + AESIS_8 + AESIS_9 + AESIS_10 + AESIS_11

#EIS =~ EIS_TOTAL
#AESIS ~~ EIS
#General =~ AESIS + EIS
#General ~~ 1*General

EIS =~ EIS_1 + EIS_2 + EIS_3 + EIS_4 + EIS_5 + EIS_6 + EIS_7 + EIS_8 + EIS_9 + EIS_10 + EIS_11 + EIS_12 + EIS_13 + EIS_14 + EIS_15 + EIS_16 + EIS_17 + EIS_18 + EIS_19 + EIS_20 + EIS_21
AESIS_SA =~ AESIS_1 + AESIS_2 + AESIS_3 + AESIS_4 + AESIS_5 + AESIS_6 + AESIS_7 + AESIS_8 + AESIS_9 + AESIS_10 + AESIS_11

'
secmod1.sem <- sem(secmod1, data = aesis_data_core)
secmod1.cfa <- cfa(secmod1, data = aesis_data_core)
semPaths(secmod1, data = aesis_data_core)
summary(secmod1.sem, fit.measures = TRUE)
summary(secmod1.cfa, fit.measures = TRUE)

fitMeasures(secmod1.sem, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
fitMeasures(secmod1.cfa, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

mod_ind_mod2 <- modificationIndices(secmod1.sem, power = TRUE)
#Spotting the top 10
head(mod_ind_mod2[order(mod_ind_mod2$mi, decreasing=TRUE), ], 10)
#And those bigger than 5	
subset(mod_ind_mod2[order(mod_ind_mod2$mi, decreasing=TRUE), ], mi > 5)

anova(secmod1.sem,secmod1.sem)

semPaths(aesismodel1.sem, layout='circle2', rotation=1, nCharNodes = 4)
##################################################
###########   End Largent Model SEM    ###########
##################################################

aesismodel_sensitive <- '
Stress  =~  PSS_TOTAL + CES_TOTAL + BDHI_TOTAL
RacialExp =~  RMAS_TOTAL + RALESB_TOTAL + AESIS_Total + EIS_TOTAL

#General Factor
RacialPressure =~ RacialExp + Stress
RacialPressure  ~~  1*RacialPressure

# regressions using ~ to relate variables
#RacialPressure  ~ ETHNICITY_ord

##Modifications


'
semPaths(aesismodel_sensitive, data = aesis_data_core)

aesismodel_sensitive.cfa <- cfa(aesismodel_sensitive, data = aesis_data_core)
aesismodel_sensitive.sem <- sem(aesismodel_sensitive, data = aesis_data_core)
#inspect(aesismodel_sensitive.sem,"cov.lv")
summary(aesismodel_sensitive.sem)
fitMeasures(aesismodel_sensitive.sem, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
fitMeasures(aesismodel_sensitive.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea"))

summary(aesismodel_sensitive.sem, standardized = TRUE, fit = TRUE, rsq = TRUE)
## insecurity3 still has a negative variance.  It seems that insecurity
## indicators are not consistently related to each other, which causes trouble
cor(AESIS_data[ , paste0("Q6.", 1:33)], method = "kendall")

cor(AESIS_data[ , paste0("Q7.", 1:33)], method = "kendall")

cor(AESIS_data[ , paste0("EIS_", 1:21)], method = "kendall")

cor(AESIS_data[ , paste0("RALESB_", 1:9)], method = "kendall")
cor(AESIS_data[ , paste0("CES_", 1:10)], method = "kendall")
cor(AESIS_data[ , paste0("PSS_", 1:14)], method = "kendall")
cor(AESIS_data[ , paste0("BDHI_IRR", 1:11)], method = "kendall")
cor(AESIS_data[ , paste0("BDHI_RES", 1:8)], method = "kendall")
cor(AESIS_data[ , paste0("BDHI_SUS", 1:10)], method = "kendall")

mod_ind_sens <- modificationIndices(aesismodel_sensitive.sem, power = T)
#Spotting the top 10
head(mod_ind_sens[order(mod_ind_sens$mi, decreasing=TRUE), ], 10)
#And those bigger than 5	
subset(mod_ind_sens[order(mod_ind_sens$mi, decreasing=TRUE), ], mi > 5)
semPaths(aesismodel_sensitive,
         layout='tree2',
         rotation=2,
         nCharNodes = 4)

summary(aesismodel_sensitive.sem)

aesismodel_sensitive2 <- '
Stress  =~  PSS_TOTAL + CES_TOTAL + BDHI_TOTAL
RacialExp =~  RMAS_TOTAL + RALESB_TOTAL + AESIS_Total + EIS_TOTAL + PSS_TOTAL

#General Factor
RacialPressure =~ RacialExp + Stress
RacialPressure  ~~  1*RacialPressure

# regressions using ~ to relate variables
#RacialPressure  ~ ETHNICITY_ord

##Modifications
RMAS_TOTAL	~~	RALESB_TOTAL
BDHI_TOTAL	~~	AESIS_Total
CES_TOTAL	~~	EIS_TOTAL
RALESB_TOTAL	~~	EIS_TOTAL
RMAS_TOTAL	~~	AESIS_Total
PSS_TOTAL	~~	RALESB_TOTAL
'
semPaths(aesismodel_sensitive2, data = aesis_data_core)

aesismodel_sensitive2.cfa <- cfa(aesismodel_sensitive2, data = aesis_data_core)
aesismodel_sensitive2.sem <- sem(aesismodel_sensitive2, data = aesis_data_core)

summary(aesismodel_sensitive2.sem)
fitMeasures(aesismodel_sensitive2.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
semPaths(aesismodel_sensitive2, data = aesis_data_core)

mod_ind_sens2 <- modificationIndices(aesismodel_sensitive2.sem, power = TRUE)
#Spotting the top 10
head(mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], 10)
#And those bigger than 5	
subset(mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], mi > 5)

aesismodel_sensitive3 <- '
Stress  =~  PSS_TOTAL + CES_TOTAL + BDHI_TOTAL
RacialExp =~  RMAS_TOTAL + RALESB_TOTAL + AESIS_Total + EIS_TOTAL + PSS_TOTAL

#General Factor
RacialPressure =~ RacialExp + Stress
RacialPressure  ~~  1*RacialPressure

# regressions using ~ to relate variables
#RacialPressure  ~ ETHNICITY_ord

##Modifications
RMAS_TOTAL	~~	RALESB_TOTAL
#BDHI_TOTAL	~~	AESIS_Total
#CES_TOTAL	~~	EIS_TOTAL
RALESB_TOTAL	~~	EIS_TOTAL
RMAS_TOTAL	~~	AESIS_Total
#PSS_TOTAL	~~	RALESB_TOTAL
'
semPaths(aesismodel_sensitive3, data = aesis_data_core)

aesismodel_sensitive3.cfa <- cfa(aesismodel_sensitive3, data = aesis_data_core)
aesismodel_sensitive3.sem <- sem(aesismodel_sensitive3, data = aesis_data_core)

summary(aesismodel_sensitive3.sem)
fitMeasures(aesismodel_sensitive3.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
semPaths(aesismodel_sensitive3, data = aesis_data_core)

mod_ind_sens2 <- modificationIndices(aesismodel_sensitive3.sem, power = TRUE)
head(mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], 10)#Spotting the top 10
subset(mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], mi > 5)#And those bigger than 5	

fitMeasures(aesismodel_sensitive.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea"))
fitMeasures(aesismodel_sensitive2.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea"))
fitMeasures(aesismodel_sensitive3.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea"))

anova(aesismodel_sensitive2.sem,aesismodel_sensitive3.sem)
RMSEA <- function(chisq, df, N)
{
  chidf <- ifelse( chisq - df > 0, chisq - df, 0)
  sqrt( chidf / (N*df) )
}

#############################
##2nd Order MODEL Two Latents

aesismodel_sensitive1 <- '
Stress  =~  PSS_TOTAL + CES_TOTAL + BDHI_TOTAL
RacialExp =~  RMAS_TOTAL + RALESB_TOTAL + AESIS_Total + EIS_TOTAL

#General Factor
RacialPressure =~ RacialExp + Stress
RacialPressure  ~~  1*RacialPressure

# regressions using ~ to relate variables
##Modifications
'
semPaths(aesismodel_sensitive1, data = aesis_data_core)

aesismodel_sensitive1.sem <- sem(aesismodel_sensitive1, data = aesis_data_core)

summary(aesismodel_sensitive1.sem)
fitMeasures(aesismodel_sensitive1.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

mod_ind_sens2 <- modificationIndices(aesismodel_sensitive1.sem, power = TRUE)
head  (mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], 10)     #Spotting the top 10
subset(mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], mi > 5) #And those bigger than 5	

fitMeasures(aesismodel_sensitive1.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
fitMeasures(aesismodel_sensitive2.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

##Model Modified using MI
aesismodel_sensitive2 <- '
Stress  =~  PSS_TOTAL + CES_TOTAL + BDHI_TOTAL
RacialExp =~  RMAS_TOTAL + RALESB_TOTAL + AESIS_Total + EIS_TOTAL + PSS_TOTAL

#General Factor
RacialPressure =~ RacialExp + Stress
RacialPressure  ~~  1*RacialPressure

# regressions using ~ to relate variables
##Modifications
RMAS_TOTAL	~~	RALESB_TOTAL
BDHI_TOTAL	~~	AESIS_Total
CES_TOTAL	~~	EIS_TOTAL
RALESB_TOTAL	~~	EIS_TOTAL
RMAS_TOTAL	~~	AESIS_Total
PSS_TOTAL	~~	RALESB_TOTAL
'

semPaths(aesismodel_sensitive2, data = aesis_data_core, rotation = 2, residuals = FALSE)
aesismodel_sensitive2.sem <- sem(aesismodel_sensitive2, data = aesis_data_core)
summary(aesismodel_sensitive2.sem)

fitMeasures(aesismodel_sensitive2.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

mod_ind_sens2 <- modificationIndices(aesismodel_sensitive2.sem, power = TRUE)
head  (mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], 10)     #Spotting the top 10
subset(mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], mi > 5) #And those bigger than 5	

fitMeasures(aesismodel_sensitive2.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

anova(aesismodel_sensitive1.sem,aesismodel_sensitive2.sem)

png(filename="images/finalmodel.png", width = 600, height = 600, res = 1200)
semPaths( aesismodel_sensitive2.sem, data = aesis_data_core, layout = "tree2",
          rotation = 1, residuals=F,
          what = "col",
          whatLabels = "std", ask = FALSE, curve = 1.0,
          groups = "latents", curveAdjacent = TRUE, curvePivot = FALSE,
          nCharNodes = 0, nCharEdges = 5,
          sizeMan  = 10, sizeLat  = 10, sizeInt = 10,
          sizeMan2 = 4, sizeLat2 = 5, sizeInt2 = 10,
          thresholds = TRUE,
          edge.label.cex=0.8,
          style = "lisrel", shapeLat = "circle",
          #mar = c(bottom, left, top, right)
          mar = c(3, 3, 3, 5),
          title = TRUE
          )
dev.off()
sink()