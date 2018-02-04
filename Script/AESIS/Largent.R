sink(file = "sink/sink_02_sem.txt", type = c("output"))
library(lavaan)
#library(qgraph)
library(sem)
#library(semPlot)
#library(semTools)
#library(psych)
#library(ggplot2)
#library(ISLR)

##2nd Order MODEL Two Latents

aesismodel_1 <- '
EIS =~ EIS_1 + EIS_2 + EIS_3 + EIS_4 + EIS_5 + EIS_6 + EIS_7 + EIS_8 + EIS_9 + EIS_10 + EIS_11 + EIS_12 + EIS_13 + EIS_14 + EIS_15 + EIS_16 + EIS_17 + EIS_18 + EIS_19 + EIS_20 + EIS_21
AESIS_SA =~ AESIS_1 + AESIS_2 + AESIS_3 + AESIS_4 + AESIS_5 + AESIS_6 + AESIS_7 + AESIS_8 + AESIS_9 + AESIS_10 + AESIS_11

AESIS_SA  =~  AESIS_stereotypeAnticipation
AESIS_PIR =~  AESIS_postinteractRum
EIS =~ EIS_TOTAL

EIS ~ AESIS_SA + AESIS_SA*AESIS_PIR
'
semPaths(aesismodel_1, data = AESIS_data)

aesismodel_1.sem <- sem(aesismodel_1, data = aesis_data_core)

summary(aesismodel_1.sem)
fitMeasures(aesismodel_1.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

mod_ind_sens2 <- modificationIndices(aesismodel_1.sem, power = TRUE)
head  (mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], 10)     #Spotting the top 10
subset(mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], mi > 5) #And those bigger than 5	

fitMeasures(aesismodel_1.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
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

anova(aesismodel.sem,aesismodel_sensitive2.sem)

png(filename="images/finalmodel.png", width = 600, height = 600, res = 1200)
semPaths( aesismodel_sensitive2.sem, data = aesis_data_core, layout = "tree2",
          rotation = 1, residuals=F,
          what = "col",
          whatLabels = "std", ask = FALSE, curve = 1.0,
          ETHNICITYs = "latents", curveAdjacent = TRUE, curvePivot = FALSE,
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


)
sink()

library(nlme)
##  Stuff pulled from Bliese
VarCorr(nul.mod)
6.797493/(6.797493+14.639342) #Calculate ICC INT_VAR/(INT_VAR+RESID_VAR)
#ICC 0.3170941
tmod<-aov(EIS_TOTAL~as.factor(ETHNICITY),data=AESIS_data)
ICC1(tmod) #  0.3050035 This ICC looks different enough to worry me
logLik(nul.gls)*-2  # 'log Lik.' 1468.847 (df=2)
logLik(nul.mod)*-2  #''log Lik.' 1429.888 (df=3)
1468.847 - 1429.888 # 38.959
write.csv(anova(nul.gls, nul.mod), file="sink/null_anova.csv")
write_csv(anova(nul.gls, nul.mod), "sink/null_anova_test.csv")
### Those anova results match up, whew


sink(file = "sink/nullmod_summary.txt")

nul.gls <- gls(EIS_TOTAL ~ 1, data=AESIS_data, na.action=na.omit)
nul.mod <- lme(EIS_TOTAL ~ 1, random=~1|ETHNICITY, data=AESIS_data, na.action = na.omit)
summary(nul.mod)
intervals(nul.mod)
VarCorr(nul.mod)
anova(nul.gls, nul.mod)

########

sink()
write.csv(cor(AESIS_data), file = "sink/correlations.csv")
sink(file = "sink/jsmod_summary.txt", type = "output")
#####
#Random Coefficient Regression Model
#####
#** Intercepts random, slopes not ...
js.mod1_t <- lme(EIS_TOTAL ~ AESIS_stereotypeAnticipation, random=~1|ETHNICITY, data=AESIS_data, na.action
                 = na.omit)
# Look at the first 20 fitted values
js.mod1_t$fitted[1:20,]
# Get the squared correlation between individual EIS_TOTAL values and
#** Intercepts and slopes random (vary by ETHNICITY)
js.mod2_t <- lme(EIS_TOTAL ~ AESIS_stereotypeAnticipation + AESIS_postinteractRum, random=~AESIS_postinteractRum|ETHNICITY, data=AESIS_data, na.action=na.omit)
anova(js.mod1_t, js.mod2_t) # deviance test to compare models
summary(js.mod1_t)  # summary of fixed effects
summary(js.mod2_t)  # summary of fixed effects
intervals(js.mod1_t)# 95% CI for fixed effects and variance components
intervals(js.mod2_t)# 95% CI for fixed effects and variance components
VarCorr(js.mod1_t)  # Variance components for the model
VarCorr(js.mod2_t)  # Variance components for the model
#####
#Intercepts as Outcomes Model
#JS ~ AESIS_postinteractRum + gmc.AESIS_postinteractRum
js.mod3_t <- lme(EIS_TOTAL ~ AESIS_stereotypeAnticipation + AESIS_postinteractRum + gmc.AESIS_postinteractRum, random = ~ 1|ETHNICITY, data = AESIS_data,
                 na.action = na.omit)
js.mod4_t <- lme(EIS_TOTAL ~ AESIS_stereotypeAnticipation + AESIS_postinteractRum + gmc.AESIS_postinteractRum, random = ~ AESIS_postinteractRum|ETHNICITY, data = AESIS_data,
                 na.action = na.omit)
summary(js.mod3_t)
VarCorr(js.mod3_t)
####
#Slopes as Outcomes
#JS ~ AESIS_postinteractRum + gmc.AESIS_postinteractRum + AESIS_postinteractRum*gmc.AESIS_postinteractRum

js.mod5_t <- lme(EIS_TOTAL ~ AESIS_Total*RALESB_TOTAL, random = ~ 1|ETHNICITY, data = AESIS_data,
                 na.action = na.omit)
js.mod6_t <- lme(EIS_TOTAL ~ AESIS_stereotypeAnticipation*AESIS_postinteractRum, random = ~ AESIS_stereotypeAnticipation|ETHNICITY, data = AESIS_data,
                 na.action = na.omit)
summary(js.mod5_t)
summary(js.mod6_t)
anova(js.mod5_t,js.mod6_t)
sink()