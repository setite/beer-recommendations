########MIS



# five possibilities (Table 6 in Saris, Satorra, van der Veld, 2009) 
#mi.significant <- ifelse( 1 - pchisq(LIST$mi, df=1) < alpha, 
#                          TRUE, FALSE ) 
#high.power <- LIST$power > high.power 
# FIXME: sepc.all or epc?? 
#epc.high <- LIST$sepc.all > LIST$delta 
#epc.high <- LIST$epc > LIST$delta 

#LIST$decision[ which(!mi.significant & !high.power)] <- "(i)" 
#LIST$decision[ which( mi.significant & !high.power)] <- "**(m)**" 
#LIST$decision[ which(!mi.significant &  high.power)] <- "(nm)" 
#LIST$decision[ which( mi.significant &  high.power & 
#                        !epc.high)] <-  "epc:nm" 
#LIST$decision[ which( mi.significant &  high.power & 
#                        epc.high)] <-  "*epc:m*# Simulate data set with \code{mvrnorm} from package \code{\pkg{MASS}}.
require(MASS)
sigma <- matrix(data = c(1, 0.2, 0.2, 0.2, 1, 0.2, 0.2, 0.2, 1), nrow = 3)
complete.data <- mvrnorm(n = 100, mu = c(5, 5, 5), Sigma = sigma)

# Change default matrices as desired
patterns <- result1$patterns
patterns[1:3, 2] <- 0
odds <- result1$odds
odds[2,3:4] <- c(2, 4)
odds[3,] <- c(3, 1, NA, NA)
# Rerun amputation
result3 <- ampute(data = complete.data, patterns = patterns, freq = 
                    c(0.3, 0.3, 0.4), cont = FALSE, odds = odds)
# Run an amputation procedure with continuous probabilities
result4 <- ampute(data = complete.data, type = c("RIGHT", "TAIL", "LEFT"))

md.pattern(aesis_data_aesis)
result1 <- ampute(data = aesis_data_aesis)
result.eis <- ampute(data = aesis_data_eisna)
result.aesis <- ampute(data = aesis_data_aesisna)

aesismodel2 <- '
# regressions using ~ to relate variables
EIS =~ EIS_1 + EIS_2 + EIS_3 + EIS_4 + EIS_5 + EIS_6 + EIS_7 + EIS_8 + EIS_9 + EIS_10 + EIS_11 + EIS_12 + EIS_13 + EIS_14 + EIS_15 + EIS_16 + EIS_17 + EIS_18 + EIS_19 + EIS_20 + EIS_21
AESIS =~ AESIS_1 + AESIS_2 + AESIS_3 + AESIS_4 + AESIS_5 + AESIS_6 + AESIS_7 + AESIS_8 + AESIS_9 + AESIS_10 + AESIS_11
General =~ EIS + AESIS
#COvariances
EIS_3 ~~  EIS_5
EIS_1 ~~  EIS_4
EIS_1 ~~  EIS_2
EIS_2 ~~  EIS_4
EIS_19 ~~ EIS_21
EIS_18 ~~ EIS_21
EIS_20 ~~ EIS_21
EIS_8 ~~  EIS_9
EIS_18 ~~ EIS_20
EIS_18 ~~ EIS_19 
EIS_19 ~~   EIS_20
EIS_1 ~~    EIS_3
EIS_1 ~~    EIS_5
EIS_15 ~~    EIS_17
EIS_4 ~~    EIS_5
EIS_2 ~~    EIS_3
EIS_3 ~~    EIS_4
EIS_2 ~~    EIS_5
EIS_2 ~~    EIS_7
EIS_1 ~~    EIS_7
EIS_3 ~~    EIS_7
EIS_4 ~~    EIS_19
EIS_1 ~~    EIS_19
EIS_3 ~~    EIS_21
EIS_7 ~~    EIS_19
EIS_7 ~~    EIS_20
EIS_16 ~~   EIS_17
EIS_3 ~~    EIS_20
EIS_5 ~~    EIS_7
EIS_1 ~~    EIS_21
EIS_3 ~~    EIS_19
EIS_5 ~~    EIS_21
EIS_10 ~~   EIS_11 #343
EIS_4 ~~    EIS_7
EIS_7	~~	EIS_21
EIS_5	~~	EIS_19
EIS_2	~~	EIS_21
AESIS_4	~~	AESIS_11
EIS_2	~~	EIS_19
EIS_2	~~	EIS_18
EIS_7	~~	EIS_18
EIS_5	~~	EIS_20
EIS_2	~~	EIS_20
EIS_4	~~	EIS_21
EIS_3	~~	EIS_18
EIS_1	~~	EIS_20
EIS_4	~~	EIS_18
EIS_14	~~	EIS_15
EIS_1	~~	EIS_18
EIS_14	~~	EIS_17
EIS_5	~~	EIS_18
EIS_1	~~	EIS_6
EIS_4	~~	EIS_20
EIS_15	~~	EIS_16
EIS_2	~~	EIS_6
AESIS_1	~~	AESIS_2
EIS_4	~~	EIS_6
EIS_11	~~	EIS_21
AESIS_2	~~	AESIS_10
AESIS_6	~~	AESIS_7
#AESIS	=~	EIS_3
EIS_10	~~	EIS_13
EIS_7	~~	AESIS_3
EIS_11	~~	EIS_20
EIS_11	~~	EIS_14
EIS_11	~~	EIS_17
#AESIS	=~	EIS_1
EIS_11	~~	EIS_13
EIS_10	~~	EIS_19
EIS_8	~~	EIS_15
EIS_3	~~	AESIS_3
EIS_17	~~	EIS_20
EIS_10	~~	EIS_21
#AESIS	=~	EIS_21
EIS_17	~~	EIS_19
AESIS_8	~~	AESIS_9
EIS_16	~~	EIS_18
AESIS_2	~~	AESIS_8
#AESIS	=~	EIS_4
AESIS_1	~~	AESIS_3
EIS_8	~~	EIS_17
EIS_9	~~	EIS_16
EIS_11	~~	EIS_19
EIS_9	~~	EIS_15
EIS_21	~~	AESIS_5
EIS_11	~~	EIS_18
EIS_17	~~	EIS_21
EIS_14	~~	EIS_16
EIS_16	~~	EIS_21
EIS_10	~~	EIS_17
AESIS	=~	EIS_2
EIS_15	~~	EIS_19
AESIS_3	~~	AESIS_11
EIS_12	~~	EIS_21
EIS_5	~~	AESIS_3
EIS_16	~~	EIS_19
EIS_2	~~	AESIS_3
EIS_12	~~	EIS_20
EIS_12	~~	EIS_19
AESIS	=~	EIS_7
AESIS	=~	EIS_20
EIS_9	~~	EIS_20
AESIS_1	~~	AESIS_10
EIS_15	~~	EIS_20
AESIS_2	~~	AESIS_5
EIS	=~	AESIS_8
EIS_12	~~	EIS_18
EIS_5	~~	EIS_6
AESIS	=~	EIS_5
EIS_15	~~	EIS_21
EIS_4	~~	EIS_10
EIS_4	~~	AESIS_3
EIS_6	~~	AESIS_8
EIS	=~	AESIS_11
EIS_15	~~	AESIS_9
EIS_1	~~	AESIS_3
EIS_9	~~	EIS_19
EIS_11	~~	EIS_15
EIS_12	~~	EIS_16
EIS_13	~~	EIS_17
AESIS	=~	EIS_18
AESIS_2	~~	AESIS_11
EIS_11	~~	AESIS_11
EIS_2	~~	AESIS_9
EIS_2	~~	EIS_10'


###
aesismodel_sensitive <- '
# regressions using ~ to relate variables
EIS =~ a*EIS_1 + a*EIS_2 + a*EIS_3 + a*EIS_4 + a*EIS_5 + a*EIS_6 + a*EIS_7 + a*EIS_8 + a*EIS_9 + a*EIS_10 + a*EIS_11 + a*EIS_12 + a*EIS_13 + a*EIS_14 + a*EIS_15 + a*EIS_16 + a*EIS_17 + a*EIS_18 + a*EIS_19 + a*EIS_20 + a*EIS_21
AESIS =~ AESIS_1 + AESIS_2 + AESIS_3 + AESIS_4 + AESIS_5 + AESIS_6 + AESIS_7 + AESIS_8 + AESIS_9 + AESIS_10 + AESIS_11
General =~ EIS + AESIS
EIS ~~ AESIS
SES ~ EIS + AESIS
MaritalStatus ~ EIS + AESIS
'

aesismodel_sensitive.sem <- sem(aesismodel_sensitive, data = aesis_data_test)

summary(aesismodel_sensitive.sem)

fitMeasures(aesismodel_sensitive.sem)
fitMeasures(aesismodel_sensitive.sem, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

aesismodel_sensitive.sem <- sem(aesismodel_sensitive, data = aesis_data_test)

semPaths(aesismodel_sensitive, data = aesis_data_test)
semPaths(aesismodel_sensitive.sem, layout='circle2', rotation=1, nCharNodes = 4)

summary(aesismodel_sensitive.sem)


aesismodel2 <- '
# regressions using ~ to relate variables
EIS =~ EIS_1 + EIS_2 + EIS_3 + EIS_4 + EIS_5 + EIS_6 + EIS_7 + EIS_8 + EIS_9 + EIS_10 + EIS_11 + EIS_12 + EIS_13 + EIS_14 + EIS_15 + EIS_16 + EIS_17 + EIS_18 + EIS_19 + EIS_20 + EIS_21
AESIS =~ AESIS_1 + AESIS_2 + AESIS_3 + AESIS_4 + AESIS_5 + AESIS_6 + AESIS_7 + AESIS_8 + AESIS_9 + AESIS_10 + AESIS_11
General =~ EIS + AESIS
General ~~ 1*General
'

aesismodel2.sem <- sem(aesismodel2, data = aesis_data_test)

summary(aesismodel2.sem)	

fitMeasures(aesismodel2.sem)
fitMeasures(aesismodel2.sem, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

aesismodel2.sem <- sem(aesismodel2, data = aesis_data_test)

semPaths(aesismodel2, data = aesis_data_test)
semPaths(aesismodel2.sem, layout='circle2', rotation=1, nCharNodes = 4)

summary(aesismodel2.sem)

names(KJudd)[1] <- "Y1"
names(KJudd)[2] <- "X1"
names(KJudd)[3] <- "X2"
names(KJudd)[4] <- "Z1"
names(KJudd)[5] <- "Z2"

##LVS Model
lvsmod1 <-'Y =~ Y1
X =~ X1 + X2
Z =~ Z1 + Z2'

lvsmodsem <- lavaan::sem(lvsmod1, data = KJudd)
semPaths(lvsmod1, data = KJudd)
summary(lvsmodsem)

KJuddLVS <- predict(lvsmod)

KJuddLVS <- predict(lvsmodsem)
KJuddLVSData <- as.data.frame(KJuddLVS)

##LVS Model 2
lvsmod2 <-'y =~ x*z '

lvsmod2sem <- lavaan::sem(lvsmod2, data = KJuddLVSData, std.lv = FALSE)
semPaths(lvsmod2, data = KJudd)
summary(lvsmod2sem)

KJuddLVSData$xz <- KJuddLVSData$x*KJuddLVSData$z

##LVS Model 2
lvsmod3 <-'y =~ x + z + xz'

lvsmod3sem <- lavaan::sem(lvsmod3, data = KJuddLVSData, std.lv = FALSE)
semPaths(lvsmod3, data = KJudd)
summary(lvsmod3sem)

anova(lvsmodsem, lvsmod2sem, lvsmod3sem)

################### calculate alpha
summary(mod1)
lam <- sum(c(1, .984, .933, .805))
td <- sum(c(.254, .278, .351, .516))
lam^2/(lam^2 +td)
alpha(KJudd)
alpha(KJuddLVSData)


aesismoda <- '
## Measurement models
ksi1 =~ X1 + X2
eta1 =~ Y1 + Y2
eta2 =~ Y3 + Y4

## Regressions
eta2 ~ eta1 + ksi1
eta1 ~ ksi1
'

aesismodb <- '
## Measurement models
ksi1 =~ X1 + X2
eta1 =~ Y1 + Y2
eta2 =~ Y3 + Y4

## Regressions
eta2 ~ eta1
eta1 ~ ksi1 + eta2
'

aesisfita <- sem(aesismoda, sample.cov=aesiscov, sample.nobs=100)	
aesisfitb <- sem(aesismodb, sample.cov=aesiscov, sample.nobs=100)	



qgraph.lavaan(aesisfita, layout='spring', filetype="", include=3)

## Example simple mediation model ... 

## uses only 3 variables INTELLNC, GRADES, OCCUASP

aesisMed1 <- '

# Mediated model as described
OCCUASP ~ b*GRADES + cp*INTELLNC
GRADES ~ a*INTELLNC

# Indirect Effects - model as specified
ab := a*b

# total effect
total := cp + (a*b)

'

aesismodel1.sem <- sem(exampleMed1, data = aesis_data, sample.nobs=767)	

summary(aesismodel1.sem)

## fixing direct path to test for full vs. partial mediation

exampleMed2 <- '

# Mediated model as described
OCCUASP ~ b*GRADES 
GRADES ~ a*INTELLNC

# Indirect Effects - model as specified
ab := a*b

'
exampleMedFit2 <- sem(exampleMed2, data = aesis_data, sample.nobs=767)

#Write a congeneric model

##CONGENERIC MODEL, all paths allowed to be freely estimated
conmod1 <-'Visual =~ X1 + X2 +X3 
Verbal=~ X4 + X5 +X6
Speed=~ X7 + X8 + X9'

conmodsem <- sem(conmod1, data = aesis_data)
semPaths(conmod1, data = aesis_data)

summary(conmodsem)

##tau MODEL, all paths allowed to be freely estimated
taumod1 <-'Visual =~ a*X1 + a*X2 + a*X3 
Verbal=~ a*X4 + a*X5 + a*X6
Speed=~ a*X7 + a*X8 + a*X9'

taumodsem <- sem(taumod1, data = aesis_data)

summary(taumodsem)

##parallel MODEL, all paths allowed to be freely estimated
parmod1 <-'Visual =~ a*X1 + a*X2 + a*X3 
Verbal=~ a*X4 + a*X5 + a*X6
Speed=~ a*X7 + a*X8 + a*X9
X1 ~~ e1*X1
X2 ~~ e1*X2
X3 ~~ e1*X3
X4 ~~ e1*X4
X5 ~~ e1*X5
X6 ~~ e1*X6
X7 ~~ e1*X7
X8 ~~ e1*X8
X9 ~~ e1*X9'

parmodsem <- sem(parmod1, data = aesis_data)
semPaths(parmod1, data = aesis_data)

summary(parmodsem)
aesis_data$EIS_T
aesismodel1 <- '
# regressions using ~ to relate variables
AESIS_Total ~ EIS_TOTAL + AESIS_stereotypeAnticipation*EIS_TOTAL
'
#Note that a1,a2,b1,b2, and c are labels. Then run the model:
fit <- sem(aesismodel1, data=aesis_data_core)
#And look at output:
summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)
#Finally, generate bootstrap confidence intervals:
boot.fit <- parameterEstimates(fit, boot.ci.type="bca.simple")
summary(boot.fit)
aesismodel1.sem <- sem(aesismodel1, data = aesis_data)

summary(aesismodel1.sem)	

fitMeasures(aesismodel1.sem)
fitMeasures(aesismodel1.sem, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

aesismodel1.sem <- sem(aesismodel1, data = aesis_data)
semPaths(aesismodel1, data = aesis_data)
semPaths(aesismodel1.sem, layout='circle2', rotation=1, nCharNodes = 4)

summary(aesismodel1.sem)

mod_ind <-   modificationIndices(aesismodel1.sem)
modindices(aesismodel1.sem)
#Spoting the top 10
head(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], 10)
#And the bigger than 5	
subset(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], mi > 5)


# Correlation matrix from mtcars
# with mpg, cyl, and disp as rows 
# and hp, drat, and wt as columns 
x <- aesis_data[c("BDHI_RES_TOTAL")]
y <- aesis_data[c("BDHI_SUS_TOTAL")]
cor(aesis_data$BDHI_IRR_TOTAL, aesis_data$AESIS_postinteractRum, use = "complete.obs")
cor(aesis_data$BDHI_SUS_TOTAL, aesis_data$AESIS_postinteractRum, use = "complete.obs")
'# outcome model 
outcomeVar ~ c*xVar + b1*medVar1 + b2*medVar2

# mediator models
medVar1 ~ a1*xVar 
medVar2 ~ a2*xVar

# indirect effects (IDE)
medVar1IDE  := a1*b1
medVar2IDE  := a2*b2
sumIDE := (a1*b1) + (a2*b2)

# total effect
total := c + (a1*b1) + (a2*b2)
medVar1 ~~ medVar2 # model correlation between mediators'



############
###
aesismodel_sensitive <- '
Sensitivity =~ AESIS_Total + EIS + RMAS_TOTAL + ETHNICITY_ord + BDHI_TOTAL + AESIS_stereotypeAnticipation
EIS =~ EIS_TOTAL
AESIS     =~  AESIS_stereotypeAnticipation + AESIS_postinteractRum
#General Factor
#General  =~  AESIS + EIS
#AESIS  ~~  1*AESIS_Total

#AESIS ~ EIS + SES + MaritalStatus
#MaritalStatus ~ EIS + AESIS

# regressions using ~ to relate variables
AESIS  ~ ETHNICITY_ord +  MaritalStatus + SES + EIS + BDHI_RES_TOTAL + BDHI_SUS_TOTAL + RALESB_TOTAL
#AESIS     ~   GENDER + AGE
##Modifications

#Reliabilities for single indicators
#AESIS_Total                   ~~ (1-.92)*AESIS_Total
#AESIS_postinteractRum         ~~ (1-.90)*AESIS_postinteractRum
#AESIS_stereotypeAnticipation  ~~ (1-.78)*AESIS_stereotypeAnticipation
#Perceived Racism-Self subscale being .90, and for the Perceived-Racism Group subscale being .83
'
aesismodel_sensitive.sem <- sem(aesismodel_sensitive, data = aesis_data_core)

summary(aesismodel_sensitive.sem)

fitMeasures(aesismodel_sensitive.sem)
fitMeasures(aesismodel_sensitive.sem, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

semPaths(aesismodel_sensitive, data = aesis_data_core)
semPaths(aesismodel_sensitive, layout='circle2', rotation=1, nCharNodes = 4)

summary(aesismodel_sensitive.sem)

aesismodel_sensitive <- '
#Sensitivity =~  RMAS_TOTAL + EIS_TOTAL + BDHI_SUS_TOTAL + AESIS_stereotypeAnticipation 
RacialPressure =~ PSS_TOTAL + CES_TOTAL + AESIS_postinteractRum
#General Factor
#RacialPressure  ~~  1*RacialPressure
PSS_TOTAL ~~ CES_TOTAL
# regressions using ~ to relate variables
#Rumination  ~ ETHNICITY_ord + MaritalStatus*SES + RALESB_TOTAL
RacialPressure  ~ AESIS_Total + ETHNICITY_ord +
RALESB_TOTAL + RMAS_TOTAL + EIS_TOTAL + BDHI_SUS_TOTAL 
#AESIS     ~   GENDER + AGE
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

##2nd Order MODEL Latent TEST with total scores
secmod1 <-'
AESIS_SA =~ AESIS_stereotypeAnticipation
AESIS_PI =~ AESIS_postinteractRum
#EIS =~ EIS_TOTAL
#AESIS_SA ~~ AESIS_PI
#General =~ AESIS + EIS
#General ~~ 1*General
AESIS_postinteractRum ~ SES + ETHNICITY_ord + RMAS_TOTAL + BDHI_RES_TOTAL + CES_TOTAL
##Modifications
'

secmod1.sem <- sem(secmod1, data = aesis_data_core)
semPaths(secmod1, data = aesis_data_core)
#sink('sink/secmod1_summary.txt', append = T)
summary(secmod1.sem)
#sink()
#secmod1.sem <- sem(secmod1, data = aesis_data_core)


fitMeasures(secmod1.sem, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

mod_ind_mod1 <- modificationIndices(secmod1.sem, power = FALSE)
#Spoting the top 10
head(mod_ind_mod1[order(mod_ind_mod1$mi, decreasing=TRUE), ], 10)
#And the bigger than 5	
subset(mod_ind_mod1[order(mod_ind_mod1$mi, decreasing=TRUE), ], mi > 5)
aesis_data_core$RAL

secmod3 <-'
AESIS_SA =~  AESIS_stereotypeAnticipation
AESIS_PI  =~  AESIS_postinteractRum
AESIS_SA ~~ AESIS_PI
#General  =~  AESIS + EIS
#General  ~~  1*General
AESIS_PI  ~   ETHNICITY_ord + MaritalStatus + RALESB
AESIS ~ BDHI_RES_TOTAL + BDHI_SUS_TOTAL + EIS_TOTAL + RALESB_TOTAL + RMAS_TOTAL

#AESIS     ~   GENDER + AGE
##Modifications
#GENDER   ~   AESIS_PI + AESIS_SA

#Reliabilities for single indicators
#AESIS_Total                   ~~ (1-.92)*AESIS_Total
#AESIS_postinteractRum         ~~ (1-.90)*AESIS_postinteractRum
#AESIS_stereotypeAnticipation  ~~ (1-.78)*AESIS_stereotypeAnticipation
#Perceived Racism-Self subscale being .90, and for the Perceived-Racism Group subscale being .83
'

secmod3.sem <- sem(secmod3, data = aesis_data_core)
semPaths(secmod3, data = aesis_data_core)
summary(secmod3.sem)
#secmod3.sem <- sem(secmod3, data = aesis_data_core)

summary(secmod3.sem)

#fitMeasures(secmod3.sem)
fitMeasures(secmod3.sem, c("chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

mod_ind_mod3 <- modificationIndices(secmod3.sem, power = FALSE)
#Spoting the top 10
head(mod_ind_mod3[order(mod_ind_mod3$mi, decreasing=TRUE), ], 10)
#And the bigger than 5	
subset(mod_ind_mod3[order(mod_ind_mod3$mi, decreasing=TRUE), ], mi > 5)

aesismodel_sensitive3 <- '
Stress  =~  CES_TOTAL + PSS_TOTAL + BDHI_TOTAL
RacialExp =~  RMAS_TOTAL + RALESB_TOTAL + AESIS_Total + EIS_TOTAL + PSS_TOTAL

#General Factor
RacialPressure =~ RacialExp + Stress
RacialPressure  ~~  1*RacialPressure

# regressions using ~ to relate variables
##Modifications
'
semPaths(aesismodel_sensitive3, data = aesis_data_core, residuals = FALSE, rotation = 1, nCharNodes = 6)

aesismodel_sensitive3.sem <- sem(aesismodel_sensitive3, data = aesis_data_core)

summary(aesismodel_sensitive3.sem)
fitMeasures(aesismodel_sensitive3.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
fitMeasures(aesismodel_sensitive.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea"))
fitMeasures(aesismodel_sensitive2.sem, c("AIC","BIC", "chisq", "df", "pvalue", "cfi", "nnfi", "rmsea"))

anova(aesismodel_sensitive2.sem,aesismodel_sensitive3.sem)
anova(aesismodel_sensitive.sem,aesismodel_sensitive2.sem)

AESIS_data
summary(fit.o, standardized = TRUE, fit = TRUE, rsq = TRUE)
## insecurity3 still has a negative variance.  It seems that insecurity
## indicators are not consistently related to each other, which causes trouble
cor(AESIS_data[ , paste0("AESIS_", 1:13)], method = "kendall")

## I see the same problems with ethical indicators
cor(AESIS_data[ , paste0("RMAS_", 1:32)], method = "kendall")

## demand4 seems unrelated to the other 3 indicators
cor(AESIS_data[ , paste0("EIS_", 1:21)], method = "kendall")

## emodemand indicators just don't seem very related, so I'd question whether
## they really mention the same thing
cor(AESIS_data[ , paste0("RALESB_", 1:9)], method = "kendall")
cor(AESIS_data[ , paste0("CES_", 1:10)], method = "kendall")
cor(AESIS_data[ , paste0("PSS_", 1:14)], method = "kendall")
cor(AESIS_data[ , paste0("BDHI_IRR", 1:11)], method = "kendall")
cor(AESIS_data[ , paste0("BDHI_RES", 1:8)], method = "kendall")
cor(AESIS_data[ , paste0("BDHI_SUS", 1:10)], method = "kendall")

semPaths(aesismodel_sensitive3, data = aesis_data_core)
mod_ind_sens2 <- modificationIndices(aesismodel_sensitive3.sem, power = FALSE)
#Spoting the top 10
head(mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], 10)
#And the bigger than 5	
subset(mod_ind_sens2[order(mod_ind_sens2$mi, decreasing=TRUE), ], mi > 5)