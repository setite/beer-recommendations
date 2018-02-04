library(foreign)
library(haven)

SJT_data <- read.csv('data/SJT+Project+Master+List.csv',      header = TRUE, na = "empty")
SJT_data <- read.csv('data/SJT+Project+Master+List copy.csv', header = TRUE, na = "empty")
X2 <- read.csv('data/X2.csv', header = TRUE, na = "empty")



SJT_data_complete <- SJT_data
#writing to CSV for better reimportation
str(SJT_data)

##################################################
#####   Random Coefficient Regression Model   ####
##################################################
SJT_data <- SJT_data_complete[c(33:44,46:57,59:69,71:134)]
SJT_SAPA <- SJT_data_complete[c(33,35:36,38,40:42,44,46,50,60,62:69,71:101)]
SJT_data2 <- na.omit(SJT_data)
SJT_data_short <- SJT_data_complete[c(33:44,46:57,59:69,71:134)]
SJT_data_short <- na.omit(SJT_data)
SJT_data <- lapply(SJT_data, as.numeric)

##################################################
#####   Random Coefficient Regression Model   ####
##################################################
summary(SJT_data$Q3.8)
summary(SJT_data$Q3.12)
summary(SJT_data$Q5.7)
library(car)
SJT_data_complete$Q3.8 <- recode(SJT_data_complete$Q3.8, "1 = 6; 2 = 5; 3 = 4;  4 = 3; 5 = 2; 6 = 1; as.factor.result = FALSE")  #should recode already - kinda doesn't work
SJT_data_complete$Q3.12 <- recode(SJT_data_complete$Q3.12, "1 = 6; 2 = 5; 3 = 4;  4 = 3; 5 = 2; 6 = 1; as.factor.result = FALSE")  #should recode already - kinda doesn't work
SJT_data_complete$Q5.7 <- recode(SJT_data_complete$Q5.7, "1 = 6; 2 = 5; 3 = 4;  4 = 3; 5 = 2; 6 = 1; as.factor.result = FALSE")  #should recode already - kinda doesn't work
# variable v1 is coded 1, 2 or 3
summary(SJT_data$Q3.8)
summary(SJT_data$Q3.12)
summary(SJT_data$Q5.7)


aesis_data_core$ETHNICITY_ord <- factor(aesis_data_core$ETHNICITY_ord,
                                        levels = c(1,2,3,4,5),
                                        labels = c("White", "Asian", "Misc", "Hispanic", "Black"))
aesis_data_core$ETHNICITY_ord <- lapply(aesis_data_core$ETHNICITY_ord,
                                        as.integer,
                                        ordered)
str(SJT_data)

sink(file = "sink/demographics.txt", type = c("output"))
demographics <- read.csv('data/Demographics.csv', header = TRUE, na = "empty")
summary(demographics)
sink()

cor(AESIS_data[ , paste0("AESIS_", 1:13)], method = "kendall")
cor(AESIS_data[ , paste0("RMAS_", 1:32)], method = "kendall")
cor(AESIS_data[ , paste0("EIS_", 1:21)], method = "kendall")
cor(AESIS_data[ , paste0("RALESB_", 1:9)], method = "kendall")
cor(AESIS_data[ , paste0("CES_", 1:10)], method = "kendall")
cor(AESIS_data[ , paste0("PSS_", 1:14)], method = "kendall")
cor(AESIS_data[ , paste0("BDHI_IRR", 1:11)], method = "kendall")
cor(AESIS_data[ , paste0("BDHI_RES", 1:8)], method = "kendall")
cor(AESIS_data[ , paste0("BDHI_SUS", 1:10)], method = "kendall")
###normalization
SJT_data <- SJT_data
SJT_data$RMAS_TOTAL    <- Normalize(SJT_data$RMAS_TOTAL)
SJT_data$EIS_TOTAL     <- Normalize(SJT_data$EIS_TOTAL)
SJT_data$RALESB_TOTAL  <- Normalize(SJT_data$RALESB_TOTAL)
SJT_data$CES_TOTAL     <- Normalize(SJT_data$CES_TOTAL)
SJT_data$PSS_TOTAL     <- Normalize(SJT_data$PSS_TOTAL)
SJT_data$BDHI_TOTAL    <- Normalize(SJT_data$BDHI_TOTAL)
SJT_data$AESIS_Total   <- Normalize(SJT_data$AESIS_Total)

str(SJT_data)

library(QuantPsyc)	# if you do not have this yet, use install.packages(QuantPsyc) or the pull down menu 

norm(SJT_data$BDHI_SUS_TOTAL)
multTest_SJT_data <- mult.norm(SJT_data)
plotNormX(SJT_data$BDHI_SUS_TOTAL)

#omit NAs so imputation can be done
SJT_data2 <- SJT_data[c(4:15)]
aesis_data_aesis <- 
  SJT_data[!is.na(SJT_data)]
aesis_data_eis <- aesis_data_eis[!is.na(aesis_data_eis)]
View(aesis_data_eis)
#aesis_data_aesisna <- max(na.omit(aesis_data_aesis))
#aesis_data_eisna <- max(na.omit(aesis_data_eis))

library(Amelia)
missmap(SJT_data, main = "Missing values vs observed")
library(BaylorEdPsych)
write.csv(LittleMCAR(SJT_data_short[c(1:50)]), file = "sink/MCAR.csv")

LittleMCAR(SJT_data_short[c(1:50)])
LittleMCAR(SJT_data)
LittleMCAR()

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)

#create mean data
m <- data.frame(matrix(sample(100, 62, replace = TRUE), ncol = 2))
m$X1 <- sapply(SJT_data[c(36:66)], mean, na.rm = T)  # Returns a vector
m$X2 <- sapply(SJT_data[c(69:99)], mean, na.rm = T)  # Returns a vector
m1 <- lapply(SJT_data[c(36:66)], mean, na.rm = T)  # Returns a list  
m2 <- lapply(SJT_data[c(69:99)], mean, na.rm = T)  # Returns a list  

##T Test
t.test(m$X1, m$X2, paired=TRUE)
library(psych)
omega(SJT_data2, nfactors = 3) 

colMeans(SJT_data[c(36:66, 69:99)], na.rm = TRUE)
fit <- aov(y ˜ A + Error(subject), data=MD11.5.long)
summary(fit)

