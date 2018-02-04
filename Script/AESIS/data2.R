### Terrence D. Jorgensen
### 22 October 2015
### lavaan group, informal consultation:
browseURL("https://groups.google.com/d/msg/lavaan/E8wEWbE77i0/7IyXKFtJBgAJ")


library(lavaan)
sessionInfo()


## import data
aesis_data_temp <- read.table("data/AESIS.csv", header = TRUE, sep = ",")
dim(aesis_data_temp)
tail(aesis_data_temp)
any(sapply(aesis_data_temp, is.na)) # no missing data

all(sapply(aesis_data_temp, is.numeric)) # all are numeric
lapply(aesis_data_temp, table)
## binary: insecurity 1-3, control 2-4
### ORDINAL STUFF
## fewer than 5 ordered categories: social 4-5, demand4
ords <- c(paste0("insecurity", 1:3), paste0("control", 2:4), "demand4",
          paste0("social", 4:5))
ords

## check distributions
omar <- par()$mar
omar
par(mfrow = c(3, 3), mar = c(4, 3, 2, 2))
for (i in names(aesis_data_temp)) hist(aesis_data_temp[ , i], main = i)
par(mfrow = c(1, 1), mar = omar)



## some variances are huge relative to others
sapply(aesis_data_temp, sd)
## I suppose this is why you calcualted alc1 and smk1, which aren't in the data
aesis_data_temp$alc1 <- aesis_data_temp$alc / 20
aesis_data_temp$smk1 <- aesis_data_temp$smk / 8


model151020_3 <- '
emotion =~ emotion1 + emotion2 + emotion3 + emotion4 + emotion5
beh =~ alc1 + smk1
demand =~ demand1 + demand2 + demand3 + demand3 + demand4
emodemand =~ emodemand1 + emodemand2 + emodemand3
ethical =~ ethical1 + ethical2 + ethical3
control =~ control2 + control3 + control4 + control5 + control6
social =~ social1 + social2 + social3 + social4 + social5
insecurity =~ insecurity1 + insecurity2 + insecurity3
'


fit.c <- cfa(model151020_3, data = aesis_data_temp)
## the warning is about a negative variance, and says to inspect this:
lavInspect(fit.c, "theta")
## negative residual variance for insecurity3.  Check scatterplots
car::scatterplotMatrix(aesis_data_temp[ , paste0("insecurity", 1:3)])
## They don't seem to be related, but they are binary, so treating them
## as though they were continuous is uninformative.

fit.o <- cfa(model151020_3, data = aesis_data_temp, ordered = ords)
summary(fit.o, standardized = TRUE, fit = TRUE, rsq = TRUE)
## insecurity3 still has a negative variance.  It seems that insecurity
## indicators are not consistently related to each other, which causes trouble
cor(aesis_data_temp[ , paste0("insecurity", 1:3)], method = "kendall")

## I see the same problems with ethical indicators
cor(aesis_data_temp[ , paste0("ethical", 1:3)], method = "kendall")

## demand4 seems unrelated to the other 3 indicators
cor(aesis_data_temp[ , paste0("demand", 1:4)], method = "kendall")

## emodemand indicators just don't seem very related, so I'd question whether
## they really mention the same thing
cor(aesis_data_temp[ , paste0("emodemand", 1:3)], method = "kendall")



