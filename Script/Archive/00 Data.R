library(readr)
ccwa14rs <- read_csv("~/Dropbox/College/UMSL/Courses/SP17/Study Group/Quant 3/Exam 2/ccwa14rs.csv")
str(ccwa14rs)



#Data Aggregation
tmp <- aggregate(ccwa14rs$motivate, by=list(ccwa14rs$group), mean)

# by = list() can include multilple layers/levels of nesting
# Note. tmp now has names that need to be changed in order to merge()
names(tmp) <- c("group", "gmotiv")
#Note … you can overwrite existing objects or create a new one … 

ccwa14rs <- merge(tmp, ccwa14rs, by="group")
names(ccwa14rs)
ccwa14rs[1:20,]

####
#   Mean Centering
# Group mean centering.
# Simply subtract group score from individual IF you have a group variable … 

ccwa14rs$gmc.motivate <- ccwa14rs$motivate - ccwa14rs$gmotiv

# Grand mean centering.
attach(ccwa14rs)
ccwa14rs$mcmotivate <- motivate - mean(motivate)
detach(ccwa14rs)
ccwa14rs_bak <- ccwa14rs
ccwa14rs[,4:5] <- lapply(ccwa14rs[,4:5], as.numeric)
str(ccwa14rs)
# Note. If you have missing data, be sure to use mean(motivate, na.rm=T)

#Graphing

#Example 1:
library(lattice) 
png(filename="images/01_plot_grouplines.png")
xyplot(pounds ~ motivate | as.factor(group), data=ccwa14rs, type=c('p', 'r'))
dev.off()

#Example 2:
library(nlme)
tmod<- lmList(pounds~motivate|group,data=ccwa14rs) 
attach(ccwa14rs) 

#open plot
png(filename="images/02_plot_motiv_weight.png")
plot(motivate,pounds,xlab="motivate", ylab="Weight") 
dev.off()

#create function to run all groups 
lmplot<-function(X){ 
  for (i in 1:50){ 
    abline(X[[i]]) 
  }} 

#plot 
lmplot(tmod) 

detach(ccwa14rs)

#Example 3:
# … plotting the predicted model fits for each group

library(nlme)
library(lattice)

lme1 <- lme(pounds~motivate, random=~motivate|group, data=ccwa14rs)

png(filename="images/01_plot_grouplines_predicted.png")
plot(augPred(lme1, primary=~motivate, level=0:1))
dev.off()

nul.mod <- lme(pounds ~ 1, random=~1|group, data=ccwa14rs, na.action =
                 na.omit)
sink(file = "sink/nullmod_summary.txt")
summary(nul.mod)
sink()

# Assumption 1: within group errors
# There are a number of strategies to assess these assumptions.
# (1) compute each of these elements and test each accordingly (a bit tedious)
#   a. resid(my.lme.obj)ormy.lme.obj$residproducesresiduals
resid(lme1) #produces only within group residuals
lme1$resi   #produces fixed and group residuals
#   b. See ?resid.lme
# (2) If using groupedData() objects via nlme package, there are many built in plot
#     functions. However, this adds a level of complexity to data setup.
ccwa14rs <- groupedData(pounds~motivate|group, ccwa14rs)
# (3) plot(my.lme.obj)
png(filename="images/01_plot_lme_std_resid.png")
plot(lme1)
dev.off()
#   a. default will produce a standardized residuals by fitted plot
#   b. see ?plot.lme for help

js.mod1 <- update(js.mod1, data=ccwa14rs) # may need to run js.mod1 from scratch plot(js.mod1)
png(filename="images/jsmod1_residuals.png")
plot(js.mod1, group ~ resid(.))
dev.off()
# alternatively,
png(filename="images/jsmod1_resid__qq.png")
qqnorm(js.mod1, ~resid(.))
dev.off()
# Assumption 2: random effects
png(filename="images/jsmod1_resid__qq_random.png")
qqnorm(js.mod1, ~ranef(.), id=.1)
dev.off()
# id specifies a critical value to identify outliers in the plot.
# Useful for checking normality of distribution of random effects and identifying outliers
# If two or more random effects are present, then ...
#js.mod2 <- update(js.mod1, random=~motivate|group)
#png(filename="images/jsmod1_paired.png")
#pairs(js.mod2, ~ranef(.))
#dev.off()
# Useful for check for assumption of homogeneity of the random effects covariance matrix


#######Empirical Example: Modeled Variance
library(multilevel)
library(nlme)

data(ccwa14rs)
data(ccwa14rs)
js.mod1 <- lme(pounds ~ motivate, random=~1|group, data=ccwa14rs, na.action
               = na.omit)
# Look at the first 20 fitted values
js.mod1$fitted[1:20,]
# Get the squared correlation between individual pounds values and
cor(js.mod1$fitted[,1],ccwa14rs$pounds)^2
#   or,
1-(var(js.mod1$resid[,1])/var(ccwa14rs$pounds))
#Using the ‘formula’ method based on variance components:
nul.mod <- lme(pounds ~ 1, random=~1|group, data=ccwa14rs, na.action =
                 na.omit)
VarCorr(nul.mod) # This is model w/o predictor (i.e., total variance in pounds)
VarCorr(js.mod1) # This is model w/ predictor (i.e., residual variance in pounds)
var(ccwa14rs$pounds)

###########OUTLIERS################
sink(file = "sink/outliers.txt")
#ccwa14rs <- read_csv("~/Dropbox/College/UMSL/Courses/SP17/Study Group/Quant 3/Exam 2/ccwa14rs.csv")
library(QuantPsyc)	# if you do not have this yet, use install.packages(QuantPsyc) or the pull down menu 

norm(ccwa14rs$motivate)
norm(ccwa14rs$pounds)
plotNormX(ccwa14rs$motivate)
plotNormX(ccwa14rs$gmc.motivate)
plotNormX(ccwa14rs$pounds)
plotNormX(ccwa14rs$mcmotivate)

summary(ccwa14rs$gmc.motivate)
multTestccwa14rs <- mult.norm(ccwa14rs[c(4:5)], chicrit = .001)
write.csv(multTestccwa14rs$Dsq, file = "sink/multi_norm.csv")
multTestccwa14rs$Dsq > multTestccwa14rs$CriticalDsq
multTestccwa14rs$CriticalDsq

##outlier removal
#ccwa14rs <- ccwa14rs[-c(51, 121), ]

## which individual variable is skewed - if any?
#for (i in 4:ncol(ccwa14rs)) print(norm(ccwa14rs[,i]))
#apply(ccwa14rs, 2, norm) #this command runs normality against each variable I think

# Just how skewed is ...
png(filename="univ_motivate.png")
eda.uni(ccwa14rs$motivate, title="Motivation")
dev.off()
png(filename="univ_pounds.png")
eda.uni(ccwa14rs$pounds, title="Pounds Lost")
dev.off()
sink()
str(ccwa14rs)
###################################