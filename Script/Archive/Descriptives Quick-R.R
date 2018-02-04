##Descriptive Statistics

##R provides a wide range of functions for obtaining summary statistics. One method of obtaining descriptive statistics is to use the sapply( ) function with a specified summary statistic.

# get means for variables in data frame mydata
# excluding missing values
sapply(beer_df, mean, na.rm=TRUE)

##Possible functions used in sapply include mean, sd, var, min, max, median, range, and quantile.

##There are also numerous R functions designed to provide a range of descriptive statistics at once. For example
sink(file = "sink/desc_test.txt")
library(readr)

sapply(beer_df, mean, na.rm=T)
write_csvy(sapply(beer_df, mean, na.rm=T), "sink/sapply_test.csv")
write.csv(sapply(beer_df, mean, na.rm=F), file = "sink/sapply_test2.csv")

sink()
# mean,median,25th and 75th quartiles,min,max
summary(beer_df)

# Tukey min,lower-hinge, median,upper-hinge,max
fivenum(x)

##Using the Hmisc package

library(Hmisc)
describe(beer_df)
# n, nmiss, unique, mean, 5,10,25,50,75,90,95th percentiles
# 5 lowest and 5 highest scores

##Using the pastecs package

library(pastecs)
stat.desc(mydata)
# nbr.val, nbr.null, nbr.na, min max, range, sum,
# median, mean, SE.mean, CI.mean, var, std.dev, coef.var

##Using the psych package
#sink(file = "sink/desc_test.txt")
library(psych)

describe
write.csv(describe(Base, na.rm=T), file = "sink/psych_describe_base.csv")
write.csv(describe(FU6, na.rm=T), file = "sink/psych_describe_FU6.csv")
write.csv(describe(FU12, na.rm=T), file = "sink/psych_describe_FU12.csv")

# item name ,item number, nvalid, mean, sd,
# median, mad, min, max, skew, kurtosis, se
#sink()

##Summary Statistics by Group

##A simple way of generating summary statistics by grouping variable is available in the psych
##package.

library(psych)
data(beer_df)
sink(file = "sink/describeBy.txt")
describeBy(beer_df, beer_df$beer_style, digits = 3) #just one grouping variable	
#describeBy(beer_df,list(beer_df$gender,beer_df$education))  #two grouping variables
#describeBy(beer_df, mat=TRUE,digits=2, list(beer_df$beer_style,beer_df$beer_abv))  #matrix output
sink()

##The doBy package provides much of the functionality of SAS PROC SUMMARY. It defines the
## desired table using a model formula and a function. Here is a simple example.

library(doBy)
summaryBy(mpg + wt ~ cyl + vs, data = mtcars,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )
# produces mpg.m wt.m mpg.s wt.s for each
# combination of the levels of cyl and vs 