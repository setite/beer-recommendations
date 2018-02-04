# Correlations/covariances among numeric variables in 
# data frame mtcars. Use listwise deletion of missing data. 
write.csv(cor(AESIS_data[c(29:60,68:73,82:90)], use="complete.obs", method="pearson"), 
          file = "sink/correlation matrix largent.CSV")  
write.csv(cov(AESIS_data[c(29:60)], use="complete.obs"),
          file = "sink/covariance matrix largent_RMAS.CSV")# RMAS 1 to 32
write.csv(cov(AESIS_data[c(68:73)], use="complete.obs"),
          file = "sink/covariance matrix largent_EIS.CSV")# EIS 8 to 13
write.csv(cov(AESIS_data[c(82:90)], use="complete.obs"),
          file = "sink/covariance matrix largent_RALESB.CSV")# RALESB 1 to 9

cov(AESIS_data[c(29:60,68:73,82:90)], use="complete.obs")
cov(AESIS_data[c(29:60)], use="complete.obs")
cov(AESIS_data[c(68:73)], use="complete.obs")
cov(AESIS_data[c(82:90)], use="complete.obs")

cor(AESIS_data[c(6:10,14:15)], use="complete.obs", method="pearson") 
cov(AESIS_data[c(6:10,14:15)], use="complete.obs")
#cov(AESIS_data[c(,paste0("EIS_", 8:13), c(,paste0("RALESB_", 1:9), c(,paste0("RMAS_", 1:32))], use="complete.obs")

aesismodel_sensitive2.sem@vcov


# Correlations with significance levels
library(Hmisc)
rcorr(AESIS_data[c(6:10,14:15)], type="pearson") # type can be pearson or spearman

#mtcars is a data frame 
rcorr(as.matrix(AESIS_data))

# Correlation matrix from mtcars



#####
####Other Types of Correlations
####
# polychoric correlation
# x is a contingency table of counts
library(polycor)
polychor(aesis_data_core[c(29:60,68:73,82:90)]) 
polychor(aesis_data_core[ , paste0("EIS_", 8:13)])
polychor(aesis_data_core[ , paste0("RALESB_", 1:9)])
polychor(aesis_data_core[ , paste0("RMAS_", 1:32)])

# heterogeneous correlations in one matrix 
# pearson (numeric-numeric), 
# polyserial (numeric-ordinal), 
# and polychoric (ordinal-ordinal)
# x is a data frame with ordered factors 
# and numeric variables
library(polycor)
hetcor(aesis_data_core[ , paste0("EIS_", 8:13)])
hetcor(aesis_data_core[ , paste0("RALESB_", 1:9)])
hetcor(aesis_data_core[ , paste0("RMAS_", 1:32)])


# partial correlations
library(ggm)
data(mydata)
pcor(c("a", "b", "x", "y", "z"), var(mydata))
# partial corr between a and b controlling for x, y, z

####Visualizing Correlations
###Use corrgram( ) to plot correlograms .

###Use the pairs() or splom( ) to create scatterplot matrices.

# Basic Scatterplot Matrix
png(filename="images/scatterplot.png")
pairs(~RMAS_TOTAL + RALESB_TOTAL + AESIS_Total + EIS_TOTAL +  PSS_TOTAL + CES_TOTAL + BDHI_TOTAL 
      #ETHNICITY_ord + MaritalStatus + SES + 
      ,
      data=AESIS_data, 
      main="Simple Scatterplot Matrix")
dev.off()
# Scatterplot Matrices from the lattice Package 
#library(lattice)
#splom(AESIS_data2[c(1:12)], data=AESIS_data2, 
      #panel=panel.superpose, 
     # groups=ETHNICITY_ord
#      )

# First Correlogram Example
library(corrgram)
png(filename="images/corrgram.png")
corrgram(aesis_data_core[c(19:50,58:63,72:80)], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram Test")
dev.off()

png(filename="images/corrgram_RMAS.png")
corrgram(aesis_data_core[c(19:50)], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram RMAS")
dev.off()

png(filename="images/corrgram_EIS.png")
corrgram(aesis_data_core[c(58:63)], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram EIS")
dev.off()

png(filename="images/corrgram_RALESB.png")
corrgram(aesis_data_core[c(72:80)], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram RALESB")
dev.off()

str(aesis_data_core[19:50]) # RMAS 1 to 32
str(aesis_data_core[58:63]) # EIS 8 to 13
str(aesis_data_core[72:80]) # RALESB 1 to 9
