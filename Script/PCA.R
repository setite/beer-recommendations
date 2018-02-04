library(psych)

sink(file = "sink/REPORTS.txt", type = c("output"))
str(beer_df_core[c(3:5,7:8,10)])
#str(beer_df_core[c(1,3:5,7:8,10:11)])

summary(beer_df_core[c(3:5,7:8,10)])

alpha(beer_df_core[c(3:5,7:8,10)])
#alpha(beer_df_core[c(1,3:5,7:8,10:11)])

omega(beer_df_core[c(3:5,7:8,10)], nfactors = 3)
#omega(beer_df_core[c(1,3:5,7:8,10:11)], nfactors = 5)


sink()
png(filename="sink/omega.png", 
    #units="in", 
    #width=5, 
    #height=4, 
    #pointsize=12, 
    res=100)
omega(beer_df_core[c(3:5,7:8,10)], nfactors = 3)
dev.off()

# Varimax Rotated Principal Components
# retaining 5 components 
library(psych)
fit <- principal(beer_df_short, nfactors=5, rotate="varimax")
fit # print results


omega(m,nfactors=3,fm="minres",n.iter=1,p=.05,poly=FALSE,key=NULL,
      flip=TRUE,digits=2, title="Omega",sl=TRUE,labels=NULL,
      plot=TRUE,n.obs=NA,rotate="oblimin",Phi=NULL,option="equal",covar=FALSE, ...)
omegaSem(m,nfactors=3,fm="minres",key=NULL,flip=TRUE,digits=2,title="Omega",
         sl=TRUE,labels=NULL, plot=TRUE,n.obs=NA,rotate="oblimin",
         Phi = NULL, option="equal",lavaan=TRUE,...)

omegah(m,nfactors=3,fm="minres",key=NULL,flip=TRUE, 
       digits=2,title="Omega",sl=TRUE,labels=NULL, plot=TRUE,
       n.obs=NA,rotate="oblimin",Phi = NULL,option="equal",covar=FALSE,...) 

omegaFromSem(fit,m=NULL,flip=TRUE,plot=TRUE)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(beer_df_short, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)


library(ggplot2)
library(ISLR)
beer_df_2 <- beer_df

beer_df<-beer_df[,c(1,2,3,4,5,6,8,9,7,10,11)]
beer_df_2$brewery_id<-NULL
beer_df_2$brewery_name<-NULL

beer_df.pca<-prcomp(beer_df[,4:6],scale. = T,center = T)
beer_df.pca<-princomp(beer_df[,1:7],scale. = T,center = T)

summary(beer_df.pca)

## Importance of components:
##                           PC1    PC2    PC3    PC4    PC5     PC6     PC7
## Standard deviation     1.3315 1.1907 1.0743 0.9893 0.9260 0.80506 0.41320
## Proportion of Variance 0.2533 0.2026 0.1649 0.1398 0.1225 0.09259 0.02439
## Cumulative Proportion  0.2533 0.4558 0.6207 0.7605 0.8830 0.97561 1.00000

scores<-as.data.frame(beer_df.pca$)
pcaplot<-ggplot(scores,(aes(PC1,PC2,color=beer_df$ShelveLoc)))+geom_point()
pcaplot

require(graphics)

## The variances of the variables in the
## USArrests data vary by orders of magnitude, so scaling is appropriate
(pc.cr <- princomp(beer_df, cor = TRUE))  # inappropriate
screeplot(pc.cr)


fit <- princomp(beer_df)
screeplot(fit)
screeplot(fit, npcs = 24, type = "lines")
screeplot(fit2, type = "lines")
#princomp(x, ...)

## S3 method for class 'formula'
fit2 <- princomp(~ ., data = beer_df)
screeplot(fit2)
## Default S3 method:
princomp(x, cor = FALSE, scores = TRUE, covmat = NULL,
         subset = rep_len(TRUE, nrow(as.matrix(x))), ...)

## S3 method for class 'princomp'
predict(object, newdata, ...)