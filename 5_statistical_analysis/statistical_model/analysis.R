library(data.table)
library(MASS)
library(matrixStats)
library(nlme)
library(parallel)
library(ggplot2)
library(bit64)
library(devtools)
library(plm)
library(bit64)
library(dplyr)
library(ggpubr)
options(scipen = 999)
library(tseries)
library(Matrix)
library(lfe)
library(Formula)
library(lfe)
library(panelr)
library(lattice)
library(devtools)
library(plm)
library(Matrix)
library(lfe)
library(Formula)
library(panelr)
options("plm.fast" = TRUE)
library(lme4)
library(lmerTest)
library(HLMdiag)
library(DHARMa)
library(car) #for the Levene test which we will not discuss here
library(Matrix)
library(lmtest)
library(tseries)
library(glmmTMB)
library(bbmle) ## for AICtab
library(ggplot2)
library(dplyr)
library(GLMMadaptive)
library(mgcv)
library(nlme)
library(mgcv)
library(parallel)
library(GLMMadaptive)
data<-read.csv("reviewer_analysis.csv")
data<-subset(data,select=-c(X))
data[is.na(data)] <- 0
data$similarity_score<-as.numeric(data$similarity_score)
data$order<-as.numeric(data$order)
data$type<-factor(data$type,levels=c("unobservable","observable"))
data$recommendation_id<-as.factor(data$recommendation_id)
str(data)
head(data)

# testing with a normal mixed_model first 

m0<-lmer((similarity_score)~type/order+(1|recommendation_id),data=data)
summary(m0)
## testing for normality
qqmath(m0,id=0.05)
## Testing for homogenity of variance
plot(m0)

# histogram
hist(data$similarity_score,xlab="Similarity Score",main="Distribution of similiarity scores",col="darkmagenta")


# implementing beta mixed_model. 

data$y<-((data$similarity_score)+0.99)/2
model1<-mixed_model(y~type+type:order,random=~ 1|recommendation_id,data=data,family=beta.fam())
summary(model1)

