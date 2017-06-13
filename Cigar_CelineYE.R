# install packages and load libraries
install.packages("psych")
install.packages("nlme")
library(psych)
library(mgcv)
install.packages("zoo")
library(lmtest)

# Time Series Plot for Two Variables
# x is in solid line and y is in dotted line
# this function is courtesy of Peter Yin
ftsplot2 <- function(x,y){plot(x,type="l",lty=1); lines(y,lty=2)}


# import data set
cigar <- read.csv("C:/Users/aden/Dropbox/cf/DS633/project/Cigar.csv")
dim(cigar)

cgdf<-cigar
names(cgdf)
names(cgdf)[1]<-paste("obs")

# small data set(time series)
smdf<-cgdf[cgdf$state==32,]
dim(smdf)

names(smdf)

# Histgram of all variables
par(mfcol=c(3,3))
hist(smdf$obs,main="Fig.1 Hist of obs",xlab="obs")
hist(smdf$year,main="Fig.2 Hist of year",xlab="year")
hist(smdf$price,main="Fig.3 Hist of price",xlab="price")
hist(smdf$pop,main="Fig.4 Hist of population",xlab="pop")
hist(smdf$pop16,main="Fig.5 Hist of population above 16",xlab="pop16")
hist(smdf$cpi,main="Fig.6 Hist of cpi",xlab="cpi")
hist(smdf$ndi,main="Fig.7 Hist of ndi",xlab="ndi")
hist(smdf$pimin,main="Fig.8 Hist of pimin",xlab="pimin")
hist(smdf$sales,main="Fig.9 Hist of sales",xlab="sales")

par(mfcol=c(3,3))
plot.ts(smdf$obs);   title("Fig.10 Tsplot of obs")
plot.ts(smdf$year);   title("Fig.11 Tsplot of year")
ts.plot(smdf$price);	title("Fig.12 Tsplot of price")
ts.plot(smdf$pop); 	title("Fig.13 Tsplot of pop")
ts.plot(smdf$pop16); 	title("Fig.14 Tsplot of pop16")
ts.plot(smdf$cpi);	  title("Fig.15 Tsplot of cpi")
ts.plot(smdf$ndi); 	  title("Fig.16 Tsplot of ndi")
ts.plot(smdf$pimin); 	  title("Fig.17 Tsplot of pimin")
ts.plot(smdf$sales); 	title("Fig.18 Tsplot of sales")


# scatter plot
par(mfcol=c(3,3))
plot(smdf$year,smdf$sales,main="Fig.19 year v sales",xlab="year",ylab="sales")  
plot(smdf$price,smdf$sales,main="Fig.20 price v sales",xlab="price",ylab="sales") 
plot(smdf$pop,smdf$sales,main="Fig.21 pop v sales",xlab="pop",ylab="sales")
plot(smdf$pop16,smdf$sales,main="Fig.22 pop16 v sales",xlab="pop16",ylab="sales")
plot(smdf$cpi,smdf$sales,main="Fig.23 cpi v sales",xlab="cpi",ylab="sales")     
plot(smdf$ndi,smdf$sales,main="Fig.24 ndi v sales",xlab="ndi",ylab="sales")
plot(smdf$pimin,smdf$sales,main="Fig.25 pimin v sales",xlab="pimin",ylab="sales")


par(mfcol=c(3,3))
scatter.smooth(smdf$year,smdf$sales, main="Fig.19 year v sales",xlab="year",ylab="sales")  
scatter.smooth(smdf$price,smdf$sales, main="Fig.20 price v sales",xlab="price",ylab="sales")
scatter.smooth(smdf$pop,smdf$sales, main="Fig.21 pop v sales",xlab="pop",ylab="sales")
scatter.smooth(smdf$pop16,smdf$sales, main="Fig.22 pop16 v sales",xlab="pop16",ylab="sales")
scatter.smooth(smdf$cpi,smdf$sales, main="Fig.23 cpi v sales",xlab="cpi",ylab="sales")     
scatter.smooth(smdf$ndi,smdf$sales, main="Fig.24 ndi v sales",xlab="ndi",ylab="sales")
scatter.smooth(smdf$pimin,smdf$sales, main="Fig.25 pimin v sales",xlab="pimin",ylab="sales")

# Table 1 Descriptive Statistics
describe(smdf[,c("price","pop","pop16","cpi","ndi","pimin","sales")])

#Table 2 Correlation matrix
round(cor(smdf[,c("price","pop","pop16","cpi","ndi","pimin","sales")]),4)

###############################################################################
#linear model
fit<-lm(sales~price+cpi+ndi+pop16+year,data=smdf)
summary(fit)
dwtest(fit)    # need library lmtest

rdf<-data.frame(smdf,pred=fit$fitted.values,resid=fit$residuals)
par(mfcol=c(2,2))
hist(rdf$resid)
plot(rdf$pred,rdf$sales);  abline(lm(sales~pred,data=rdf))
ftsplot2(rdf$sales,rdf$pred)
ts.plot(rdf$resid); abline(h=0)

par(mfcol=c(1,1));
ftsplot2(rdf$sales,rdf$pred)

##############################################################

fit1<-lm(log(sales)~price+cpi+ndi+pop16+year,data=smdf)
summary(fit1)
dwtest(fit1)    

rdf1<-data.frame(smdf,pred=exp(fit1$fitted.values),resid=fit1$residuals)
par(mfcol=c(2,2))
hist(rdf1$resid,main="Fig.26 Hist of residuals",xlab="resid")
plot(rdf1$pred,rdf1$sales,main="Fig.27 pred v actual");  abline(lm(sales~pred,data=rdf1))
plot(rdf1$pred,rdf1$resid,main="Fig.28 pred v resid",xlab="pred",ylab="resid")  
ts.plot(rdf1$resid,main="Fig.28 Time series plot-residual"); abline(h=0)

par(mfcol=c(1,1));
ftsplot2(rdf1$sales,rdf1$pred)

###########################################################

#gam - nonlinear, nonparametric regression
names(smdf)
gamFit<-gam(sales~s(price)+s(pop16)+s(ndi),data=smdf)
summary(gamFit)

par(mfcol=c(2,2)); plot.gam(gamFit)
rdf2<-data.frame(smdf,p=predict(gamFit),r=resid(gamFit))

cor(rdf2$p,rdf2$sales)^2

par(mfcol=c(2,2))
hist(rdf2$r)
plot(rdf2$p,rdf2$sales);abline(lm(sales~pred,data=rdf))
ftsplot2(rdf2$sales,rdf2$p)
plot.ts(rdf2$r); abline(h=0)
par(mfcol=c(1,1))
ftsplot2(rdf2$sales,rdf2$p)

