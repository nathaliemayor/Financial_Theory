install.packages("xts")
install.packages("ggplot2")
install.packages("fPortfolio")
install.packages("quantmod")
install.packages("tidyverse")
install.packages("tidyquant")
install.packages("timetk")
install.packages("tseries")
install.packages("scales")
install.packages("PerformanceAnalytics")
install.packages("fPortfolio")
install.packages("zoo")
install.packages("foreach")
install.packages("nloptr")
install.packages("corpcor")
install.packages(c("GenSa","pso","ROI.plugin.glpk","ROI.plugin.quadprog","ROI","fGarch","DEoptim"))
install.packages(c("GenSA","ROI.plugin.symphony"))
install.packages("dplyr")
install.packages("plyr")
install.packages("data.table")
install.packages("BBmisc")
install.packages("timetk")
install.packages("ppls")
install.packages("https://cran.r-project.org/src/contrib/Archive/ppls/ppls_1.6-1.1.tar.gz", repos = NULL)
install.packages("matrix")
install.packages("gdata")
install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
install.packages("NMOF")
library(NMOF)
library(IntroCompFinR)
library(gdata)
library(Matrix)
library(ppls)
library(timetk)
library(BBmisc)
library(data.table)
library(plyr)
library(xts)
library(ggplot2)
library(fPortfolio)
library(quantmod)
library(tidyverse)
library(tidyquant)
library(timetk)
library(PerformanceAnalytics)
library(tseries)
library(scales)
library(fPortfolio)
library(zoo)
library(foreach)
library(foreach)
library(DEoptim)
library(iterators)
library(fGarch)
library(Rglpk)
library(quadprog)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ROI.plugin.symphony)
library(pso)
library(GenSA)
library(corpcor)
library(testthat)
library(nloptr)
library(MASS)
library(robustbase)
library(dplyr)

# setting wd
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))


### PS1
## Problem 1
# a.

## Import data.csv 
data<-read.csv("data.csv",stringsAsFactors = FALSE,sep=";",header = TRUE)
str(data)

## Import Asset names
assetnames<-read.csv("constituents.csv",sep=";",header = TRUE)
str(assetnames)
assetnames_transpose <- as.data.frame(t(as.matrix(assetnames)))
head(assetnames_transpose)
write.csv(assetnames_transpose, file = "AN.csv", row.names = TRUE)
a<-full_join(data,assetnames_transpose)

## Set date in date format
data$date <- as.Date(data$date, format="%Y-%m-%d")
class(data$date)

## Transform data as a TS object
data.xts <- xts(subset(data, select=-date), order.by=data$date) 
head(data.xts)
time(data.xts)

## Subset the data into 2 time periods
train<-data.xts['2018-01-01/2019-12-31']
test<-data.xts['2020-01-01/2020-07-31']

## Daily Log-Returns computation for train
train.DailyLogReturns <- na.omit(Return.calculate(train, method = "log"))
head(train.DailyLogReturns)
train.DailyReturns<-na.omit(Return.calculate(train,method = "discrete"))


## Daily Log-Returns computation for test
test.DailyLogReturns <- na.omit(Return.calculate(test, method = "log"))
test.DailyReturns<-na.omit(Return.calculate(test,method = "discrete"))
head(test.DailyLogReturns)

# b. 
## The differences between returns (R) and log-returns (Log(1+r)) are
## 1. Net returns aggregate across assets whereas log-returns aggregate across time. We can compute the portfolio net return with a weighting average of the assets' net returns. This however does not work with log returns. We can compute the log returns for a time period (ex.one year) by summing the
## log returns of smaller time periods (ex. days). 
## 2. Log-Returns allow us to work with large numbers as it transforms complex computations (factorials and products) into more simple ones (sums and substractions), thus we can avoid rounding errors. In the case of FX too, Logarithms allow to compute returns easily.
## 3. Log-Returns also allow to compare different periods and in the case of short selling, the logarithm allow to simplify the computation as the log return of a short position is equivalement to the negative of the log return of the long one.   
## 4. Log-Returns are always smaller than net returns (Log returns are a concave function of net returns) and the lowest possible log return is minus infinity where it is -100% for net returns. 
## 5. As we shorten the period over which we evaluate the investment, the difference goes to zero as log(P(n)/P(n-1)) is approx. equal to P(n)/P(n-1)-1

#c 
##Compute Mean, Volatility and Sharpe Ratio, Rf=0.001 annual
meanTrain<-colMeans(train.DailyLogReturns)
print(meanTrain)
volatilityTrain<-sapply(train.DailyLogReturns, sd)
sort(colStdevs(train.DailyLogReturns))

## The function "SharpeRatio" compute the VaR, ES and S.Dev Sharpe Ratio, we proceed with the last one.
RfDaily<-0.001/252
ExcessReturnsTrain<-train.DailyLogReturns-RfDaily
SRTrain<-col_means(ExcessReturnsTrain)/colSds(ExcessReturnsTrain)

summary(meanTrain)*100
summary(volatilityTrain)
summary(SRTrain)

# d 
## Annualization of Mean, Volatility and Sharpe Ratio
AnnualizedMean<-meanTrain*252
AnnualizedSD<-volatilityTrain*sqrt(252)
AnnualizedSR<-SRTrain*sqrt(252)

MAM<-mean(AnnualizedMean)

hist(AnnualizedMean,nclass = 100, xaxt = "n",yaxt="n",col="purple",xlab = "Annualized Mean Returns",main = "Histogram of Annualized Mean Returns")
abline(v=MAM,col="blue")
axis(1, at = seq(-1.5, 1, 0.05))
axis(2, at = seq(-5, 50,5))

MASD<-mean(AnnualizedSD)

hist(AnnualizedSD,nclass = 100,xaxt = "n",yaxt="n",col="purple",xlab = "Annualized Returns' Volatility",main="Histogram of Annualized Returns' Volatility")
abline(v=MASD,col="blue")
axis(1, at = seq(-1.5, 1, 0.05))
axis(2, at = seq(-5, 50,5))

MASR<-mean(AnnualizedSR,na.rm=TRUE)

hist(AnnualizedSR,nclass = 100,xaxt = "n",yaxt="n",col="purple",xlab = "Annualized S.Dev. Sharpe Ratios",main="Histogram of Annualized S.D. Sharpe Ratios")
abline(v=MASR,col="blue")
text(-1.5, 70, "cutoff", col = "red")
axis(1, at = seq(-3, 3, 0.05))
axis(2, at = seq(-5, 200,5))

# e
## Get vectors for the portfolios' weights
w1<-rep(1/ncol(train.DailyLogReturns),ncol(train.DailyLogReturns))
w23<-rep(1/30,30)

## min Volatility portfolio, n=30
ordervol<-sort(colStdevs(train.DailyLogReturns))
OV<-as.data.frame(ordervol)
minvol<-top_n(OV,-30)
minvolnames<-t(row.names(minvol))
minvolptf<-as.data.frame(minvolnames)

## get names from constituents.csv
minVolNames<-read.csv("minVolNames.csv",header = TRUE)
minVolNames$gvkey
Namesptfii<-minVolNames[minVolNames$gvkey%in%minvolnames,]

## high momentum portfolio, n=30
momentum<-train.DailyLogReturns['2019-01-01/2019-11-30']

orderMomentum<-sort(colSums(momentum),decreasing = TRUE)
OR<-as.data.frame(orderMomentum)
maxReturn<-top_n(OR,30)

maxreturnsnames<-t(row.names(maxReturn))
ncol(maxreturnsnames)
join(max,train)
max<-as.data.frame(maxreturnsnames)
str(max)

Namesptfiii<-minVolNames[minVolNames$gvkey%in%maxreturnsnames,]
ab[,colnames(ab)%in%Namesptfiii]



## Portfolio i
qw<-mean(row_sums(train.DailyReturns*w1))*252
rs<-StdDev(train.DailyLogReturns,weights = w1)*sqrt(252)
plot(rs,qw)


## Portfolio ii
MinVolatilityAssets<-train[,colnames(train)%in%minvolptf]
MinVolReturns<-na.omit(Return.calculate(MinVolatilityAssets,method = "log"))

## Portfolio iii
HighestReturnsAssets<-train[,colnames(train)%in%max] 
haranr<-na.omit(Return.calculate(HighestReturnsAssets,method = "discrete"))
sumharanr<-mean(row_sums(haranr*w23))*252
HRAReturns<-na.omit(Return.calculate(HighestReturnsAssets, method = "log"))


## realized volatility
sdPTFIII<-StdDev(HRAReturns,weights = w23)*sqrt(252)


## Leverage
## As we can write sdIII=vsdI with v the Leverage ration, then, v=sdIII/sdI, the Leverage ratio v is then 1.469046, meaning that we invest 146.9% percent of our wealth in the PTF1
## as a result we borrow a the risk free rate 46.9% of our wealth and invest in PTF1. ex if our capital is 100, we borrow 46.9 and invest the total sum of 146.9 in the portfolio 1.
Leverage<-sdPTFIII/rs

## The leverage would be 46.9046%%




## g 
str(test)

## Portfolio i
### Realized Annualized Returns
test.DailyLogReturns <- na.omit(Return.calculate(test, method = "log"))
head(test.DailyLogReturns)
PiReturns<-colMeans(test.DailyLogReturns)*252
hist(PiReturns,nclass = 100)

PTFIAR<-sum(PiReturns*w1)

## Realized annual volatility
StdDev(test.DailyLogReturns,weights=w1)*sqrt(252)

## Annualized Sharpe ratio¨

RfDaily<-0.001/252
ExcessReturnstest<-test.DailyLogReturns-RfDaily
SRTest<-col_means(ExcessReturnstest)/colSds(ExcessReturnstest)*sqrt(252)
SRTest[!is.finite(SRTest)]<-NA
sum(w1*SRTest,na.rm = TRUE)


## Portfolio ii
### Realized Annualized Returns
MinVolatilityAssets2Returns<-test.DailyLogReturns[,colnames(test.DailyLogReturns)%in%minvolptf]

a<-colMeans(MinVolatilityAssets2Returns)*252
hist(a,nclass = 30)
PTFIIAR<-sum(a*w23)

## Realized annual volatility
StdDev(MinVolatilityAssets2Returns,weights = w23)*sqrt(252)

## Annualized Sharpe Ratio
x<-SharpeRatio(MinVolatilityAssets2Returns)[1,]*sqrt(252)
sum(x*w23)



## Portfolio iii
### Realized Annualized Returns
HighestReturnsAssets2Returns<-test.DailyLogReturns[,colnames(test.DailyLogReturns)%in%max] 
b<-colMeans(HighestReturnsAssets2Returns)*252
hist(b,nclass = 30)
PTFIIIAR<-sum(b*w23)


## Realized annual volatility
StdDev(HighestReturnsAssets2Returns,weights = w23)*sqrt(252)

## Annualized Sharpe ratio
y<-SharpeRatio(HighestReturnsAssets2Returns)[1,]*sqrt(252)
sum(y*w23)

# h
## Portfolio i 
### normalized prices 

PTFIPrices2<-data.xts


ad<-PTFIPrices2[1,]/as.numeric(sum(PTFIPrices2[1,]))
bd<-function(x){x/as.numeric(ad)}
ewp12<-t(apply(PTFIPrices2,MARGIN = 1,bd))
head(ewp12)

SumsPTFIPrices2<-.xts(x = rowSums(ewp12), .index(PTFIPrices2))
norPTFIPrices2<-SumsPTFIPrices2/as.numeric(SumsPTFIPrices2[1,])*100
plot(norPTFIPrices2,main="Normalized portfolio i prices")
norPTFIPrices2<-cbind(dates=rownames(norPTFIPrice2),norPTFIPrices2)


g<-rowSums(PTFI)
SumsPTFIPrices<-.xts(x = g, .index(PTFIPrices))
norPTFIPrices<-SumsPTFIPrices/2021.404
plot(norPTFIPrices)

PTFIPrices<-test

sum(ac)

ac<-PTFIPrices[1,]/as.numeric(sum(PTFIPrices[1,]))
bb<-function(x){x/as.numeric(ac)}
ewp1<-t(apply(PTFIPrices,MARGIN = 1,bb))
head(ewp1)

SumsPTFIPrices<-.xts(x = rowSums(ewp1), .index(PTFIPrices))
norPTFIPrices<-SumsPTFIPrices/as.numeric(SumsPTFIPrices[1,])*100
plot(norPTFIPrices)
norPTFIPrices<-cbind(dates=rownames(norPTFIPrices),norPTFIPrices)

g<-rowSums(PTFI)
SumsPTFIPrices<-.xts(x = g, .index(PTFIPrices))
norPTFIPrices<-SumsPTFIPrices/2021.404
plot(norPTFIPrices)

## Portfolio ii 
### normalized prices
PTFIIPrices<-test[,colnames(test)%in%minvolptf]

bc<-PTFIIPrices[1,]/as.numeric(sum(PTFIIPrices[1,]))
cc<-function(x){x/as.numeric(bc)}
ewp2<-t(apply(PTFIIPrices,MARGIN = 1,cc))
head(ewp2)

sum(bc)


SumsPTFIIPrices<-.xts(x = rowSums(ewp2), .index(PTFIIPrices))
norPTFIIPrices<-SumsPTFIIPrices/as.numeric(SumsPTFIIPrices[1,])*100
plot(norPTFIIPrices)
norPTFIIPrices<-cbind(dates=rownames(norPTFIIPrices),norPTFIIPrices)
SumsPTFIIPrices<-.xts(x = rowSums(PTFIIPrices), .index(PTFIIPrices))
norPTFIIPrices<-SumsPTFIIPrices/945.3314

## Portfolio iii
### normalized prices of the portfolios
PTFIIIPrices<-test[,colnames(test)%in%max]

ab<-PTFIIIPrices[1,]/as.numeric(sum(PTFIIIPrices[1,]))
dd<-function(x){x/as.numeric(ab)}
ewp3<-t(apply(PTFIIIPrices,MARGIN = 1,dd))
head(ewp3)
ewp3/PTFIIIPrices
sum(ab)

SumsPTFIIIPrices<-.xts(x = rowSums(ewp3), .index(PTFIIIPrices))
norPTFIIIPrices<-SumsPTFIIIPrices/as.numeric(SumsPTFIIIPrices[1,])*100
plot(norPTFIIIPrices)
norPTFIIIPrices<-cbind(dates=rownames(norPTFIIIPrices),norPTFIIIPrices)

### Plot with normalized prices of the 3 portfolios
plot(norPTFIIIPrices,ylim=range(65:130),main="Normalized Portfolio prices",col="brown")
lines(norPTFIPrices,col="blue")
lines(norPTFIIPrices,col="purple")

## portfolio returns
### portfolio i 
PTF1returns<-na.omit(Return.calculate(norPTFIPrices,method = "log"))
mean(PTF1returns)*252
sd(PTF1returns)*sqrt(252)
mean(PTF1returns)/sd(PTF1returns)*sqrt(252)

### portfolio ii 
PTF2returns<-na.omit(Return.calculate(norPTFIIPrices,method = "log"))
PTF2returns<-PTF2returns
mean(PTF2returns)*252
sd(PTF2returns)*sqrt(252)
mean(PTF2returns)/sd(PTF2returns)*sqrt(252)
plot(PTF2returns)


### portfolio iii
PTF3returns<-na.omit(Return.calculate(norPTFIIIPrices,method = "log"))
mean(PTF3returns)*252
sd(PTF3returns)*sqrt(252)
mean(PTF3returns)/sd(PTF3returns)*sqrt(252)


plot(PTF3returns,col="brown",main="Portfolios' returns")
lines(PTF1returns,col="blue")
lines(PTF2returns,col="purple")

plot(PTF2returns)
sd(PTF2returns)*sqrt(252)


###############################################################
########################### Problem 2 #########################
###############################################################

## Selecting the stocks "325248", "234246" and "242381"

ThreeStocks<-train[,colnames(train)%in%c("X325248","X234246","X242381")]
ThreeStocks.DailyLogReturns <- na.omit(Return.calculate(ThreeStocks, method = "log"))
ThreeStocksAnnualizedReturns<-colMeans(ThreeStocks.DailyLogReturns)*252
ThreeStocksSD<-StdDev(ThreeStocks.DailyLogReturns)*sqrt(252)
###
ER<-ThreeStocksAnnualizedReturns
names(ER)<-Stocks
covmat<-cov(ThreeStocks.DailyLogReturns*sqrt(252))
cov(ThreeStocks.DailyLogReturns*sqrt(252))

dimnames(covmat)<-list(Stocks,Stocks)
ew=rep(1,3)/3
equalWeight.portfolio = getPortfolio(er=er,cov.mat=covmat,weights=ew)
class(equalWeight.portfolio)
names(equalWeight.portfolio)
getPortfolio(er=ER,cov.mat = covmat,weights = ew)
plot(equalWeight.portfolio)
gmin.port<-globalMin.portfolio(ER,covmat)
attributes(gmin.port)
target.return<-ER[1]
e.port.msft<-efficient.portfolio(er,covmat,target.return)
RFA<-0.001
tan.port<-tangency.portfolio(ER,covmat,RFA,shorts = FALSE)
ef<-efficient.frontier(ER,covmat,alpha.min = -2,alpha.max = 1.5,nport = 200,shorts = FALSE)
attributes(ef)
summary(ef)
plot(ef$sd,ef$sd,xlim=c(0,1))
ef$call

plot(ef$sd,ef$er, plot.assets=T,xlim=c(0,0.3),ylim=c(0,0.7),xlab="SD",ylab="ER",main="Efficient Frontier")
points(ThreeStocksSD,ER,pch=20,col="green")
abline(a=0.0001, b=sr.tan,col="brown")
points(gmin.port$sd, gmin.port$er, col="black")
points(1,1)
points(tan.port$sd, tan.port$er, col="pink",pch=19)
points(tan.port$sd*opt,tan.port$er*opt,pch=19,col="blue")
sr.tan = (tan.port$er - RFA)/tan.port$sd

opt<-tan.port$er/(tan.port$sd^2)*1/10
opt*tan.port$er
opt*tan.port$sd

Stocks<-c("NIBE","WTP","SBBB")

table<-matrix(c(Stocks,ThreeStocksAnnualizedReturns,ThreeStocksSD),ncol = 3,byrow = FALSE)
colnames(table)<-c("Stocks","Expected Return","Volatility")
table1<-as.data.frame(table)
str(table1)

plot1<-ggplot(data = table1,mapping = aes(x=ThreeStocksSD,y=ThreeStocksAnnualizedReturns,color=Stocks))
plot1+
  geom_point()

CovMatrixy<-cov(ThreeStocks.DailyLogReturns*sqrt(252))
CorrMatrix<-cor(ThreeStocks.DailyLogReturns)

a1 <- matrix(rbeta(4000*3,2,2), nc=3)
a1 <- sweep(a1, 1, rowSums(a1), FUN="/")
head(a1)
colnames(a1)<-c("w1","w2","w3")
a1<-as.data.frame(a1)
str(a1)  

PReturns<-as.numeric(ef$er)
PVolatility<-as.numeric(ef$sd)

as.numeric(ef$sd)

tableforplot<-matrix(c(PReturns,PVolatility),ncol = 2,byrow=FALSE)

tableforplot<-as.data.frame(tableforplot)

plot2<-ggplot(data = tg,mapping = aes(x=PVolatility,y=PReturns))
plot2+
  geom_point()
  geom_point(data = table1,mapping = aes(x=ThreeStocksSD,y=ThreeStocksAnnualizedReturns,color=Stocks,shape="triangle",size=2))+
  geom_point(data = table1,mapping = aes(x=0,y=0.001,color="Rf",size=2))+
  geom_point(data = table1,mapping = aes(x=tangencyptfsd,y=tangencyptfmean,color="Tangency portfolio",size=2))+
  geom_point(data = table1,mapping = aes(x=weighttangency*tangencyptfsd,y=weighttangency*tangencyptfmean,color="Optimal portfolio,k=10",size=2))+
  geom_abline(intercept = 0.001,slope = slope,color="purple")


ggplot(data = e)  

tangencyptfmean<-0.4156946
tangencyptfsd<-0.009823199

weighttangency<-tangencyptfmean/tangencyptfsd*1/10


### Covariances
returnsw<-as.data.frame(ThreeStocksAnnualizedReturns)
ThreeStocksSD[,1]

sdp<-sqrt(w_1^2*sd_1^2+w_2^2*sd_2^2+w_3^2+sd_3^2+2*w_1*w_2*cov_12+2*w_1*w_3*cov_13+2*w_2*w_3*cov_23)

tableabc<-matrix(c(PReturns,sdp),ncol = 2,byrow=FALSE)

tableabc<-as.data.frame(tableabc)

plot3<-ggplot(data = tableabc,mapping = aes(x=sdp,y=PReturns))
plot3+
  geom_point()


#######################################################################################################
############################################### Problem 3 #############################################
#######################################################################################################

## a. Correlation matrix for every fiscal month
str(data.xts)
rdata.xts<-na.omit(Return.calculate(data.xts, method = "log"))
Jan.18<-rdata.xts['2018-01-01/2018-02-01']
feb.18<-rdata.xts['2018-02-01/2018-03-01']
mar.18<-rdata.xts['2018-03-01/2018-04-01']
apr.18<-rdata.xts['2018-04-01/2018-05-01']
may.18<-rdata.xts['2018-05-01/2018-06-01']
jun.18<-rdata.xts['2018-06-01/2018-07-01']
jul.18<-rdata.xts['2018-07-01/2018-08-01']
aug.18<-rdata.xts['2018-08-01/2018-09-01']
sep.18<-rdata.xts['2018-09-01/2018-10-01']
oct.18<-rdata.xts['2018-10-01/2018-11-01']
nov.18<-rdata.xts['2018-11-01/2018-12-01']
dec.18<-rdata.xts['2018-12-01/2019-01-01']
jan.19<-rdata.xts['2019-01-01/2019-02-01']
feb.19<-rdata.xts['2019-02-01/2019-03-01']
mar.19<-rdata.xts['2019-03-01/2019-04-01']
apr.19<-rdata.xts['2019-04-01/2019-05-01']
may.19<-rdata.xts['2019-05-01/2019-06-01']
jun.19<-rdata.xts['2019-06-01/2019-07-01']
jul.19<-rdata.xts['2019-07-01/2019-08-01']
aug.19<-rdata.xts['2019-08-01/2019-09-01']
sep.19<-rdata.xts['2019-09-01/2019-10-01']
oct.19<-rdata.xts['2019-10-01/2019-11-01']
nov.19<-rdata.xts['2019-11-01/2019-12-01']
dec.19<-rdata.xts['2019-12-01/2020-01-01']
jan.20<-rdata.xts['2020-01-01/2020-02-01']
feb.20<-rdata.xts['2020-02-01/2020-03-01']
mar.20<-rdata.xts['2020-03-01/2020-04-01']
apr.20<-rdata.xts['2020-04-01/2020-05-01']
may.20<-rdata.xts['2020-05-01/2020-06-01']
jun.20<-rdata.xts['2020-06-01/2020-07-01']
jul.20<-rdata.xts['2020-07-01/2020-08-01']


correlationJan.18<-cor(Jan.18)
correlationFeb.18<-cor(feb.18)
correlationMar.18<-cor(mar.18)
correlationAPR.18<-cor(apr.18)
correlationMay.18<-cor(may.18)
correlationJun.18<-cor(jun.18)
correlationJul.18<-cor(jul.18)
correlationAug.18<-cor(aug.18)
correlationSep.18<-cor(sep.18)
correlationOct.18<-cor(oct.18)
correlationNov.18<-cor(nov.18)
correlationDec.18<-cor(dec.18)
correlationJan.19<-cor(jan.19)
correlationFeb.19<-cor(feb.19)
correlationMar.19<-cor(mar.19)
correlationApr.19<-cor(apr.19)
correlationMay.19<-cor(may.19)
correlationJun.19<-cor(jun.19)
correlationJul.19<-cor(jul.19)
correlationAug.19<-cor(aug.19)
correlationSep.19<-cor(sep.19)
correlationOct.19<-cor(oct.19)
correlationNov.19<-cor(nov.19)
correlationDec.19<-cor(dec.19)
correlationJan.20<-cor(jan.20)
correlationFeb.20<-cor(feb.20)
correlationMar.20<-cor(mar.20)
correlationApr.20<-cor(apr.20)
correlationMay.20<-cor(may.20)
correlationJun.20<-cor(jun.20)
correlationJul.20<-cor(jul.20)

str(CorList)

### correlations for the first 10 stocks in Jan.18 and Mar.20
TenCorrelationsJan.18<-cor(Jan.18[,1:10])
TenCorrelationsMar.20<-cor(mar.20[,1:10])
summary(TenCorrelationsJan.18)


hist(TenCorrelationsJan.18,nclass = 30,col="lavender",xlim = range(-1:1),main = "Correlation matrix - Jan.2018")
abline(v=0.14)


hist(TenCorrelationsMar.20,nclass = 20,col="skyblue",xlim = range(-1:1),main = "Correlation matrix - Mar.2020")
abline(v=0.52)


sd(TenCorrelationsJan.18)
sd(TenCorrelationsMar.20)
plot(TenCorrelationsJan.18)
scatter.smooth(TenCorrelationsJan.18)
scatter.smooth(TenCorrelationsMar.20)

## 
CorList<-list(correlationJan.18,
              correlationFeb.18,
              correlationMar.18,
              correlationAPR.18,
              correlationMay.18,
              correlationJun.18,
              correlationJul.18,
              correlationAug.18,
              correlationSep.18,
              correlationOct.18,
              correlationNov.18,
              correlationDec.18,
              correlationJan.19,
              correlationFeb.19,
              correlationMar.19,
              correlationApr.19,
              correlationMay.19,
              correlationJun.19,
              correlationJul.19,
              correlationAug.19,
              correlationSep.19,
              correlationOct.19,
              correlationNov.19,
              correlationDec.19,
              correlationJan.20,
              correlationFeb.20,
              correlationMar.20,
              correlationApr.20,
              correlationMay.20,
              correlationJun.20,
              correlationJul.20)

Triangles<-function(x){
  triu(x,1)
}

sapply(CorList, Triangles)


MaxMinMeanCor<-sapply(CorList, Triangles)

f<-function(x){
  list(max(x[x!=0],na.rm=TRUE),min(x[x!=0],na.rm = TRUE),mean(x[x!=0],na.rm=TRUE))
}

listMaxMinMean<-sapply(MaxMinMeanCor, f)
transposed_listMaxMinMean<-t(listMaxMinMean)

tscor<-ts(transposed_listMaxMinMean,frequency = 12,start = 2018)
colnames(tscor)<-c("Max","Min","Mean")

### Correlation plots
plot(tscor[,1],main="Correlation overtime",ylim=range(-1:1),col="darkgreen",ylab="correlation")
lines(tscor[,2],col="red")
lines(tscor[,3],col="blue")
text(2020.4,0.91,labels="max")
text(2020.4,0.2,labels="mean")
text(2020.4,-0.9,labels="min")

plot(norPTFIPrices,ylim=range(60:110),main="Normalized PTF prices",col="brown")



