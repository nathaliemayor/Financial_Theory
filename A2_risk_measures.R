## THEORY OF FINANCE - PROBLEM SET 2 - NATHALIE MAYOR
#####################################################################################################################
############################################### PROBLEM 1 ###########################################################
#####################################################################################################################
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
library(data.table)
library(DescTools)
## import data task 1 as csv

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

data1<-read.csv("data_task1.csv",header = TRUE)
data1rev=data1[order(nrow(data1):1),]
data1rev

data1rev$Date <- as.Date(data1rev$Date, format="%d.%m.%y")
class(data1rev$Date)
data1rev

tickers<-c("AAPL","JPM","PFE","V")
data1<-getSymbols(tickers,from='2009-08-30',to='2020-09-30',warnings = FALSE,auto.assign = TRUE)
colnames(prices) <- tickers
head(prices)
AAPL<-AAPL[,4]
JPM<-JPM[,4]
PFE<-PFE[,4]
V<-V[,4]
data<-merge(AAPL,JPM,PFE,V)

head(data)

AAPL<-data[,1]
JPM<-data[,2]
PFE<-data[,3]
V<-data[,4]
V

w1<-sum(AAPL)/sum(data)
w2<-sum(JPM)/sum(data)
w3<-sum(PFE)/sum(data)
w4<-sum(V)/sum(data)

AJPV.DailyLogReturns <- na.omit(Return.calculate(data, method = "log"))
head(AJPV.DailyLogReturns)


### a. Empirical VaR.99 1 day for each stock
E_VaR_AAPL<--quantile(AJPV.DailyLogReturns$AAPL.Close,0.01)
E_VaR_JPM<--quantile(AJPV.DailyLogReturns$JPM.Close,0.01)
E_VaR_PFE<--quantile(AJPV.DailyLogReturns$PFE.Close,0.01)
E_VaR_V<--quantile(AJPV.DailyLogReturns$V.Close,0.01)


### a. Theoretical VaR.99 1 day for each stock
meanAAPL<-mean(AJPV.DailyLogReturns$AAPL.Close)
meanJPM<-mean(AJPV.DailyLogReturns$JPM.Close)
meanPFE<-mean(AJPV.DailyLogReturns$PFE.Close)
meanV<-mean(AJPV.DailyLogReturns$V.Close)
sdAAPL<-sd(AJPV.DailyLogReturns$AAPL.Close)
sdJPM<-sd(AJPV.DailyLogReturns$JPM.Close)
sdPFE<-sd(AJPV.DailyLogReturns$PFE.Close)
sdV<-sd(AJPV.DailyLogReturns$V.Close)

T_VaR_AAPL<-(-(meanAAPL-2.33*sdAAPL))
T_VaR_JPM<-(-(meanJPM-2.33*sdJPM))
T_VaR_PFE<-(-(meanPFE-2.33*sdPFE))
T_VaR_V<-(-(meanV-2.33*sdV))

E_VaR_AAPL
T_VaR_AAPL
E_VaR_JPM
T_VaR_JPM
E_VaR_PFE
T_VaR_PFE
E_VaR_V
T_VaR_V

### b. 1 day portfolio VaR MEAN of all returns
portfolioAJPV<-.xts(rowSums(data),.index(data))
portfolio.DailyLogReturns<- na.omit(Return.calculate(portfolioAJPV, method = "log"))
portfolio.DailyLogReturns

E_VaR_PTF<--quantile(portfolio.DailyLogReturns,0.01)
meanPTF<-mean(portfolio.DailyLogReturns)
sdPTF<-sd(portfolio.DailyLogReturns)
T_VaR_PTF<--(-meanPTF-2.33*sdPTF)

E_VaR_PTF
T_VaR_PTF

### VaR from individual assets
CorrAJPV<-cor(AJPV.DailyLogReturns)
weights<-c(w1,w2,w3,w4)###
VaRi<-c(T_VaR_AAPL,T_VaR_JPM,T_VaR_PFE,T_VaR_V)
vs<-weights*VaRi
vs<-as.matrix(vs)

VaR_Portfolio<-(t(vs)%*%CorrAJPV%*%vs)^0.5

VaR_Portfolio



### c. histogram of the portfolio returns
hist(portfolio.DailyLogReturns*100,nclass = 100,col="purple",main = "Histogram of Portfolio daily returns",xlab = "daily returns")
abline(v=-VaR_Portfolio*100,col="brown")
mean(portfolio.DailyLogReturns)
sd(portfolio.DailyLogReturns)
skewness(portfolio.DailyLogReturns)
kurtosis(portfolio.DailyLogReturns)

### d. multiperiod VaR 90%
T_VaR90_AAPL<-(-(30*meanAAPL-1.28*sqrt(30)*sdAAPL))
T_VaR90_JPM<-(-(30*meanJPM-1.28*sqrt(30)*sdJPM))
T_VaR90_PFE<-(-(30*meanPFE-1.28*sqrt(30)*sdPFE))
T_VaR90_V<-(-(30*meanV-1.28*sqrt(30)*sdV))
VaR90i<-c(T_VaR90_AAPL,T_VaR90_JPM,T_VaR90_PFE,T_VaR90_V)
v90<-weights*VaR90i
v90<-as.matrix(v90)
VaR90_Portfolio<-(t(v90)%*%CorrAJPV%*%v90)^0.5
VaR90_Portfolio





#####################################################################################################################
############################################### PROBLEM 2  ###########################################################
#####################################################################################################################


data2<-read.csv("data_task2.csv",header = TRUE)
data2rev=data2[order(nrow(data2):1),]
data2$Date<-as.Date(data2$Date,format = "%d.%m.%Y")
data2ts<-ts(data2rev,frequency = 12,start =c(2009,09))
data2ts<-data2ts[,-1]
plot(data2ts)
MonthlySPYLogReturns<-na.omit(Return.calculate(data2ts, method = "log"))
MonthlySPYLogReturns
plot(MonthlySPYLogReturns)
hist(MonthlySPYLogReturns,n=50)

### a. Semi variance of montly long returns
var(MonthlySPYLogReturns)
dfMLR<-as.data.frame(MonthlySPYLogReturns)
h1<-mean(dfMLR$x)
SemiVariance<-sum((dfMLR[dfMLR$x<h1,])^2)/133
SemiVariance

### b. target semi variance 
h<-0.005
SemiVariance2
dfMLR[dfMLR$x<0.005,]
SemiVariance2<-sum((dfMLR[dfMLR$x<0.005,])^2)/133
SemiVariance2

### semi variance = target semivariance when h = mean of the returns = 0.88%

### c. Semi sd, Sharpe and sortino    
rf<-0.001/252
SemiSD<-sqrt(SemiVariance)
SemiSD
SharpeRatio<-(mean(dfMLR$x)-rf)/sd(dfMLR$x)
SortinoRatio<-(mean(dfMLR$x)-h)/sqrt(SemiVariance2)
SharpeRatio
mean(dfMLR$x)
SortinoRatio
### d. All time high
n=133

for (i in 1:n) {
  print(max(data2ts[1:i]))
}

ATH<-c(1057.08,
1095.63,1115.1,1169.43,1186.69,1257.64,1286.12,1327.22,1363.61,1365.68,1408.47,1440.67,1498.11,1514.68,1569.19,1597.57,1630.74,1685.73,1756.54,1805.81,1848.36,1859.45,1872.34,1883.95,1923.57,
1960.23,2003.37,2018.05,2067.56,2104.5,2107.39,2173.6,2198.81,2238.83,2278.87,2363.64,2384.2,2411.8,2423.41,2470.3,2471.65,2519.36,2575.26,2647.58,2673.61,2823.81,2901.52,2913.98,2945.83,2980.38,3037.56,3140.98,3230.78,3271.12,3500.31)

ad<-as.vector(data2ts)

peaksindex<-match(ATH,ad)
peaksvalues<-as.data.frame(data2ts[peaksindex])
peaksindex[1:4]
peaksindex
data2rev[peaksindex]
datepeaks<-as.data.frame(data2rev[peaksindex,1])

### 2.e max drawdawn

for (i in 55) {
  print(maxdrawdown(data2ts[1:i]))
}

maxdrawdown(data2ts[6:133])
data2ts[124:127]
data2ts

min(data2ts[peaksindex[3]:peaksindex[4]])

for (i in 1:54) {
  print(min(data2ts[peaksindex[i]:peaksindex[1+i]]))
}


peaksindex

plot(data2ts)

maxdrawdowns<-c(1036.2,1095.63,1073.87,1169.43,1030.71,1257.64,1286.12,1325.83,1131.42,
1365.68,1310.33,1412.16,1498.11,1514.68,1569.19,1597.57,1606.28,1632.97,1756.54,1805.81,
1782.59,1859.45,1872.34,1883.95,1923.57,1930.67,1972.29,2018.05,1994.99,2067.89,1920.03,
2126.15,2198.81,2238.83,2278.87,2362.72,2384.2,2411.8,2423.41,2470.3,2471.65,2519.36,
2575.26,2647.58,2673.61,2640.87,2901.52,2506.85,2752.06,2926.46,3037.56,3140.98,2584.59,
3271.12)

percentagedrawdowns<-as.data.frame((peaksvalues-maxdrawdowns)/peaksvalues)
valuesmaxdrawdowns<-as.data.frame(peaksvalues-maxdrawdowns)
plot(percentagedrawdowns)

md<-read.csv("Book2.csv",header = TRUE)
md
mdrev=md[order(nrow(md):1),]
md$date<-as.Date(md$date,format = "%d.%m.%Y")
mdts<-ts(mdrev,frequency = 12,start =c(2009,09))
mdts<-mdts[,-1]
plot(mdts*100,main="Maximum drawdowns overtime in percentages",ylab="max drawdowns",)


#####################################################################################################################
############################################### PROBLEM 3  ###########################################################
#####################################################################################################################
data3<-read.csv("data_task3.csv",header = TRUE,stringsAsFactors = FALSE,sep = ",")
data3rev=data3[order(nrow(data3):1),]
data3rev$Date<-as.Date(data3rev$Date,format = "%d.%m.%Y")
data3ts <- xts(subset(data3rev, select=-Date), order.by=data3rev$Date)

SX8Pprices<-data3rev[,2]
AHSprices<-data3rev[,3:5]


###a. 1'000'000 eq. weighted portolio
AHSpricesTS<-data3ts[,3:5]
weightsPTF3<-as.numeric(AHSpricesTS[1,])/sum(as.numeric(AHSpricesTS[1,]))
sum(as.numeric(PTF3Prices[1,]))
a<-as.matrix(data3rev[,3:5])
b<-as.matrix(1/weightsPTF3)
AHSptfPrice<-a%*%b
AHSptfPriceTS<-.xts(x = AHSptfPrice, .index(data3ts))

SX8Pprices<-data3rev[,2]
SX8PpricesTS<-.xts(x = SX8Pprices, .index(data3ts))

plot(SX8PpricesTS)


### index tracking error
DailyLogReturnsSX8P<-na.omit(Return.calculate(SX8PpricesTS, method = "log"))
DailyLogReturnsAHS<-na.omit(Return.calculate(AHSptfPriceTS, method = "log"))
ReturnsDiff<-(DailyLogReturnsAHS-DailyLogReturnsSX8P)
sd(ReturnsDiff)
IndexTrackingError<-sqrt(sum(ReturnsDiff^2)/255)
str(ReturnsDiff)
IndexTrackingError
plot(ReturnsDiff)

### summary statistics
meanTE<-mean(ReturnsDiff)
VarianceTE<-var(ReturnsDiff)
SDTE<-IndexTrackingError
minTE<-min(ReturnsDiff)
maxTE<-max(ReturnsDiff)
meanTE
VarianceTE
SDTE
minTE
maxTE
### b. rebalancing: weights proportional to market cap
Q42019start<-data3ts[1,5:7]
Q42019start<-as.numeric(Q42019start)
Q12020start<-data3ts[65,5:7]
Q12020start<-as.numeric(Q12020start)
Q22020start<-data3ts[129,5:7]
Q22020start<-as.numeric(Q22020start)
Q32020start<-data3ts[192,5:7]
Q32020start<-as.numeric(Q32020start)

WQ42019<-Q42019start/sum(Q42019start)
WQ12020<-Q12020start/sum(Q12020start)
WQ22020<-Q22020start/sum(Q22020start)
WQ32020<-Q32020start/sum(Q32020start)

c<-as.matrix(data3rev[1:64,3:5])
d<-as.matrix(WQ42019)
Q42019PtfPrice<-c%*%d

e<-as.matrix(data3rev[65:128,3:5])
f<-as.matrix(WQ12020)
Q12020PtfPrice<-e%*%f

g<-as.matrix(data3rev[129:191,3:5])
h<-as.matrix(WQ22020)
Q22020PtfPrice<-g%*%h

i<-as.matrix(data3rev[192:257,3:5])
j<-as.matrix(WQ32020)
Q32020PtfPrice<-i%*%j

Q42019PtfPrice<-as.data.frame(Q42019PtfPrice)
Q12020PtfPrice<-as.data.frame(Q12020PtfPrice)
Q22020PtfPrice<-as.data.frame(Q22020PtfPrice)
Q32020PtfPrice<-as.data.frame(Q32020PtfPrice)


rebalancedAHSPTFprices<-rbind(Q42019PtfPrice,Q12020PtfPrice,Q22020PtfPrice,Q32020PtfPrice)

RAHSPTF<-.xts(x = rebalancedAHSPTFprices, .index(data3ts))


DailyLogReturnsSX8P<-na.omit(Return.calculate(SX8PpricesTS, method = "log"))
DailyLogReturnsRAHSPTF<-na.omit(Return.calculate(RAHSPTF, method = "log"))
returnsDiff2<-(DailyLogReturnsRAHSPTF - DailyLogReturnsSX8P)
plot(returnsDiff2)
lines(ReturnsDiff,col="4")

IndexTrackingError2<-sqrt(sum(returnsDiff2^2)/255)
IndexTrackingError2
sd(returnsDiff2)


### summary statistics
meanTE2<-mean(returnsDiff2)
meanTE2
VarianceTE2
SDTE2
minTE2
maxTE2
VarianceTE2<-var(returnsDiff2)
SDTE2<-IndexTrackingError2
minTE2<-min(returnsDiff2)
maxTE2<-max(returnsDiff2)

### c.empirical standard deviation, mean absolute deviation, 
### MaxDrawdown, 95% and 99% 1-month VaR and Expected Shortfall (ES) 
### for eq. weigted PTF (AHS) and SX8P

sdAHS<-sd(DailyLogReturnsAHS)
sdSX8P<-sd(DailyLogReturnsSX8P)
meana<-mean(DailyLogReturnsAHS)
means<-mean(DailyLogReturnsSX8P)
MADAHS<-MeanAD(DailyLogReturnsAHS)
MADSX8P<-MeanAD(DailyLogReturnsSX8P)
VARAHS95<--(30*mean(DailyLogReturnsAHS)-1.64*sdAHS*sqrt(30))
VARS95<--(30*mean(DailyLogReturnsSX8P)-1.64*sdSX8P*sqrt(30))
VARAHS99<--(30*mean(DailyLogReturnsAHS)-2.33*sdAHS*sqrt(30))
VARs99<--(30*mean(DailyLogReturnsSX8P)-2.33*sdSX8P*sqrt(30))
ESa95<-(-30*meana+sdAHS*sqrt(30)*dnorm(1.64)/0.05)
ESs95<-(-30*means+sdSX8P*sqrt(30)*dnorm(1.64)/0.05)
ESa99<-(-30*meana+sdAHS*sqrt(30)*dnorm(2.33)/0.01)
ESs99<-(-30*means+sdSX8P*sqrt(30)*dnorm(2.33)/0.01)

weights
sdAHS
sdSX8P
MADAHS
MADSX8P
VARAHS95
VARS95
VARAHS99
VARs99
ESa95
ESs95
ESa99
ESs99

maxDrawdown(DailyLogReturnsAHS)
maxDrawdown(DailyLogReturnsSX8P)



### d. qq plot: normality of returns (SX8P & SAP, ASML, PROSUS)

DailyLogReturnsSX8P
qqnorm(DailyLogReturnsSX8P)
qqnormPlot(DailyLogReturnsSX8P)
hist(DailyLogReturnsSX8P,nclass = 75)
lines(seq(min(DailyLogReturnsSX8P), max(DailyLogReturnsSX8P), by = .01),dnorm(seq(min(DailyLogReturnsSX8P), max(DailyLogReturnsSX8P), by = .01), mean = mean(DailyLogReturnsSX8P), sd = sd(DailyLogReturnsSX8P)))

DailyLogReturnsSAP<-na.omit(Return.calculate(data3ts[,2], method = "log"))
qqnorm(DailyLogReturnsSAP)
qqnormPlot(DailyLogReturnsSAP)
hist(DailyLogReturnsSAP,nclass = 75)
lines(seq(min(DailyLogReturnsSAP), max(DailyLogReturnsSAP), by = .01),dnorm(seq(min(DailyLogReturnsSAP), max(DailyLogReturnsSAP), by = .01), mean = mean(DailyLogReturnsSAP), sd = sd(DailyLogReturnsSAP)))
DailyLogReturnsSX8P

DailyLogReturnsASML<-na.omit(Return.calculate(data3ts[,3], method = "log"))
qqnorm(DailyLogReturnsASML)
qqnormPlot(DailyLogReturnsASML)
hist(DailyLogReturnsASML,nclass=75)
lines(seq(min(DailyLogReturnsASML), max(DailyLogReturnsASML), by = .01),dnorm(seq(min(DailyLogReturnsASML), max(DailyLogReturnsASML), by = .01), mean = mean(DailyLogReturnsASML), sd = sd(DailyLogReturnsASML)))

DailyLogReturnsPROSUS<-na.omit(Return.calculate(data3ts[,4], method = "log"))
qqnorm(DailyLogReturnsPROSUS)
qqnormPlot(DailyLogReturnsPROSUS)
hist(DailyLogReturnsPROSUS,nclass = 75)
lines(x,y)


x <- seq(min(DailyLogReturnsPROSUS), max(DailyLogReturnsPROSUS), by = .01)
x

# Choose the mean as 2.5 and standard deviation as 0.5.
y <- dnorm(x, mean = mean(DailyLogReturnsPROSUS), sd = sd(DailyLogReturnsPROSUS))
plot(y)

density(DailyLogReturnsPROSUS)
plot(density(DailyLogReturnsPROSUS))







