### THEORY OF FINANCE - PS3 - NATHALIE MAYOR
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

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))


##############################################################################################################
################################################ PROBLEM 1 ###################################################
##############################################################################################################
data1<-read.csv("PS3_task1.csv",header = TRUE)
head(data1)
data1$Date<-as.Date(data1$Date, format="%d-%m-%Y")
data1ts<-ts(data1[-1],frequency = 12,start =c(2015,01))
str(data1ts)

## 1 - CAPM equation
MonthlyReturns<-Return.calculate(data1ts,method = "log")
rf=0.001
rfm=rf/12

### AMD
MRAMD<-MonthlyReturns[,1]
MRSPY<-MonthlyReturns[,11]-rfm
CAPM_AMD<-lm(MRAMD~MRSPY)
summary(CAPM_AMD)



plot(MRSPY[1:69],MRAMD[1:69], main="CAPM for AMD",
     ylab="Excess Return: AAPL",
     xlab="Excess Return: MARKET")
abline(CAPM_AMD)

### BA
MRBA<-MonthlyReturns[,2]
CAPM_BA<-lm(MRBA~MRSPY)
summary(CAPM_BA)

### BBVA
MRBBVA<-MonthlyReturns[,3]
MRIBEX<-MonthlyReturns[,9]-rfm
CAPM_BBVA<-lm(MRBBVA~MRIBEX)
summary(CAPM_BBVA)

### GILD
MRGILD<-MonthlyReturns[,4]
CAPM_GILD<-lm(MRGILD~MRSPY)
summary(CAPM_GILD)
  
### MAERSK
MRMAERSK<-MonthlyReturns[,5]
MROMXC<-MonthlyReturns[,10]-rfm
CAPM_MAERKS<-lm(MRMAERSK~MROMXC)
summary(CAPM_MAERKS)

### RWE
MRRWE<-MonthlyReturns[,6]
MRDAX<-MonthlyReturns[,8]-rfm
CAPM_RWE<-lm(MRRWE~MRDAX)
summary(CAPM_RWE)

### WMT
MRWMT<-MonthlyReturns[,7]
CAPM_WMT<-lm(MRWMT~MRSPY)-rfm
summary(CAPM_WMT)

### b. SML and CML
meansUS<-colMeans(na.omit(MonthlyReturns[,c(1,2,4,7)]))

capm.betas = function(r,market) {
  capm.fit = lm(r~market)
  capm.beta = coef(capm.fit)[2]
  capm.beta
}

betas = apply(MonthlyReturns[,c(1,2,4,7)],2,
              FUN=capm.betas,
              market=MRSPY)

SML<-lm(meansUS~betas)

plot(betas,meansUS,main="Expected returns vs. Beta",ylab="Mean Returns",xlab="Beta",ylim=c(-0.02,0.09),xlim=c(0,2.5))
abline(SML,col=4)
legend("topright",1, "Estimated SML",4)
text(betas[1],meansUS[1],"AMD",1.3)
text(betas[2],meansUS[2],"BA",1.3)
text(betas[3],meansUS[3],"GILD",1.3)
text(betas[4],meansUS[4],"WMT",1.1)


sdUS<-colSds(na.omit(MonthlyReturns[,c(1,2,4,7)]))
CML<-lm(meansUS~sdUS)

plot(sdUS,meansUS,main="Expected returns vs. Volatility",ylab="Mean Returns",xlab="Standard Deviation",ylim=c(-0.03,0.09),xlim=c(0.03,0.2))
abline(CML,col=6)
legend("topright",1, "Estimated CML",6)
text(sdUS[1],meansUS[1],"AMD",1.3)
text(sdUS[2],meansUS[2],"BA",1.3)
text(sdUS[3],meansUS[3],"GILD",1.3)
text(sdUS[4],meansUS[4],"WMT",1.1)

### c. (take the share price as of 30.09.2020)
AMDandWMTprices<-data1ts[69,c(1,7)]
AMDshare<-12000*AMDandWMTprices[1]
WMTshare<-3500*AMDandWMTprices[2]
EndownmentValue<-AMDshare+WMTshare

WavgBeta<-betas[1]*AMDshare/EndownmentValue+betas[4]*WMTshare/EndownmentValue
WavgBeta ### = 1.571

### to be market we need a beta of 0, so we need to short 1.57*endowment of SPY = 

shortSPYvalue<-WavgBeta*EndownmentValue

lm(MRDAX~MRSPY)
lm(MROMXC~MRSPY)
lm(MRIBEX~MRSPY)

### all indexes have a beta inferior to 1 regressed to SPY, then it is optimal to use the SPY

### d. 
### 1st approach: compute growth in SPY volatility multipl by beta^s to get the assets volatility 
### then compute exp mean returns and exp prices through SML

spyprice<-data1ts[69,11]
spylow<-spyprice/1.12
spyhigh<-spyprice/0.95

meanl<--0.12/3
meanh<-0.05/3

data1ts[69,1]*betas[1]*(1+meanl)^3
data1ts[69,1]*betas[1]*(1+meanh)^3

data1ts[69,2]*betas[2]*(1+meanl)^3
data1ts[69,2]*betas[2]*(1+meanh)^3

data1ts[69,1]*betas[1]*(1+0.05)
data1ts[69,1]*betas[1]*(1-0.12)

data1ts[69,2]*betas[2]*(1+0.05)
data1ts[69,2]*betas[2]*(1-0.12)

##############################################################################################################
################################################ PROBLEM 2 ###################################################
##############################################################################################################
data2PS3<-read.csv("PS3_task2.csv",header = FALSE)


###  Average Value Weighted Returns -- Monthly
table1<-as.numeric(data2PS3[16:1146,1:11])
colnames(table1)<-c("date",table1[1,2:11])
table11<-as.numeric(table1[-1,-1])
rownames(table11)<-date11
date11<-seq(as.Date("1926/7/1"),as.Date("2020/8/1"), by = "month")
rfm<-as.numeric(table11[,10])
rp1<-as.numeric(table11[,1])-rfm
rp2<-as.numeric(table11[,2])-rfm
rp3<-as.numeric(table11[,3])-rfm
rp4<-as.numeric(table11[,4])-rfm
rp5<-as.numeric(table11[,5])-rfm
rp6<-as.numeric(table11[,6])-rfm
fMRF<-as.numeric(table11[,7])
fSMB<-as.numeric(table11[,8])
fHML<-as.numeric(table11[,9])

table1[704,]
data3PS3[1,]

### a

FF1<-lm(rp1~fMRF+fSMB+fHML)
summary(FF1)

FF2<-lm(rp2~fMRF+fSMB+fHML)
summary(FF2)

FF3<-lm(rp3~fMRF+fSMB+fHML)
summary(FF3)

FF4<-lm(rp4~fMRF+fSMB+fHML)
summary(FF4)

FF5<-lm(rp5~fMRF+fSMB+fHML)
summary(FF5)

FF6<-lm(rp6~fMRF+fSMB+fHML)
summary(FF6)

### b 
rp1FF<-FF1$coefficients[1]+FF1$coefficients[2]*fMRF+FF1$coefficients[3]*fSMB+FF1$coefficients[4]*fHML
rp1m<-mean(rp1FF)
rp1FFm<-mean(rp1)

summary(rp1FF)
summary(rp1)

rp2FF<-FF2$coefficients[1]+FF2$coefficients[2]*fMRF+FF2$coefficients[3]*fSMB+FF2$coefficients[4]*fHML
rp2m<-mean(rp2FF)
rp2FFm<-mean(rp2)

rp3FF<-FF3$coefficients[1]+FF3$coefficients[2]*fMRF+FF3$coefficients[3]*fSMB+FF3$coefficients[4]*fHML
rp3m<-mean(rp3FF)


rp3FFm<-mean(rp3)

rp4FF<-FF4$coefficients[1]+FF4$coefficients[2]*fMRF+FF4$coefficients[3]*fSMB+FF4$coefficients[4]*fHML
rp4m<-mean(rp4FF)
rp4FFm<-mean(rp4)

FF5$coefficients

rp5FF<-FF5$coefficients[1]+FF5$coefficients[2]*fMRF+FF5$coefficients[3]*fSMB+FF5$coefficients[4]*fHML
rp5m<-mean(rp5FF)
rp5FFm<-mean(rp5)

rp6FF<-FF6$coefficients[1]+FF6$coefficients[2]*fMRF+FF6$coefficients[3]*fSMB+FF6$coefficients[4]*fHML
rp6m<-mean(rp6FF)
rp6FFm<-mean(rp6)

plot(predicted,true,xlab="Predicted mean excess returns",ylab="Mean excess returns",main="FF model")
text(true[1],predicted[1],"SMALL LoBM",-0.3)
text(true[2],predicted[2],"ME1 BM2",-0.3)
text(true[3],predicted[3],"SMALL HiBM",1.1)
text(true[4],predicted[4],"BIG LoBM",-0.3)
text(true[5],predicted[5],"ME2 BM2",-0.3)
text(true[6],predicted[6],"BIG HiBM",-0.3)
abline(a=0,b=1,col="8")

####### not including the intercept

rp1FF1<-FF1$coefficients[2]*fMRF+FF1$coefficients[3]*fSMB+FF1$coefficients[4]*fHML
summary(rp1FF1)
 
rp1m1<-mean(rp1FF1)
rp1FFm1<-mean(rp1)

rp2FF1<-FF2$coefficients[2]*fMRF+FF2$coefficients[3]*fSMB+FF2$coefficients[4]*fHML
rp2m1<-mean(rp2FF1)
rp2FFm1<-mean(rp2)

rp3FF1<-FF3$coefficients[2]*fMRF+FF3$coefficients[3]*fSMB+FF3$coefficients[4]*fHML
rp3m1<-mean(rp3FF1)

rp3FFm1<-mean(rp3)

rp4FF1<-FF4$coefficients[2]*fMRF+FF4$coefficients[3]*fSMB+FF4$coefficients[4]*fHML
rp4m1<-mean(rp4FF1)
rp4FFm1<-mean(rp4)

FF5$coefficients

rp5FF1<-FF5$coefficients[2]*fMRF+FF5$coefficients[3]*fSMB+FF5$coefficients[4]*fHML
rp5m1<-mean(rp5FF1)
rp5FFm1<-mean(rp5)

rp6FF1<-FF6$coefficients[2]*fMRF+FF6$coefficients[3]*fSMB+FF6$coefficients[4]*fHML
rp6m1<-mean(rp6FF1)
rp6FFm1<-mean(rp6)


true1<-c(rp1m1,rp2m1,rp3m1,rp4m1,rp5m1,rp6m1)
predicted1<-c(rp1FFm1,rp2FFm1,rp3FFm1,rp4FFm1,rp5FFm1,rp6FFm1)

plot(true1,predicted1,xlab="Predicted mean excess returns",ylab="Mean excess returns",main="FF model",xlim=c(0.6,1.2),ylim=c(0.6,1.2))
text(true1[1],predicted1[1],"SMALL LoBM",-0.3)
text(true1[2],predicted1[2],"ME1 BM2",-0.3)
text(true1[3],predicted1[3],"SMALL HiBM",1.1)
text(true1[4],predicted1[4],"BIG LoBM",-0.3)
text(true1[5],predicted1[5],"ME2 BM2",-0.3)
text(true1[6],predicted1[6],"BIG HiBM",-0.3)
abline(a=0,b=1,col="8")


plot(rp1,rp1FF)
plot(rp2,rp2FF)
plot(rp3,rp3FF)


### Average Equal Weighted Returns -- Monthly
table2<-data2PS3[1150:2280,1:7]
colnames(table2)<-table2[1,1:7]
table22<-table2[-1,]

###    Average Value Weighted Returns -- Annual	
table3<-data2PS3[2284:2377,1:7]
colnames(table3)<-table3[1,1:7]
table33<-table3[-1,]

###  Average Equal Weighted Returns -- Annual	
table4<-data2PS3[2381:2474,1:7]
colnames(table4)<-table4[1,1:7]
table44<-table4[-1,]

### 	 Number of Firms in Portfolios
table5<-data2PS3[2478:3608,1:7]
colnames(table5)<-table5[1,1:7]
table55<-table5[-1,]

###    Average Market Cap
table6<-data2PS3[3612:4742,1:7]
colnames(table6)<-table6[1,1:7]
table66<-table6[-1,]

### For portfolios formed in June of year t										
### Value Weight Average of BE/ME Calculated for June of t to June of t+1 as: 										
### Sum[ME(Mth) * BE(Fiscal Year t-1) / ME(Dec t-1)] / Sum[ME(Mth)]										
### Where Mth is a month from June of t to June of t+1										
### and BE(Fiscal Year t-1) is adjusted for net stock issuance to Dec t-1
table7<-data2PS3[4750:5880,1:7]
colnames(table7)<-table7[1,1:7]
table77<-table7[-1,]

### For portfolios formed in June of year t										
### Value Weight Average of BE_FYt-1/ME_June t Calculated for June of t to June of t+1 as: 										
### Sum[ME(Mth) * BE(Fiscal Year t-1) / ME(Jun t)] / Sum[ME(Mth)]										
### Where Mth is a month from June of t to June of t+1										
### and BE(Fiscal Year t-1) is adjusted for net stock issuance to Jun t	
table8<-data2PS3[5888:7018,1:7]
colnames(table8)<-table8[1,1:7]
table88<-table8[-1,]

### For portfolios formed in June of year t										
### Value Weight Average of OP Calculated as: 										
### Sum[ME(Mth) * OP(fiscal year t-1) / BE(fiscal year t-1)] / Sum[ME(Mth)] 										
### Where Mth is a month from June of t to June of t+1
table9<-data2PS3[7025:7711,1:7]
colnames(table9)<-table9[1,1:7]
table99<-table9[-1,]

### For portfolios formed in June of year t										
### Value Weight Average of investment (rate of growth of assets) Calculated as: 										
### Sum[ME(Mth) * Log(ASSET(t-1) / ASSET(t-2) / Sum[ME(Mth)] 										
### Where Mth is a month from June of t to June of t+1
table10<-data2PS3[7719:8404,1:7]
colnames(table10)<-table10[1,1:7]
table1010<-table10[-1,]


##############################################################################################################
################################################ PROBLEM 3 ###################################################
##############################################################################################################
data3PS3<-read.csv("PS3_task3.csv",header = TRUE,stringsAsFactors=FALSE,sep = ";")
data3PS3[,2] <- as.numeric(gsub("%", "",data3PS3[,2]))/100
data3PS3[,3] <- as.numeric(gsub("%", "",data3PS3[,3]))/100
data3PS3[,4] <- as.numeric(gsub("%", "",data3PS3[,4]))/100
data3PS3[,5] <- as.numeric(gsub("%", "",data3PS3[,5]))/100
data3PS3[,6] <- as.numeric(gsub("%", "",data3PS3[,6]))/100
data3PS3[,7] <- as.numeric(gsub("%", "",data3PS3[,7]))/100
data3PS3[,8] <- as.numeric(gsub("%", "",data3PS3[,8]))/100
data3PS3$DATE<-as.Date(data3PS3$DATE,format = "%m/%d/%Y")


m1<-mean(data3PS3$MKT.RF+data3PS3$RF)*12
sd1<-sd(data3PS3$MKT.RF+data3PS3$RF)*sqrt(12)
m2<-mean(data3PS3$HML.Devil+data3PS3$RF)*12
sd2<-sd(data3PS3$HML.Devil+data3PS3$RF)*sqrt(12)
m3<-mean(data3PS3$HML.FF+data3PS3$RF)*12
sd3<-sd(data3PS3$HML.FF+data3PS3$RF)*sqrt(12)

m11<-mean(data3PS3$MKT.RF-data3PS3$RF)*12
sd11<-sd(data3PS3$MKT.RF-data3PS3$RF)*sqrt(12)
m22<-mean(data3PS3$HML.Devil)*12
sd22<-sd(data3PS3$HML.Devil)*sqrt(12)
m33<-mean(data3PS3$HML.FF)*12
sd33<-sd(data3PS3$HML.FF)*sqrt(12)

sharpe1<-m11/sd11
sharpe2<-m22/sd22
sharpe3<-m33/sd33

MER<-data3PS3$MKT.RF-data3PS3$RF


## b

summary(lm(data3PS3$MKT.RF+data3PS3$RF~data3PS3$SMB+data3PS3$HML.Devil+data3PS3$RMW+data3PS3$CMA))
summary(lm(data3PS3$MKT.RF+data3PS3$RF~data3PS3$SMB+data3PS3$HML.FF+data3PS3$RMW+data3PS3$CMA))


## c
BPratios1<-c(data1ts[1,12]/data1ts[1,1],
data1ts[1,13]/data1ts[1,2],
data1ts[1,15]/data1ts[1,4],
data1ts[1,18]/data1ts[1,7])
min(BPratios1)

BPratios2<-c(data1ts[17,12]/data1ts[17,1],
             data1ts[17,13]/data1ts[17,2],
             data1ts[17,15]/data1ts[17,4],
             data1ts[17,18]/data1ts[17,7])
min(BPratios2)

BPratios3<-c(data1ts[33,12]/data1ts[33,1],
             data1ts[33,13]/data1ts[33,2],
             data1ts[33,15]/data1ts[33,4],
             data1ts[33,18]/data1ts[33,7])
min(BPratios3)

BPratios4<-c(data1ts[49,12]/data1ts[49,1],
           data1ts[49,13]/data1ts[49,2],
           data1ts[49,15]/data1ts[49,4],
           data1ts[49,18]/data1ts[49,7])
min(BPratios4)


BPratios5<-c(data1ts[65,12]/data1ts[65,1],
             data1ts[65,13]/data1ts[65,2],
             data1ts[65,15]/data1ts[65,4],
             data1ts[65,18]/data1ts[65,7])
min(BPratios5)

## 1.BA, 2. AMD, 3.BA, 4.BA, 5. BA
period1<-data1ts[1:17,2]
period2<-data1ts[17:33,1]
period3<-data1ts[33:69,2]

r1<-returns(period1)
r2<-returns(period2)[-1]
r3<-returns(period3)[-1]

ok<-append(r1,r2)
ok1<-append(ok,r3)
mean(na.omit(ok1))*12  ### yealry avg returns on BP strategy
mean(na.omit(returns(data1ts[,11])))*12 ### yearly avg returns on SP500 

### d. 
BPratios1<-c(data1ts[1,12]/data1ts[1,1],
             data1ts[1,13]/data1ts[1,2],
             data1ts[1,15]/data1ts[1,4],
             data1ts[1,18]/data1ts[1,7])
min(BPratios1)

a<-as.data.frame(data1ts[,12]/data1ts[,1])
b<-as.data.frame(data1ts[,13]/data1ts[,2])
c<-as.data.frame(data1ts[,7]/data1ts[,4])
d<-as.data.frame(data1ts[,18]/data1ts[,7])
k<-cbind(a,b,c,d)


## 1:2: BA
## 3:20 AMD
## 21:69 BA

period4<-data1ts[1:3,2]
period5<-data1ts[3:21,1]
period6<-data1ts[21:69,2]

r4<-returns(period4)[-1]
r5<-returns(period5)[-1]
r6<-returns(period6)[-1]

as<-append(r4,r5)
ad<-append(as,r6)
mean(ad)*12 ### new mean HLM devil








