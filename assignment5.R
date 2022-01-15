############################################################################
######################### TOF - PS5 - PROBLEM 2 ############################
############################################################################
install.packages("qrmtools")
install.packages("RND")
install.packages("derivmkts")
install.packages("fOptions")
install.packages("ggfortify")
install.packages("latex2exp")
install.packages("shape")
library(shape)
library(latex2exp)
library(fOptions)
library(derivmkts)
library(PerformanceAnalytics)
library(qrmtools)
library(RND)
library(ggfortify)

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

data5<-read.csv("PS5_data.csv",header = TRUE, sep = ";")

### a.
daily_returns_SP500<-returns(data5$SP500,method="logarithmic")
mean_daily_returns_SP500<-mean(daily_returns_SP500)
mean_daily_returns_SP500
volatility_SP500<-sd(daily_returns_SP500)
volatility_SP500

rho0<-0.015
alpha<-0.5
rhoH<-volatility_SP500
tau<-189

expected_volatility_SP500<-(rho0+(rhoH-rho0)*tau^(-alpha))*sqrt(tau)
expected_volatility_SP500

### b.  
K1=3700
K2=3400
S=data5$SP500[23]
rf=0.003
m=tau/252

BS <- function(S, K, r, T, sig, type){
  
  if(type=="C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(value)}
  
  if(type=="P"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
    return(value)}
}

### i Call with tau=189 and strike K=3700
call <- BS(S,K1,rf,m,expected_volatility_SP500,"C")
call

### ii Put with tau=189 and strike K=3400
put <- BS(S,K2,rf,m,expected_volatility_SP500,"P")
put

### c. Payoff diagrams
position_value<-call-put
position_value

p=4000

payoff1 <- matrix(NA, nrow = 1600, ncol = 1)
payoff2 <- matrix(NA, nrow = 1600, ncol = 1)

for (i in 2900:4500) {
  payoff1[i]<-max(i-K1,0)-call*exp(m*rf)
}

for (i in 2900:4500) {
  payoff2[i]<-exp(m*rf)*put-max(0,K2-i)
}

plot(payoff1,type="l",ylim=c(-300,300),xlim =c(2900,4500),main = "Long Call Payoff Diagram",col="purple",xlab = "S&P 500 Price at Expiration",ylab = "Payoff")
abline(a=0,b=0,col="darkgreen")
points(K1+call,0)
text(K1+call-70,30,"Breakeven",cex=0.6)
points(K1,-call)
text(K1-70,-call+30,cex = 0.6,"Strike Price")
abline(v=3700+call)


plot(payoff2,type="l",xlim =c(2900,4500),ylim = c(-300,300),main = "Short Put Payoff Diagram",col="purple",xlab = "S&P 500 Price at Expiration",ylab = "Payoff")
abline(a=0,b=0,col="darkgreen")
points(K2-put,0)
text(K2-put-70,30,"Breakeven",cex=0.6)
points(K2,put)
text(K2-70,put,cex = 0.6,"Strike Price")
lines(payoff1)
lines(payoff1+payoff2)
abline(v=3700+call-put)

3700+call-put

plot(payoff2+payoff1,type="l",xlim =c(3300,4100),ylim = c(-300,300),main = "Short Put Payoff Diagram",col="blue",xlab = "S&P 500 Price at Expiration",ylab = "Payoff")
abline(a=0,b=0,col="darkgreen")
points(3811,0)
text(3810-70,30,"Breakeven",cex=0.6)

plot(max(payoff1+payoff2,0))

plot(rowMaxs(payoff1[2900:4500]+payoff2[2900:4500],0),type="l",xlim=)

plot(rowMaxs(sumpayoff,0),type="l",xlim =c(3300,4100),ylim = c(-300,300),main = "Short Put Payoff Diagram",col="blue",xlab = "S&P 500 Price at Expiration",ylab = "Payoff")
abline(a=0,b=0,col="darkgreen")
points(3811,0)
text(3810-70,30,"Breakeven",cex=0.6)

sumpayoff<-as.data.frame(payoff1[2900:4500]+payoff2[2900:4500])
sumpayoff

index(sumpayoff)<-2900:4500
rownames(sumpayoff)<-2900:4500

a<-rbind(sumpayoff,2900:4500)
a[1]

a<-sumpayoff+rep(call-put,1600)
a


plot(rownames(sumpayoff),rowMaxs(a$`payoff1[2900:4500] + payoff2[2900:4500]`,0),type="l",xlim =c(2900,4500),ylim = c(-200,300),main = "Option Strategy Payoff Diagram",col="blue",xlab = "S&P 500 Price at Expiration",ylab = "Payoff at Expiration",lwd=2)
abline(a=0,b=0,col="darkgreen")
points(K1,0)
text(K1-70,30,"Breakeven",cex=0.6)
lines(rownames(sumpayoff),rowMaxs(sumpayoff$`payoff1[2900:4500] + payoff2[2900:4500]`,put-call),lty=2,col="purple")
legend(3500,300,legend = c("Payoff at Expiration","Profit"),col=c("blue", "purple"), lty=1:2, cex=0.6)

plot(rownames(sumpayoff),a$`payoff1[2900:4500] + payoff2[2900:4500]`,type="l",
     xlim =c(3100,4100),ylim = c(-300,300),main = "Option Strategy Payoff Diagram",
     col="blue",xlab =TeX('S&P Price at Expiration, $S_T$'),
     ylab = "Payoff at Expiration",lwd=2)
abline(a=0,b=0,col="darkgreen")
points(K1+call-put,0)
text(K1+call-put+70,20,"Breakeven",cex=0.6)
lines(rownames(sumpayoff),sumpayoff$`payoff1[2900:4500] + payoff2[2900:4500]`,lty=2,col="purple")
legend(3100,300,legend = c("Payoff at Expiration","Profit","Strike Prices"),col=c("blue", "purple","grey"), lty=c(1,2,1), cex=0.6)
abline(v=3400,col="grey",lwd=0.5)
abline(v=3700,col="grey",lwd=0.5)
text(3400,-310,"K2",col="grey")
text(3700,-310,"K1",col = "grey")
arrows(3550,-100,x1=3550,y1=-10,code=3)
text(3500,-50,"Premium",cex=0.7)


### d. Expected return and volatility

### returns for the long call
annual_return<-0.068
expected_return_at_expiration<-annual_return*189/252
expected_return_at_expiration
S*1.051

expected_call_returns<-max((1+expected_return_at_expiration)*S-K,0)-call
expected_call_returns

### returns for the short put
expected_put_returns<-put-max(0,K2-S*(1+expected_return_at_expiration))
expected_put_returns+expected_call_returns+call-put


### returns for the strategy
expected_returns_option_strategy<-(expected_put_returns+expected_call_returns)/(call-put)
expected_returns_option_strategy


### expected volatility
vega_call<-S*exp(m*rf)*dnorm((log(S/K1) + (rf + expected_volatility_SP500^2/2)*m) / (expected_volatility_SP500*sqrt(m)))*sqrt(m)/100
vega_put<-S*exp(m*rf)*dnorm((log(S/K2) + (rf + expected_volatility_SP500^2/2)*m) / (expected_volatility_SP500*sqrt(m)))*sqrt(m)/100
vega_call ### in USD, a rise in volatility of 1% makes the call more expensive by 12.21 USD
vega_put ### in USD, a rise in volatility of 1% makes the put more expensive by 10.50 USD
vega_call
vega_put
vega_call/call
vega_put/put

vega_call-vega_put

(vega_call-vega_put)/(call-put)

gBSM <- function(S, X, sigma, r, q, ttm, type){
  #S = stock price
  #X = strike price
  #sigma = volatility
  #r = risk free interest rate
  #q = dividend yield
  #ttm = time to maturity in days
  #type = option type
  
  b <- r - q
  t <- ttm/252
  
  d1 <- (log(S / X) + (b + sigma ^ 2 / 2) * t) / (sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  
  if(type == "call"){
    price <- S * exp((b - r) * t) * pnorm(d1) - X * exp(-r * t) * pnorm(d2)
  }else if (type == "put"){
    price <-  (X * exp(-r * t) * pnorm(-d2) - S * exp((b - r) * t) * pnorm(-d1))
  }
  
  return(price)
}

volOptimFun <- function(sigma, price, S, K, r, q, ttm, type){
  abs(price - gBSM(S, K, sigma, r, q, ttm, type))
}

optimize(volOptimFun, interval = c(0, 1), price = put, S = S,q=0, K = K2, r = rf, ttm = 189, type = "put")
optimize(volOptimFun, interval = c(0, 1), price = call, S = S,q=0, K = K1, r = rf, ttm = 189, type = "call")

expected_volatility_SP500
K1
#### Implied volatility is 23.36 for the call
#### IV is 23.21 for the put

compute.implied.volatility(r = rf, te = m, s0 = S, k = K1, y = 0, 
                           call.price = call, lower = 0.001, upper = 0.999)

bsputimpvol(S,K2,rf,m,0,put)
bscallimpvol(S,K1,rf,m,0,call)

### e. Compare Option Strategy VS Buy and Hold Strategy
expected_returns_option_strategy_annualized<-expected_returns_option_strategy*252/189
expected_returns_option_strategy_annualized

S*(1+0.051)

expected_returns_option_strategy_annualized/(annual_return-0.003)

7.74*6.8*189/252-0.3*189/252*6.74

### short Rf 9.407x and long the ETF 10.407x

### f. Probability Normal
### B&H
pnorm(S,mean = mean(data5$SP500)*1.068,sd=sd(data5$SP500))

pnorm(0,mean = 0.068*m,sd=expected_volatility_SP500*sqrt(189)/sqrt(252))

### option
returnTE<-(3813.172-S)/S
retrunTE
3700+call-put


#### probability that the B&H strategy is profitable
1-pnorm(0.02802,mean = 0.068*189/252,sd=expected_volatility_SP500) 

#### probability that the Option Strategy is profitable
1-pnorm(returnTE,mean = 0.068*189/252,sd=expected_volatility_SP500) 

ggdistribution(dnorm, seq(-0.6, 0.6, 0.001), mean = 0.068*189/252, sd = expected_volatility_SP500,fill = "cyan")

ggdistribution(pnorm, seq(-0.6, 0.6, 0.001), mean = 0.038427, sd = expected_volatility_SP500, colour = 'red',fill = "purple")

0.2009/sqrt(189)

