install.packages("yuima")
library(yuima)
library(dplyr)
library(ggplot2)


########## PROBLEM 3 ##########
mu<-0.02
rho1<-0.5
rho2<-0.95
t<-360
rt1<-0.005
rt2<-0.02
rt3<-0.05

ir1 <- matrix(NA, nrow = t, ncol = 1)
ir2 <- matrix(NA, nrow = t, ncol = 1)
ir3 <- matrix(NA, nrow = t, ncol = 1)
ir4 <- matrix(NA, nrow = t, ncol = 1)
ir5 <- matrix(NA, nrow = t, ncol = 1)
ir6 <- matrix(NA, nrow = t, ncol = 1)


for (t in 1:360) {
  ir1[t]<- print((1-rho1^t)*mu+rho1^t*rt1)
  ir2[t]<- print((1-rho1^t)*mu+rho1^t*rt2)
  ir3[t]<- print((1-rho1^t)*mu+rho1^t*rt3)
}

plot(ir1,type="l",ylim=c(0.01,0.03),ylab="Short interest rate (%)",xlab="Forcasting Horizon (months)")
lines(ir2)
lines(ir3)

for (t in 1:360) {
  ir4[t]<- print((1-rho2^t)*mu+rho2^t*rt1)
  ir5[t]<- print((1-rho2^t)*mu+rho2^t*rt2)
  ir6[t]<- print((1-rho2^t)*mu+rho2^t*rt3)
}

plot(ir4,type="l",ylim=c(0.00,0.05),ylab="Short interest rate (%)",xlab="Forcasting Horizon (months)",main="Expected future short rate, Vasicek")
lines(ir5,col="red")
lines(ir6,col="blue")
text(320,0.05,"current short rate: 5%")
text(320,0.047,"current short rate: 2%")
text(323,0.044,"current short rate: 0.5%")
text(280,0.05,"___",col = "blue")
text(280,0.047,"___",col = "red")
text(280,0.044,"___")
text(75,0.035,"ρ=0.95 and µ=0.02")

plot(ir1,type="l",ylim=c(0.01,0.036),ylab="Short interest rate (%)",xlab="Forcasting Horizon (months)",main="Expected future short rate, Vasicek")
lines(ir2,col="red")
lines(ir3,col="blue")
text(320,0.035,"current short rate: 5%")
text(320,0.0335,"current short rate: 2%")
text(323,0.032,"current short rate: 0.5%")
text(280,0.035,"___",col = "blue")
text(280,0.0335,"___",col = "red")
text(280,0.032,"___")
text(75,0.030,"ρ=0.5 and µ=0.02")

#### 3.c
## slope

slope1 <- matrix(NA, nrow = t, ncol = 1)
slope2 <- matrix(NA, nrow = t, ncol = 1)

for (n in 1:t) {
  slope1[n]<- print((1-rho1^n)/(1-rho1)/n)
  slope2[n]<- print((1-rho2^n)/(1-rho2)/n)
}

plot(slope1,type = "l",col="purple",main = "Slope b(n) in Vasicek model",xlab="Maturity (months)",ylab = "b(n)")
lines(slope2,col="darkgreen")
text(340,0.95,"ρ=0.95")
text(340,0.90,"ρ=0.50")
text(310,0.95,"___",col = "darkgreen")
text(310,0.90,"___",col = "purple")

intercept1<-matrix(NA, nrow = t, ncol = 1)
intercept2<-matrix(NA, nrow = t, ncol = 1)

for (n in 1:t) {
  intercept1<-print(mu*(1-slope1[1:n]))
  intercept2<-print(mu*(1-slope2[1:n]))
}  


plot(intercept1,type = "l",col="purple",main = "Intercept a(n) in Vasicek model",xlab="Maturity (months)",ylab = "a(n)")
lines(intercept2,col="darkgreen")
text(340,0.015,"ρ=0.95")
text(340,0.014,"ρ=0.50")
text(310,0.015,"___",col = "darkgreen")
text(310,0.014,"___",col = "purple")
text(340,0.013,"µ=0.02")











