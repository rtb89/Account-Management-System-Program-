dims(carData)
attach(carData)
plot(MPG, VOL, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(MPG, 1/VOL, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
min(VOL)
max(VOL)
plot(1/VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(log(VOL),MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot((VOL-45),MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot((VOL-50),MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(VOL-45),MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(sqrt(VOL-45),MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(log(VOL-45),MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
identify(VOL, MPG, MAKE.MODEL) 
plot(HPEngine,horspower,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(HP,MPG, xlab= "Engine Horspower", ylab =" Average miles per gallon ", pch=19, cex=0.5)
identify(VOL, MPG, MAKE.MODEL) 
identify(HP, MPG, MAKE.MODEL) 
plot(1/HP,MPG, xlab= "Engine Horspower", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP-40),MPG, xlab= "Engine Horspower", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),MPG, xlab= "Engine Horspower", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),MPG, xlab= "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
HPT=1/(HP)
plot(SP,MPG, xlab= "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(SP,MPG, xlab= "Top speed (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/SP,MPG, xlab= "Top speed (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(SP)^2,MPG, xlab= "Top speed (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(SP)^4,MPG, xlab= "Top speed (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(SP)^4,MPG, xlab= "(Top speed)^(-4) (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
identify(1/(SP)^4,MPG,MAKE.MODEL)
min(SP)
plot(1/(SP-90),MPG, xlab= "Top speed (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
par(mfrow=c(1,2)
)
plot(1/(SP-90),MPG, xlab= "Top speed (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(SP)^4,MPG, xlab= "(Top speed)^(-4) (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(SP)^4,MPG, xlab= "(Top speed)^(-4) (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(SP-90),MPG, xlab= "1/(Top speed-90) (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(SP)^4,MPG, xlab= "(Top speed)^(-4) (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
par(mfrow=c(1,1))
plot(1/(SP-90),MPG, xlab= "1/(Top speed-90) (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(WT,MPG, xlab= "Vehicle weight (100 lb)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
identify(WT,MPG,MAKE.MODEL)
plot(WT,MPG, xlab= "Vehicle weight (100 lb)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1(WT-10),MPG, xlab= "Vehicle weight (100 lb)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(WT-10),MPG, xlab= "Vehicle weight (100 lb)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(WT-10),MPG, xlab= "1/(Vehicle weight-10) (.01 lb^-1)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
identify(1/(WT-10),MPG,MAKE.MODEL)
identify(1/(WT-10),MPG,MAKE.MODEL)
WTT=1/(WT-10)
SPT= 1/(SP)^4
plot(VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ",pch=19, cex=0.5)
modC=lm(MPG~WT+SP+HOP+VOL)
modC=lm(MPG~WT+SP+HP+VOL)
summc=summary(modC)
summc
modC1=lm(MPG~WTT+SPT+HPT+VOL)
summc1=summary(modc1)
summc1=summary(modC1)
summc1
plot(VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ",pch=19, cex=0.5)
identify(VOL, MPG, MODEL.MAKE)
identify(VOL, MPG, MAKE.MODEL)
class <- factor(vol) 
class <- factor(VOL) 
utils:::menuInstallPkgs()
update.packages
library(lattice)
class= factor(WT)
xyplot( MPG~WT, group = class,auto.key = list(title = "Class", column=10))
xyplot( MPG~WT, group = class,pch=19, cex=0.5,auto.key = list(title = "Class", column=10,pch=19, cex=0.5))
xyplot( MPG~WT, group = class,pch=19, cex=0.5,auto.key = list(title = "Class", column=10))
xyplot( MPG~WT, group = class,pch=19, cex=0.5,auto.key = list(title = "Class", column=10))
xyplot( MPG~WT, group = class,pch=19, cex=0.5,auto.key = list(title = "Class", column=10))
q()
plot(VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
attach(carData)
plot(VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
cor(VOL, WT)
cor(VOL, SP)
cor(VOL, HP)
cor(SP,HP)
cor(SP,WT)
cor(HP,WT)
VOL2=VOL^2
VOL2=VOL^3
VOL2=VOL^4
VOL2=VOL^2
VOL3=VOL^3
VOL4=VOL^4
VOL5=VOL^5
modv1= lm(MPG~VOL)
sumv1=summary(modv1)
modv1= lm(MPG~VOL+VOL2)
modv1= lm(MPG~VOL+VOL2+VOL3)
modv1= lm(MPG~VOL+VOL2+VOL4)
modv1= lm(MPG~VOL+VOL2+VOL4+VOL5)
sumv2=summary(modv2)
modv1= lm(MPG~VOL)
modv2= lm(MPG~VOL+VOL2)
modv3= lm(MPG~VOL+VOL2+VOL3)
modv4= lm(MPG~VOL+VOL2+VOL3+VOL4)
modv5=lm(MPG~VOL+VOL2+VOL3+VOL4+VOL5)
sumv2=summary(modv2)
sumv3=summary(modv3)
sumv4=summary(modv4)
sumv5=summary(modv5)
sumv1
sumv2
sumv3
sumv4
sumv5
VOL6=VOL^6
VOL7=VOL^7
modv6=lm(MPG~VOL+VOL2+VOL3+VOL4+VOL5+VOL6)
modv7=lm(MPG~VOL+VOL2+VOL3+VOL4+VOL5+VOL6+VOL7)
sumv6=summary(modv6)
sumv7=summary(modv7)
sumv6
sumv7
modv7=lm(MPG~VOL+VOL7)
modv7=lm(MPG~VOL+VOL2+VOL3+VOL4+VOL5+VOL6+VOL7)
modv55=lm(MPG~VOL+VOL5)
sumv55=summary(modv55)
sumv55
modv55=lm(MPG~VOL5)
sumv55=summary(modv55)
sumv55
VOL8=VOL^8
VOL9=VOL^9
modv9=lm(MPG~VOL+VOL2+VOL3+VOL4+VOL5+VOL6+VOL7+VOL8+VOL9)
sumv9=summary(modv9)
sumv9
summary(lm(MPG ~ VOL5+HP+SPT+WT))$adj.r.sq
summary(lm(MPG ~ VOL+VOL5+HP+SPT+WT))$adj.r.sq
summary(lm(MPGT ~ VOL+VOL5+HP+SPT+WT))$adj.r.sq
summary(lm(MPGT ~ VOL5+HP+SPT+WT))$adj.r.sq
AIC(lm(MPGT ~ VOL+VOL5+HP+SPT+WT))
BIC(lm(MPGT ~ VOL+VOL5+HP+SPT+WT))
BIC(lm(MPGT ~ VOL5+HP+SPT+WT))
BIC(lm(MPGT ~ VOL+HP+SPT+WT))
summary(lm(MPGT ~ VOL+VOL9+HP+SPT+WT))$adj.r.sq
summary(lm(MPGT ~ VOL9+HP+SPT+WT))$adj.r.sq
summary(lm(MPGT ~ expVOL+HP+SPT+WT))$adj.r.sq
summary(lm(MPGT ~ exp(VOL)+HP+SPT+WT))$adj.r.sq
summary(lm(MPGT ~ VOL+VOL1+VOL2+VOL3+VOL4+VOL5+HP+SPT+WT))$adj.r.sq
summary(lm(MPGT ~ VOL+VOL2+VOL3+VOL4+VOL5+HP+SPT+WT))$adj.r.sq
AIC(lm(MPGT ~ VOL+VOL2+VOL3+VOL4+VOL5+HP+SPT+WT))
max(VOL)
min(VOL)
xx=48:125/15
y4 = sumv5$coe[1,1] + sumv5$coe[2,1]*xx + sumv5$coe[3,1]*xx^2 + sumv5$coe[4,1]*xx^3 + sumv5$coe[5,1]*xx^4+sumv5$coe[6,1]*xx^5
points(xx, y4, type = "l", col = 4)
plot(VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
points(xx, y4, type = "l", col = 4)
xx
xx=480:1250/10
xx
y4 = sumv5$coe[1,1] + sumv5$coe[2,1]*xx + sumv5$coe[3,1]*xx^2 + sumv5$coe[4,1]*xx^3 + sumv5$coe[5,1]*xx^4+sumv5$coe[6,1]*xx^5
points(xx, y4, type = "l", col = 4)
y55 = sumv5$coe[1,1] + sumv5$coe[2,1]*xx + sumv5$coe[6,1]*xx^5
points(xx, y55, type = "l", col = 3)
y55
y55 = sumv55$coe[1,1] + sumv55$coe[2,1]*xx + sumv55$coe[6,1]*xx^5
y55 = sumv55$coe[1,1] + sumv55$coe[2,1]*xx + sumv55$coe[3,1]*xx^5
sumv55
modv55=lm(MPG~VOL+VOL5)
sumv55=summary(modv55)
y55 = sumv55$coe[1,1] + sumv55$coe[2,1]*xx + sumv55$coe[3,1]*xx^5
points(xx, y55, type = "l", col = 3)
modt55=lm(MPGT~VOL+VOL5)
sumt55=summary(modt55)
y7 = sumv5$coe[1,1] + sumv5$coe[2,1]*xx + sumv5$coe[3,1]*xx^2 + sumv5$coe[4,1]*xx^3 + sumv5$coe[5,1]*xx^4+sumv5$coe[6,1]*xx^5
y7 = sumv7$coe[1,1] + sumv7$coe[2,1]*xx + sumv7$coe[3,1]*xx^2 + sumv7$coe[4,1]*xx^3 + sumv7$coe[5,1]*xx^4+sumv7$coe[6,1]*xx^5+sumv7$coe[7,1]*xx^6+sumv7$coe[8,1]*xx^7
points(xx, y7, type = "l", col = 2)
points(xx, y7, type = "l", col = 2)
modv5=lm(MPGT~VOL+VOL2+VOL3+VOL4+VOL5)
modv5=lm(MPG~VOL+VOL2+VOL3+VOL4+VOL5)
modt5=lm(MPGT~VOL+VOL2+VOL3+VOL4+VOL5)
sumt5=summary(modt5)
sumt5
plot(VOL,MPGT, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)
yt5 = sumt5$coe[1,1] + sumt5$coe[2,1]*xx + sumt5$coe[3,1]*xx^2 + sumt5$coe[4,1]*xx^3 + sumt5$coe[5,1]*xx^4+sumt5$coe[6,1]*xx^5
points(xx, yt5, type = "l", col = 2)
mod4=lm(MPGT~VOL+VOL2+VOL3+VOL4+VOL5+WT+SPT+HP)
summary(mod4
)
summ4
sum4
summ3
mod4=lm(MPGT~VOL+WT+SPT+HP)
summ4=summary(mod4)
summ4
mod5=lm(MPGT~VOL+VOL2+VOL3+VOL4+VOL5+WT+SPT+HP)
summ5=summary(mod5)
summ5
mod6=lm(MPGT~VOL+VOL5+WT+SPT+HP)
summ6=summary(mod6)
summ6
par(mfrow = c(1, 2))
qqnorm(mod6$res, pch = 19, cex = 0.5)
qqline(mod6$res)
qqnorm(mod5$res, pch = 19, cex = 0.5)
qqline(mod5$res)
identify(mod6$res)
lev = hatvalues(mod6)
plot(lev, ylab = "Leverage Value", pch = 19, cex = 0.5)
identify(lev, labels =MAKE.MODEL)
mod7=lm(MPGT~WT+SPT+HP)
summ7=summary(mod7)
summ7
dif_betas = dfbeta(mod7)
plot(dif_betas[,1], ylab = "Change in Intercept Value", pch = 19, cex = 0.5)
identify(dif_betas[,1], labels = MAKE.MODEL)
par(mfrow = c(1, 1))
plot(dif_betas[,1], ylab = "Change in Intercept Value", pch = 19, cex = 0.5)
identify(dif_betas[,1], labels = MAKE.MODEL)
dif_betas6 = dfbeta(mod6)
plot(dif_betas6[,1], ylab = "Change in Intercept Value", pch = 19, cex = 0.5)
identify(dif_betas6[,1], labels = MAKE.MODEL)
modx=lm(log(MPG)~log(VOL)+log(WT)+log(HP)+(SP))
summary(modx)
mod8=lm(log(MPG)~WT+SPT+HP)
summary(mod8)
mod8=lm(log(MPG)~WT+SP+HP)
summary(mod8)
summ7
plot(VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ",pch=19, cex=0.5)
attach(carData)
plot(VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ",pch=19, cex=0.5)
plot(log(VOL),1/log(MPG), xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ",pch=19, cex=0.5)
plot(VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ",pch=19, cex=0.5)
identify(VOL,MPG,MAKE.MODEL)
points(xx, y5, type = "l", col = 2)
y7 = sumv5$coe[1,1] + sumv5$coe[2,1]*xx + sumv5$coe[3,1]*xx^2 + sumv5$coe[4,1]*xx^3 + sumv5$coe[5,1]*xx^4+sumv5$coe[6,1]*xx^5
y5 = sumv5$coe[1,1] + sumv5$coe[2,1]*xx + sumv5$coe[3,1]*xx^2 + sumv5$coe[4,1]*xx^3 + sumv5$coe[5,1]*xx^4+sumv5$coe[6,1]*xx^5
points(xx, y5, type = "l", col = 2)
summary(lm(MPGT ~ VOL+HP+SPT+WT))$adj.r.sq 
summary(lm(MPG ~ VOL+HP+SP+WT))$adj.r.sq 
summary(lm(MPG ~ HP+SP+WT))$adj.r.sq 
summary(lm(MPGT ~ HP+SPT+WT))$adj.r.sq 
plot(1/(HP),log(MPG), xlab = "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),1/log(MPG), xlab = "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),log(MPG), xlab = "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),1/log(MPG), xlab = "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),1/log(MPG), xlab = "Inverse(Engine Horspower)", ylab ="1/log(Average miles per gallon) ", pch=19, cex=0.5)
plot((SP),1/log(MPG), xlab = "Inverse(Engine Horspower)", ylab ="1/log(Average miles per gallon) ", pch=19, cex=0.5)
plot(1/(SP)^4,1/log(MPG), xlab = "Inverse(Engine Horspower)", ylab ="1/log(Average miles per gallon) ", pch=19, cex=0.5)
plot(1/(SP)^4,log(MPG), xlab = "Inverse(Engine Horspower)", ylab ="1/log(Average miles per gallon) ", pch=19, cex=0.5)
plot(SP,log(MPG), xlab = "Inverse(Engine Horspower)", ylab ="1/log(Average miles per gallon) ", pch=19, cex=0.5)
plot(1/(SP-90),log(MPG), xlab = "Inverse(Engine Horspower)", ylab ="1/log(Average miles per gallon) ", pch=19, cex=0.5)
plot(1/(SP)^4,1/log(MPG), xlab = "Inverse(Engine Horspower)", ylab ="1/log(Average miles per gallon) ", pch=19, cex=0.5)
identify(1/(SP)^4,1/log(MPG), MAHE.MODEL)
identify(1/(SP)^4,1/log(MPG), MAKE.MODEL)
plot(WT,VOL, xlab = "Inverse(Engine Horspower)", ylab ="1/log(Average miles per gallon) ", pch=19, cex=0.5)
plot(WT,HP, xlab = "Inverse(Engine Horspower)", ylab ="1/log(Average miles per gallon) ", pch=19, cex=0.5)
plot(WT,SP, xlab = "Inverse(Engine Horspower)", ylab ="1/log(Average miles per gallon) ", pch=19, cex=0.5)
WT
WTA=17.5
WTB= 20.0
WTC = 22.5
WTD =25.0
WTE = 30.0
WTF =35.5
WTF = 35
modz= lm (MPGT~ VOL+ HP+SPT+WTA)
modz= lm (MPGT~ VOL+HP+SPT+WTA+WTB+WTC+WTD+WTE+WTF)
modz= lm (MPGT~ VOL+HP+SPT+WTB+WTC+WTD+WTE+WTF)
summary(lm(MPGT ~ VOL+VOL2+VOL3+VOL4+VOL5+HP+SPT))$adj.r.sq
summary(lm(MPGT ~ VOL+HP+SPT))$adj.r.sq
summary(lm(MPGT ~ VOL+HPT+SPT))$adj.r.sq
summary(lm(MPGT ~ WT+HPT+SPT))$adj.r.sq
summary(lm(MPGT ~ WT+HP+SPT))$adj.r.sq
summary(lm(MPGT ~ WT+HP+SPT))$adj.r.sq
AIC(lm(MPGT ~ VOL+VOL2+VOL3+VOL4+VOL5+HP+SPT+WT))
BIC(lm(MPGT ~ VOL+VOL2+VOL3+VOL4+VOL5+HP+SPT+WT))
mod3
mod4
summary(mod4)
mod6
mod6=lm(MPGT ~ WT + SPT + HP)
summ6=summary(mod6)
summ6
plot(1/(HP),1/log(MPG), xlab = "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),log(1/MPG), xlab = "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),1/log(MPG), xlab = "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),1/log(MPG), xlab = "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),log(1/MPG), xlab = "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),1/log(MPG), xlab = "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
qqnorm(mod3$res, pch = 19, cex = 0.5)
qqnorm(mod6$res, pch = 19, cex = 0.5)
qqline(mod6$res)
qqnorm(mod3$res, pch = 19, cex = 0.5)
qqline(mod3$res)
qqnorm(mod4$res, pch = 19, cex = 0.5)
qqline(mod4$res)
mod9=lm(MPG~HP+WT+SP)
qqnorm(mod9$res, pch = 19, cex = 0.5)
qqline(mod9$res)
mod9=lm(MPGT~HP+WT+SP)
qqnorm(mod9$res, pch = 19, cex = 0.5)
qqline(mod9$res)
qqnorm(mod4$res, pch = 19, cex = 0.5)
qqnorm(mod6$res, pch = 19, cex = 0.5)
qqline(mod6$res)
qqnorm(mod9$res, pch = 19, cex = 0.5)
qqline(mod9$res)
mod9=lm(MPG~HP+WT+SP)
qqnorm(mod9$res, pch = 19, cex = 0.5)
qqline(mod9$res)
qqnorm(mod6$res, pch = 19, cex = 0.5)
qqline(mod6$res)
mod9=lm(MPGT~HP+WT+SP)
qqnorm(mod9$res, pch = 19, cex = 0.5)
qqline(mod9$res)
mod9=lm(MPG~HP+WT+SP)
qqnorm(mod9$res, pch = 19, cex = 0.5)
qqline(mod9$res)
mod9=lm(MPG~HP+WT+SP+VOL)
qqnorm(mod9$res, pch = 19, cex = 0.5)
qqline(mod9$res)
qqnorm(mod6$res, pch = 19, cex = 0.5)
mod9=lm(MPGT~HP+WT+SP)
plot(mod6$fit, mod6$res, xlab = "Fitted Values", ylab = "Residual Value", pch = 19, cex = 0.5)
identify(mod6$fit, mod6$res, MAKE.MODEL)
lev = hatvalues(mod6)
plot(lev, ylab = "Leverage Value", pch = 19, cex = 0.5)
identify(lev, labels =MAKE.MODEL)
dif_betas = dfbeta(mod6)
plot(dif_betas[,1], ylab = "Change in Intercept Value", pch = 19, cex = 0.5)
identify(dif_betas[,1], labels = MAKE.MODEL)
plot(dif_betas[,1], ylab = "Change in Intercept Value", pch = 19, cex = 0.5)
identify(dif_betas[,1], labels = MAKE.MODEL)
plot(dif_betas[,2], ylab = "Change in beta1 Value", pch = 19, cex = 0.5)
identify(dif_betas[,2], labels = MAKE.MODEL)
plot(dif_betas[,3], ylab = "Change in beta2 Value", pch = 19, cex = 0.5)
identify(dif_betas[,3], labels = MAKE.MODEL)
plot(dif_betas[,4], ylab = "Change in beta2 Value", pch = 19, cex = 0.5)
identify(dif_betas[,4], labels = MAKE.MODEL)
dif_fits = dffits(mod6)
plot(dif_fits, ylab = "Change in Fitted Value", pch = 19, cex = 0.5)
