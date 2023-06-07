###Multiple Regression

library(fBasics)


SO2.raw<-c(65,26,69,61,94,10,18,9,10,28,31,26,29,31,16,10,13,12,17,56,36,29,14,10,24,110,28,17,8,30,9,47,35,29,14,56,14,11,46,11,23)

AveTemp.raw<-c(49.7,51.5,54.6,50.4,50,61.6,59.4,66.2,68.9,51,59.3,57.8,51.1,55.2,45.7,70.3,61,56.7,51.9,49.1,54,57.3,68.4,75.5,61.5,50.6,52.3,49,56.6,55.6,68.3,55,49.9,43.5,54.5,55.9,51.5,56.8,47.6,47.1,54)

Num.factories.raw<-c(1007,266,1692,347,343,337,275,641,721,137,96,197,379,35,569,213,91,453,454,412,80,434,136,207,368,3344,361,104,125,291,204,625,1064,699,381,775,181,46,44,391,462)

Population.raw<-c(751,540,1950,520,179,624,448,844,1233,176,308,299,531,71,717,582,132,716,515,158,80,757,529,335,497,3369,746,201,277,593,361,905,1513,744,507,622,347,244,116,463,453)


cbind(SO2.raw,AveTemp.raw,Num.factories.raw,Population.raw)


#SO2
hist(SO2.raw)
normalTest(SO2.raw,"da")
SO2<-log(SO2.raw)
normalTest(SO2,"da")

#AveTemp
hist(AveTemp.raw)
normalTest(AveTemp.raw,"da")
normalTest(log(AveTemp.raw),"da")
AveTemp<-log(AveTemp.raw)

#Num.factories
hist(Num.factories.raw)
normalTest(Num.factories.raw,"da")
normalTest(log(Num.factories.raw),"da")
Num.factories<-log(Num.factories.raw)

#Population
hist(Population.raw)
normalTest(Population.raw,"da")
normalTest(log(Population.raw),"da")
Population<-log(Population.raw)

#3D plot
library(scatterplot3d)

x<-seq(from=3.5,to=4.5,length=20)
y<-seq(from=3,to=8.7,length=20)
f<-function(x,y){z<-13.207+x*-2.772+y*0.188}
z<-outer(x,y,f)
#

par(mar=c(2,2,0,0))
sp<-scatterplot3d(AveTemp,Num.factories,SO2,zlim=c(0,7))
sp$plane3d(model,col="red")

par(mar=c(0,0,0,0))
sp<-scatterplot3d(AveTemp,Num.factories,SO2,angle=174,asp=0.15,zlim=c(1,5))
sp$plane3d(model,col="red")


model<-lm(SO2~AveTemp+Num.factories)
model
#Slopes come from cor

b1p <- (cor(SO2,AveTemp) - (cor(SO2,Num.factories)*cor(AveTemp,Num.factories)))/(1-cor(AveTemp,Num.factories)^2) # I'm calling this b1p because it's b prime 1 because it's in units of standard deviations
b2p <- (cor(SO2,Num.factories) - (cor(SO2,AveTemp)*cor(AveTemp,Num.factories)))/(1-cor(AveTemp,Num.factories)^2)



b1<-b1p*sd(SO2)/sd(AveTemp)
b2<-b2p*sd(SO2)/sd(Num.factories)



bigmod<-lm(SO2~Population+AveTemp+Num.factories)
model.matrix(bigmod)
library(car)

vif(bigmod)
cor(Population,Num.factories)
cor(Population,AveTemp)
cor(Num.factories,AveTemp)

Classmodel<-lm(SO2~AveTemp+Num.factories)



summary(Classmodel)



junk<-rnorm(n = 41,mean = 12)
junk2<-rnorm(n = 41,mean = 12)
junkmod<-lm(SO2~AveTemp+Num.factories+junk+junk2)
summary(junkmod)


#AIC

AIC(junkmod)
AIC(Classmodel)
AIC(lm(SO2~1))
step(junkmod)

plot(Classmodel)

summary(Classmodel)

zmod<-lm(scale(SO2)~scale(AveTemp)+scale(Num.factories))
summary(zmod)
library(QuantPsyc)
lm.beta(Classmodel)

####ANCOVA####

Diet<-read.csv(file = "DietAncovaData.csv")

hist(Diet$DietConsumed)
hist(Diet$Temperature)
hist(Diet$Weight)
normalTest(Diet$Weight,"da")

bigmodel<-lm(DietConsumed~Temperature+Weight+Diet,data = Diet)

vif(bigmodel)

step(bigmodel)
bettermod<-lm(formula = DietConsumed ~ Weight + Diet, data = Diet)


plot(bettermod)
normalTest(bettermod$residuals,"da")


#Modify Weight

sdqrtweight<-sqrt(Diet$Weight)
hist(sdqrtweight)


bettermod<-lm(formula = Diet$DietConsumed ~ sdqrtweight + Diet$Diet)
plot(bettermod)
hist(bettermod$residuals)
normalTest(bettermod$residuals,"da")

model.matrix(bettermod)

