####Week 9 & 10

diet<-read.csv(file = "DietAncovaData.csv")


#Does type of diet modify the ammount of food consumed?

hist(diet$DietConsumed)
ks.test(diet$DietConsumed,pnorm,mean(diet$DietConsumed),sd(diet$DietConsumed))


model<-lm(DietConsumed~Diet,data=diet)
mean(diet$DietConsumed[diet$Diet=="A"])
mean(diet$DietConsumed[diet$Diet=="B"])

summary(model)
t.test(diet$DietConsumed~diet$Diet)


#Does diet consumed change with increasing mass?



model2<-lm(diet$DietConsumed~diet$Weight)

summary(model2)


#Interaction Model


intmod<-lm(diet$DietConsumed~diet$Diet*diet$Weight)

step(intmod)

summary(intmod)

Bigintmod<-lm(diet$DietConsumed~diet$Diet*diet$Weight*diet$Temperature)

Bigintmod
betterbigmod<-lm(diet$DietConsumed~diet$Diet:diet$Weight+diet$Diet:diet$Temperature+diet$Diet+diet$Weight+diet$Temperature)

step(betterbigmod)

####Randome effects model


library(car)
load("splityield.R")
yields
lm(yield~irrigation,data=yields)
mean(yields$yield)
mean(yields$yield[yields$irrigation=="control"])
mean(yields$yield[yields$irrigation=="irrigated"])
mean(scale(yields$yield,scale=FALSE))
zmodel<-lm(scale(yields$yield,scale=FALSE)~yields$irrigation)

mean(yields$yield)-mean(yields$yield[yields$irrigation=="control"])
str(yields)
comb.field.irrigation<-paste(yields[,2],yields[,3],sep="")
boxplot(yields[,1]~comb.field.irrigation)

library(lme4)
model<-lmer(yield~irrigation+(1|field),data=yields)

nullmodel<-lmer(yield~(1|field),data=yields)
summary(model)
AIC(model)
AIC(nullmodel)
#Gotta have that p value

anova(nullmodel,model)

coef(model) #The intercepts are different but not the slopes! is that okay?


#Random slope

model2<-lmer(yield~irrigation+(1+irrigation|field),data=yields)
coef(model2)
anova(nullmodel,model2)

MegaMODEL<-lmer(yield ~ irrigation * density * fertilizer+ (fertilizer | field) + (density | field)+(irrigation | field),data=yields)

#Model selection in 2 parts

#part 1 decide on your error structure
summary(MegaMODEL)
Errormodel1<-lmer(yield~(fertilizer | field) + (density | field)+(irrigation | field),data=yields)
errorinterceptfull<-lmer(yield~(1|fertilizer) + (1|density)+(1|irrigation ),data=yields)
AIC(errorinterceptfull)
errorinterceptFERT<-lmer(yield~ (1|density)+(1|irrigation ),data=yields)
AIC(errorinterceptFERT)
errorinterceptDIV<-lmer(yield~(1|fertilizer) +(1|irrigation ),data=yields)
AIC(errorinterceptDIV)
errorinterceptIRR<-lmer(yield~(1|fertilizer) + (1|density),data=yields)
AIC(errorinterceptIRR)

JustIRR<-lmer(yield~(1|irrigation ),data=yields)
AIC(JustIRR)
