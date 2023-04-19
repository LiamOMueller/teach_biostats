#intro thinking to continuous
?dunif
dunif(0.4,min=0,max=1)
dunif(0.6,min=0,max=1)

dunif(1,min=0,max=10)
dunif(0:10,min=0,max=10)
dunif(0:10,min=0,max=100)

curve(dunif(x,0,1),main="Uniform: Probability Density Function (PDF)",ylim=c(0,1),xlim=c(-1,2))

curve(punif(x,0,1),main="Uniform: Cumulative Distribution Function (CDF)",xlim=c(-1,2))
punif(0.5,min=0,max=1)
abline(v = .4)

# Normal
#normal
windows()
par(mfrow=c(2,2))#this sets up a 2*2 plotting window
curve(dnorm(x,0,1),xlim=c(-5,5),main="mu=0, stdev=1")
curve(dnorm(x,2,1),xlim=c(-5,5),main="mu=2, stdev=1")
curve(dnorm(x,0,2),xlim=c(-5,5),main="mu=0, stdev=2")
curve(dnorm(x,0,0.5),xlim=c(-5,5),main="mu=0, stdev=0.5")

y<-c(1,3,7,100,1000,1001,1002)
mean(y)
median(y)

y<-c(1,3,7,100,1000,1001)
mean(y)
median(y)


#Variance - The expected variability around the location. For a normal distribution, this is described by the parameter σ, specifically, the variance is σ^2.
set.seed(2)
y<-rnorm(40,mean=10,sd=2) #draw 40 random variables from normal
mean(y)
SS <- sum((y-mean(y))^2)
SS/(length(y)-1)
var(y)
sqrt(var(y))
sd(y)
###Standard normal
#z = (Yi-mean(Y))/sd
zy <- (y-mean(y))/sd(y)
cbind(y,zy)

#Example
#For example, say that you have data on African elephant weights. Based on previous work, your estimate of mu is 5200kg and sigma is 1450kg for bull elephants, and your estimate of mu is 2600kg and sigma is 320kg for cow elephants. If you were to find two more elephants, one male and one female, and the male weighed 6700kg and the female weighed 4100kg, which of these two animals might you be tempted to describe as more "extraordinary"? They both weigh 1500kg above the mean (the expected value). By converting kgs into units of standard deviation, we see:


#For the male:
(6700-5200)/1450

#For the female:
(4100-2600)/320

inches<-c(4,7,6,4,5,3,2,4,5)
mean(inches)
sd(inches)

cm<-inches*2.54
mean(cm)
sd(cm)

zinches<-scale(inches)
zcm<-scale(cm)

mean(zinches)#pretty close to zero - rounding problems
mean(zcm)

sd(zinches)
sd(zcm)

qnorm(p = .25)
qnorm(p=.75)

qnorm(0.025);qnorm(0.975)
qnorm(0.005);qnorm(0.995)

par(mar=c(4,4,1,1))
curve(dnorm(x),xlim=c(-5,5),main="mu=0, sd=1",ylab="probability density",xlab="y")
abline(v=c(qnorm(0.25),qnorm(0.75)),lwd=3)
abline(v=c(qnorm(0.025),qnorm(0.975)),col="red",lwd=3)
abline(v=c(qnorm(0.005),qnorm(0.995)),col="blue",lwd=3)



#Coefficient of variation
y1<-rnorm(1000,10,2) #both y1 and y2 have the same sd
y2<-rnorm(1000,100,2)

sd(y1)
sd(y2)
CV1<-sd(y1)/mean(y1)
CV2<-sd(y2)/mean(y2)

#Law of large numbers
#The **law of large numbers** is a theorem that describes that the average of a large number of experiments (or samples) will become close to the true expected value.

#Say we take a random sample from a normal population with mu=10 and sigma=6. We can explore how close we get to the "true" population parameters as we increase sample size.


set.seed(2)
y<-rnorm(100000,10,6)
mean(y[1:10])
sd(y[1:10])
mean(y[1:100])
sd(y[1:100])
mean(y)
sd(y)




#Normal Review

curve(dnorm(x,0,1),xlim=c(-5,5),main="mu=0, stdev=1")
abline(v = -2)
pnorm(q = -2,lower.tail = F)
curve(pnorm(x,0,1),xlim=c(-5,5),main="mu=0, stdev=1")
rnorm(n = 10,mean = 0,sd = 1)
qnorm(p = .25)
abline(v=-0.6744898)


####Central Limit Theorem

mean(round(runif(n = 20,min = 1,max = 20)))


samplemeans<-NA

for(i in 1:1000){
  #Sys.sleep(.1)
  sampleU<-runif(n = 500,min = 1,max = 20)
  samplemeans[i]<-mean(sampleU)
 
}

hist(samplemeans,xlim = c(3,18))

sd(samplemeans)
SE<-sd(sampleU)/sqrt(length(sampleU))
mean(sampleU)





qnorm(p = c(.025,.975),mean = mean(sampleU),sd = sd(sampleU)/sqrt(length(sampleU)))

1.96*SE+mean(sampleU)
1.96*SE-mean(sampleU)





samplemeansNORM<-NA

for(i in 1:1000){
  #Sys.sleep(.1)
  sampleNORM<-rnorm(n = 50,mean = 15,sd = 4)
  samplemeansNORM[i]<-mean(sampleNORM)
}

hist(samplemeansNORM,xlim = c(12,18))


boxplot(sampleNORM)
points(x = rep(x = 1,50),sampleNORM,pch=16,col="deeppink2",alpha=.5)
points(x = 1,mean(sampleNORM),pch=16,col="cornflowerblue",cex=.5)

sampleNORM[1]<-50
hist(sampleNORM)
