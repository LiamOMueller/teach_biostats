### moments
library("e1071")
?skewness

set.seed(3)
curve(dgamma(x,2,1),xlim=c(0,10))
x<-rgamma(10000,2,1)
plot(density(x,adjust=2))

# a<-rnorm(5000,0,2)
# b<-rnorm(1000,-2,4)
# c<-rnorm(3000,4,5)
# skewed<-c(a,b,c)
skewed.mean<-mean(x)
abline(v=skewed.mean,col="orange")
abline(v=median(x),col="darkgreen")
pnorm(skewed.mean,skewed.mean,sd(x))#predicted density if normal
pgamma(skewed.mean,2,1)#actual density to the mean
skewed.median<-quantile(x,0.5)
pgamma(skewed.median,2,1)#median actually does a better job...
abline(v=skewed.median,col="blue")
skewed.sd<-sd(x)

curve(dnorm(x,skewed.mean,skewed.sd),add=TRUE,col="red",lwd=2,lty=2)
skewness(x) #right, or positive skewed

curve(dgamma(x,1.1,1),xlim=c(0,10))
xm<-rgamma(10000,1.1,1)
skewness(xm)

set.seed(3)
qqplot(-rnorm(length(x),mean(x),sd(x)),x,ylim=c(-10,12));abline(0,1)


lx<--x
plot(density(lx,adjust=2))
skewed.mean<-mean(lx)
abline(v=skewed.mean,col="orange")
skewed.sd<-sd(lx)
abline(v=median(lx),col="darkgreen")
curve(dnorm(x,skewed.mean,skewed.sd),add=TRUE,col="red",lwd=2,lty=2)
skewness(lx) #Left, or negative skewed

set.seed(3)
qqplot(rnorm(length(lx),mean(lx),sd(lx)),lx);abline(0,1)




?kurtosis

#leptokurtic - too much concentration in center

adj.mean1<-sort(rnorm(1000,0,0.1))
adj.mean2<-sort(rnorm(500,0,0.5))
adj.mean3<-sort(rnorm(300,0,0.8))
adj.mean4<-sort(rnorm(200,0,1))
adj.mean5<-sort(rnorm(100,0,1.2))
kurtos<-c(adj.mean1,adj.mean2,adj.mean3,adj.mean4,adj.mean5)


plot(density(kurtos,adj=2))
kurtos.mean<-mean(kurtos)
kurtos.sd<-sd(kurtos)
curve(dnorm(x,kurtos.mean,kurtos.sd),add=TRUE,col="red")
kurtosis(kurtos)

qqplot(rnorm(length(kurtos),mean(kurtos),sd(kurtos)),kurtos);abline(0,1)



#platykurtic - fat tails

set.seed(2)
kurtos<-rbeta(1000,2,2)

plot(density(kurtos,adj=2),ylim=c(0,2))
hist(kurtos)
kurtos.mean<-mean(kurtos)
kurtos.sd<-sd(kurtos)
curve(dnorm(x,kurtos.mean,kurtos.sd),add=TRUE,col="red")
kurtosis(kurtos)

set.seed(3)
qqplot(rnorm(length(kurtos),mean(kurtos),sd(kurtos)),kurtos);abline(0,1)



#TESTS FOR NORMALITY
#package fBasics - has function to test for normality


#3 MAIN TESTS FOR NORMALITY, 3RD IS THE BEST
#1st - Shapiro Wilks Test
#weaknesses - sensitive to small samples, with small samples will allow things to pass
#with large samples, will flag the ones that don't work
#doesn't work well if have identical values        

set.seed(6)
library(fBasics)
x<-rnorm(50)

hist(x,breaks=15)

normalTest(x,method=c("sw"))   #function in fBasics that tests Shapiro wilks and others
#null hypo of normality test is that data is not different from normal
#so according to the result of this shapiro wilks test, our data is not normal
#but we drew it from a normal distribution, so use head and look at it


#Kolmogorov-Smirnoff Test
#most common one in the lit, but probs the worst
#originally developed to compare 2 distributions, so is doing a QQ plot really
#is taking our data and is challenging it against the theoretical expectations, and looking at how far our is from theretical
#if is normal, all dots should be on line
normalTest(x,method=c("ks")) #how to test for normality with KS
#here is passes the test, so acceept the null that our data is normally distributed, cause 2 sided is 0.57



#D'Agostine Pearson Omnibus Test
#looking at both skewness and kurtosis (whether have a lot of data crammed towards mean or tail)
normalTest(x,method=c("da"))
#get a p value of 0.1798, so accept null that data is dsitributed normally
#also give a skewness and kurtosis numbers, so can help us decide how and if should transform data

#Error term must be normally distributed, can see if residual data is normally distributed

#care about normality with regression, cause taking the mean of each variable when calculating intercept and the covariance of this

#ERROR TERM IS THE MOST IMPORTANT THING THAT HAS TO BE NORMALLY DISTRIBUTED, MORE IMPORTANT THAN ACTUAL DATA


#MA & RMA





#############
################################################################

#Why transform? So that the data are in better agreement with the assumptions of the model (i.e., they fit to a particular distribution). 

#People often get nervous about transforming (it might feel like 'cheating'). Transforming for the purpose of p-hacking is just bad form. However, there is nothing wrong with transforming your data. The units in which we measure things are entirely arbitrary. We feel comfortable with base 10 (coincidently, we have 10 fingers. Hmmmm). However many things that we do feel comfortable dealing with are not on a base 10 scale. For example, the amount of free hydrogen ions in an aquous solution (i.e., pH) is measured on a log10 scale. Similarly, the Richter scale is based on a log10 scale. 


#
#transformations
#power #Good for positive skewed data (right skewed). Often makes sense for area and often makes sense for volumes.
#are not uncommon for abundance data in ecology, where there are many 0's and few large values (however, this still won't eliminate 0s).
set.seed(4)
y<-rnorm(50,10,3)
yy<-y^3

hist(yy)
hist(yy^(1/3),breaks=10)

#### makes sense for some things   e.g., area
set.seed(2)
length<-sort(rnorm(100,10,3))
width<-sort(rnorm(100,30,5))

area<-length*width
hist(area,breaks=20)
plot(length,area)
plot(length,sqrt(area))

little.area<-area[1:50]
large.area<-area[51:100]

boxplot(little.area,large.area)
sd(little.area)
sd(large.area)
sd(sqrt(little.area))
sd(sqrt(large.area))


#log
#also good for positive skewed data, especially when the mean is related to the standard deviation (i.e., when the variance increases and the mean increases).

#log(Y+c). 
# c is an appropriate constant if you have values <=0

set.seed(3)
y<-rlnorm(1000,10,1)
hist(y,breaks=50)
hist(log(y+1),breaks=50)

ylog<-log(y+1)#transform
mean(ylog)
exp(mean(ylog))-1#back transform
abline(v=mean(y),lwd=2,col="red")
abline(v=median(y),lwd=2,lty=2,col="goldenrod")
abline(v=exp(mean(ylog))-1,lwd=2,col="blue")
abline(v=mean(ylog),col="firebrick",lwd=10)
abline(v=mean(ylog),col="red",lwd=2,lty=10)
####Biostatistics
set.seed(7)


#recip
#often used for rates
set.seed(4)
y<-rnorm(1000,10,3)
yy<-1/y

hist(yy,breaks=50)
hist(1/yy,breaks=50)






# arcsin - used to be commonly used for proportion or percentages
#*Percentages or proportions** - data that are percentages or proportions are bounded between 0 and 1 (or 0 and 100 for percentages). Power transformation doesn't work well for these because they change each end of the distribution differently.
#The traditional way to expand out the tails was to use the arcsin transformation.
#Most effective if Y is close to 0 or 1. It has little effect on the mid-range of proportions.
#The arcsin transformation has fallen out of favor with the advent of generalized linear models (mid 1980s).

set.seed(1)
y<-rbeta(100,3,2)#beta distribution is contained between 0 and 1
hist(y,breaks=20)

trans.y<-asin(sqrt(y))
hist(trans.y,breaks=15)

set.seed(1)
y<-rbeta(1000,1.1,2)
hist(y,breaks=20)
trans.y<-asin(sqrt(y))
hist(trans.y,breaks=15)

#rank - simply a rank of the data.
#This transformation loses information and it is impossible to get it back (i.e., you cannot back transform the data).
set.seed(4)
y<-runif(100,10,200)
hist(y,breaks=20)
y.trans<-rank(y)
hist(y.trans,breaks=20)

round.y<-round(y)
length(unique(round.y))
rank(round.y)

cbind(y[order(y)],round.y[order(y)],rank(round.y[order(y)]))

#standardization- standardize variables in relation to one another.
y<-rnorm(100,10,4)
hist(y,breaks=20)
normalTest(y,method = "da")
mean(y)
sd(y)

#centering -centering. Centers a variable so it's mean is zero. This is sometimes called "translation".
std.var.center<-y-mean(y)
hist(std.var.center,breaks=20)
mean(std.var.center)#new mean is zero - or really really close (computer floating point issue)
sd(std.var.center)#sd is the same as the uncentered

#center and scaling by standard deviation
#It will change the units of the data into standard deviations. The transformed data will have a mean of 0 and a standard deviation of 1.
std.var<-(y-mean(y))/sd(y)
hist(std.var,breaks=20)
mean(std.var)
sd(std.var)

#the function scale() will do this too
mean(scale(y,scale=FALSE))#this will center the data, but not scale by sd
sd(scale(y,scale=FALSE))

#the default setting of scale() will give the z-transformation
mean(scale(y))
sd(scale(y))
Yz<-scale(x = y,center = TRUE,scale = TRUE)


mean(Yz)
sd(Yz)

?boxcox

boxcoxres<-boxcox(object = lm(y~1),lambda =seq(-2, 2, 1/1000),plotit = T)


boxcoxres$x[which.max(boxcoxres$y)]

### Single sample t tests. Above, We asked if the data were random samples drawn from a normal distribution with a mu parameter = to the mean of the data and a sigma parameter = to the standard deviation of the data. Now however, lets ask if our data are instead random variables drawn from a different, null hypothesized mu, which we call theta.

jagteath <- rnorm(10,mean = .4,sd = .1)#Your observed data

theta<-.3 #The null hypothesized mean.

tscore<-(mean(jagteath)-theta)/(sd(jagteath)/sqrt(10))#The t score is how far away our observation is from the null in Standard Error Units

curve(dt(x,9),xlim=c(-4,4)) #Looks kinda normal
abline(v=tscore,lwd=2,col="pink") #Both the positive and negative t score must be accounted for in testingthe null.
abline(v=-tscore,lwd=2,col="pink")


pt(q = tscore,df = 9,lower.tail = FALSE)*2


#Calculating the confidence interval of T tests.
SEJT<-sd(jagteath)/sqrt(10) #The standard error of the mean
BadUpperCI<-mean(jagteath)+1.96*SEJT #This is what you would normally do, but now that we have done the t test, our confidence intervals should be based on the t distribution, not the standard normal!!!


qt(p = .025,df = 9) #The quantiles of a t distribution are wider! especially at low degrees of freedom
CorrectUpperCI<-mean(jagteath)+(qt(p = .975,df = 9))*SEJT
CorrectLowerCI<-mean(jagteath)+(qt(p = .025,df = 9))*SEJT

t.test(x = jagteath,mu = theta)#Compare to results in t.test! 
