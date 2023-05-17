###Week 7####


length.cm<-c(64,69,71,67,63,62,66,60,65,68)
weight.g<-c(130,148,180,175,120,127,141,118,120,159)


cbind(length.cm,weight.g)

plot(length.cm,weight.g,pch=19,las=1);points(mean(length.cm),mean(weight.g),pch=19,cex=2,col="red")
arrows(x0 = length.cm,y0 = mean(weight.g),x1 = length.cm,y1 = weight.g,length = .1)
abline(h=mean(weight.g))
abline(v=mean(length.cm))
arrows(x0 = length.cm,y0 = weight.g,x1 = mean(length.cm),y1 = weight.g,length = .1)

length.minus.mean<-length.cm-mean(length.cm)
weight.minus.mean<-weight.g-mean(weight.g)


cbind(length.cm,weight.g,length.minus.mean,weight.minus.mean)
product<-length.minus.mean*weight.minus.mean
sum.product<-sum(product)
Covariance<-sum.product/(length(weight.g)-1)
cov(length.cm,weight.g)

length.in<-length.cm/2.54
weight.lbs<-weight.g*0.00220462
cov(scale(length.in),scale(weight.lbs))
cov(scale(length.cm),scale(weight.g))
r<-cor(length.in,weight.g)


#Fisher's r to z
z<-0.5*log((1+r)/(1-r))
sigmaz<-sqrt(1/(10-3)) #n=10


upperz<-z+sigmaz*1.96#
lowerz<-z-sigmaz*1.96#

upperz<-z+sigmaz*qnorm(0.975)#
lowerz<-z-sigmaz*qnorm(0.975)#

R<-(exp(2*z)-1)/(exp(2*z)+1)
upper<-(exp(2*upperz)-1)/(exp(2*upperz)+1)
lower<-(exp(2*lowerz)-1)/(exp(2*lowerz)+1)

cor.test(length.cm,weight.g)

SEr <- sqrt((1-cor(weight.g,length.cm)^2 )/(10-2))
t.stat <- cor(weight.g,length.cm)/SEr
one.tailed.p<-pt(t.stat,df=8,lower.tail=FALSE)

t.stat.n <- (cor(weight.g,length.cm)-0.6)/SEr
one.tailed.pn<-pt(t.stat.n,df=8,lower.tail=FALSE)


one.tailed.p<-1-pt(t.stat,df=10-2);
par(mfrow=c(1,1))
curve(dt(x,df=10-2),xlim=c(-5,5),ylim=c(0,0.5))
abline(v=0,lty=2,col="red",lwd=3)
text(-3,0.4,"50%",cex=4)
text(3,0.4,"50%",cex=4)

curve(dt(x,df=10-2),xlim=c(-5,5),ylim=c(0,0.5))
abline(v=t.stat,lty=2,col="red",lwd=3)
text(0,0.45,paste(round(pt(t.stat,df=10-2)*100,digits=3),"%"),cex=2)
text(4.9,0.3,paste(round((1-pt(t.stat,df=10-2))*100,digits=3),"%"),cex=2,srt=270)


curve(pt(x,df=10-2),xlim=c(-4,5),ylim=c(0,1))
abline(v=0,lty=2,lwd=4,col="red")
text(-1,0.8,"50%",cex=3)
text(1,0.8,"50%",cex=3)



curve(pt(x,df=10-2),xlim=c(-2,5),ylim=c(0,1))
abline(v=t.stat,lty=3,lwd=4,col="red")
p.value<-2*one.tailed.p
abline(h=pt(t.stat,df=10-2),lty=2,lwd=3,col="blue")
arrows(3,0.6,4,pt(t.stat,df=10-2)-0.05)
arrows(3,0.5,4,0.1)
text(3,0.55,paste("p=",round(pt(t.stat,df=10-2),digits=4)),cex=2)

pt(t.stat,df=10-2,lower.tail=FALSE)#one tailed p-value
2*one.tailed.p#two tailed p value

Correlation<-cor(weight.g,length.cm)
plot(length.cm,weight.g,pch=19,las=1)
text(68,135,paste("r=",round(Correlation,digits=5)),cex=1)
text(68,130,paste("p=",round(p.value,digits=4)),cex=1)

#calculate p-value
sr<-sqrt((1-R^2)/(n-2))
tstat<-R/sr
pval<-pt(tstat,n-2,lower.tail=FALSE)*2


cat("\n\n\nr=",r,"\nupper 95% CI",upper,"\nlower 95% CI",lower,"\n\nstandard error of r:",sr,"\n\nt =",tstat,"\ndf=",n-2,"\np=",pval,"\n\n")

cor(length.cm,weight.g)
cor.test(length.cm,weight.g)

####So with r and n, we can test hypotheses about r without even having the raw data
# suppose n=20 and r=0.8
#95% CI of r

z<-0.5*log((1+0.8)/(1-0.8))
sigma.z <- sqrt(1/(20-3))

upper <- z + sigma.z*1.96
lower <- z - sigma.z*1.96

# translate back to r
upper.r<-(exp(2*upper )-1)/(exp(2*upper )+1)
lower.r<-(exp(2*lower)-1)/(exp(2*lower)+1)

# calculate t
se.r<-sqrt((1-0.8^2)/18)
t.r <- 0.8/ se.r
pt(t.r,18,lower.tail=FALSE)*2



####Power

library(pwr)
pwr.r.test(n=10,r=0.844,sig.level=0.05)
pwr.r.test(r=0.444,sig.level=0.05,power=.8)
pwr.r.test(n=5,sig.level=0.05,power=.8)

###Non parametric

# spearman vs. pearson
#spearman is often called a non-parametric correlation
#spearman correlation is simply the Pearson Product Moment correlation of rank transformed data

cor.test(length.cm,weight.g,method="spearman")
cor.test(rank(length.cm),rank(weight.g),method="pearson")

#Kendal Tau - another type of "non-parametric correlation" - a measure of discordance
# = (C-D)/(C+D)
# Concordance is the number of rankings that are above the focal ranking (the second column)
X<-c(1,2,3,4,5,6,7,8,9,10,11,12)
Y<-c(2,1,4,3,6,5,8,7,10,9,12,11)
dat<-cbind(X,Y)# for 1, there are 10 rankings higher than 2  for 2, there are 10 rankings higher than 1  for 4, there are 8 rankings higher and so on...
#Concordant:how many larger ranks are below a certain rank
Concordant<-c(10,10,8,8,6,6,4,4,2,2,0,NA)# add NA for showing cbind dat - NA means no assignment
#Discordant: how many smaller ranks are are below a certain rank
Discordant<-c(1,0,1,0,1,0,1,0,1,0,1,NA)
cbind(dat,Concordant,Discordant)
C<-c(10,10,8,8,6,6,4,4,2,2,0)# 
D<-c(1,0,1,0,1,0,1,0,1,0,1)
tau<-(sum(C)-sum(D))/(sum(C)+sum(D))
cor(X,Y,method="kendall")
