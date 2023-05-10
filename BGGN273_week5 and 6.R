#T tests
#####  t thinking

dat<-c(3,5,4,7,5,6,8,9,6,4,5,2,5,6,9,6,4,5,7)

theta<-0

t.stat<-(mean(dat)-theta)/(sd(dat)/sqrt(length(dat)))

#p - value
pt(q=t.stat,df=length(dat)-1,lower.tail=FALSE)*2

t.test(dat,mu=0)#single sample t-test 

#could test it with theta of any value (e.g., 5)


########
#paired t test
#A paired t-test is simply a single sample t-test where we are testing weather the difference between two paired samples is different from zero.
control<-c(3,5,4,7,5,6,8,9,6,4,5,2,5,6,9,6,4,5,7)
treatment<-c(10,7,8,6,0,8,5,4,8,5,6,8,7,9,4,5,12,9,5)
cbind(control,treatment)
diff<-control-treatment


mean(diff)
sd(diff)/sqrt(length(diff))


t.test(control,treatment,paired=TRUE)
t.test(diff,mu=0)
###############

#Comparing two groups (the 'classic t-test')

x1<-c(4,6,8,8,5,9,6,12,7,3)
x2<-c(9,8,11,14,9,10,6,13,12,8)

SSx1<-sum((x1-mean(x1))^2)
SSx2<-sum((x2-mean(x2))^2)
DF<-length(x1)+length(x2)-2

s.pooled<-sqrt((SSx1+SSx2)/DF)

t.stat<-(mean(x1)-mean(x2))/(s.pooled*sqrt((1/length(x1))+(1/length(x2))))
(pt(t.stat,18))*2 #we don't say lower.tail=TRUE here because t < 0

t.test(x1,x2,var.equal=TRUE)
t.test(x1,x2)#Welch, or Welch-Satterthwaite correct - adjusts degrees of freedom when variances are unequal


#Welch-Satterthwaite ###
x1sd<-sd(x1)
x2sd<-sd(x2)

numer<-((x1sd^2/length(x1))+(x2sd^2/length(x2)))^2
denom<-(1/(length(x1)-1))*(x1sd^2/length(x1))^2 + (1/(length(x2)-1))*(x2sd^2/length(x2))^2
numer/denom

t.test(x1,x2)

######
#simulate data where the variances are really different
set.seed(5)
x<-rnorm(50)
y<-rnorm(50,mean=2.5,sd=10)
t.test(x,y,var.equal=TRUE)
t.test(x,y)



#A brief mention of "non-parametric" analyses. 

#######
#non-parametrics, distribution free
#Non-parametric tests do not require normality of the data. They are sometimes called distribution free methods.

#Mann-WHitney U test # "non-parametric t-test"

a<-c(3,5,4,7,5,6,8,9,6,4,5,2,5,6,9,6,4,5,7)
b<-c(10,7,8,6,0,8,5,4,8,5,6,8,7,9,4,5,12,9,5)

wilcox.test(a,b)

#alternate way
treat<-c(rep("a",length(a)),rep("b",length(b)))
as.data.frame(cbind(treat,c(a,b)))
wilcox.test(c(a,b)~treat)

#or another alternate
t.test(rank(c(a,b))~treat)


##Paired data
#Wilcoxen signed-rank test: a "non-parametric paired t-test".
wilcox.test(a,b,paired=TRUE)

#similar results
t.test(rank(c(a,b))~treat,paired=TRUE)



# You could even test the hypothesis that groups are different using a permutation test.
# Say you have two groups
g1<-c(2,5,3,6,4,1,5,6)
g2<-c(10,12,9,6,8,3,1,8)



#could try this
mean(g1)
mean(g2)
t.test(g1,g2)
boxplot(g1,g2)#

#or, you could ask if the medians are further apart than you would expect by random chance
#permute the data
obs.dif<-abs(median(g1)-median(g2))
dat<-c(g1,g2)

set.seed(2)
biggerORnot<-NA
for(i in 1:10000){
  randomSample<-sample(dat,16)
  temp.g1 <- randomSample[1:8]
  temp.g2 <- randomSample[9:16]
  randomDiff<-abs(median(temp.g1)-median(temp.g2))
  biggerORnot[i]<- obs.dif >= randomDiff
}

1-(sum(biggerORnot)/10000) #permuted p-value

###Multiple Comps.

A<-c(56,63,45,41,71,60,78,50,68,62)
B<-c(40,48,60,38,32,44,66,22,45,54)
C<-c(71,57,64,44,73,50,79,67,84,61)

boxplot(A,B,C)


t.test(A,B)
t.test(A,C)
t.test(C,B)


p.adjust(c(0.01735,0.002458,0.3139),"bonferroni")
0.01735*3
0.002458*3
0.3139*3

p.adjust(c(0.01735,0.002458,0.3139),"holm")#holm is the default - so, you don't have to actually include it as an arguement
0.002458*3
0.01735*2
0.3139*1

#Linear Model

treats<-c(rep("A",length(A)),rep("B",length(B)),rep("C",length(C)))
response<-c(A,B,C)
dat<-as.data.frame(cbind(treats,response)) 

boxplot(response~treats)


model<-lm(response~treats)
names(model)
model
mean(A)
mean(B)-mean(A)


cbind(model.matrix(model),dat)

summary(model)



####Power

power.t.test(power = .9,sd = 1,sig.level = .05,delta = .5,type = "paired")


#It is often useful to examine 'power curves' to understand the relationship between beta, delta, alpha, n, and sigma. We will continue with the example.

#Power as a function of sample size (delta=1.2, sigma=2.4, alpha=0.05)
windows()
n<-2:50
power<-NA
windows()
for(i in n){
  power[i]<-power.t.test(n=i,delta=1.2,sd=1,sig.level=0.05,type="one.sample")$power}
par(mar=c(4,4,1,1))
plot(5:50,power[5:50],type="l",ylim=c(0,1),ylab="power",xlab="n")
#Based on our observed difference (effect size), this tells us how many replicates are needed for a given amount of power.
#Power as a function of effect size (n=20, sigma=2.4, alpha=0.05)
delt<-seq(from=0.1, to=3,length=100)
power<-NA
for(i in 1:length(delt)){
  power[i]<-power.t.test(n=20,delta=delt[i],sd=1,sig.level=0.05,type="one.sample")$power
}
par(mar=c(4,4,1,1))
plot(delt,power,type="l",ylim=c(0,1),ylab="power",xlab="raw effect size")
#This shows us our power to detect a certain effect size given our n,alpha,and,sigma.


#Effect size and number of replicates (Power =0.8,sigma=2.4,alpha=0.05)
n<-2:100
effectsize<-NA
for(i in n){
  effectsize[i]<-power.t.test(power=0.8,n=i,sd=2.4,sig.level=0.05,type="one.sample")$delta
}
par(mar=c(4,4,1,1))
plot(n,effectsize[2:100],type="l",ylim=c(0,30),ylab="effect size",xlab="sample size",xlim=c(0,40))


power.t.test(n = 15,sd = 1,sig.level = .05,power=.8)

####Problems with P

par(mar=c(0,0,0,0))
plot(0:1,0:1,type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="n")

polygon(x=c(0.05,0.25,0.25,0.05),y=c(.4,0.4,0.6,0.6));text(0.15,0.5,"10,000 tests",cex=0.8)
polygon(x=c(0.3,0.6,0.6,0.3),y=c(0.65,0.65,0.85,0.85));text(0.45,0.75,"'Real effect' in 1500\n15%",cex=0.8)
polygon(x=c(0.3,0.6,0.6,0.3),y=c(0.15,0.15,0.35,0.35));text(0.45,0.25,"No effect in 8500\n85%",cex=0.8)
polygon(x=c(0.7,0.95,0.95,0.7),y=c(0.8,0.8,0.95,0.95));text(0.82,0.87,"~55%\n true positive\n825")
polygon(x=c(0.7,0.95,0.95,0.7),y=c(0.55,0.55,0.7,0.7));text(0.82,0.63,"1500-825=\n675\n false negative")
polygon(x=c(0.7,0.95,0.95,0.7),y=c(0.45,0.45,0.3,0.3));text(0.82,0.37,"95%\n true negative\n 8075")
polygon(x=c(0.7,0.95,0.95,0.7),y=c(0.2,0.2,0.05,0.05));text(0.82,0.13,"5%\n false postive\n 425")

segments(0.25,0.6,0.3,0.65)
segments(0.25,0.4,0.3,0.35)
segments(0.6,0.8,0.7,0.9)
segments(0.6,0.7,0.7,0.6)

segments(0.6,0.3,0.7,0.4)
segments(0.6,0.2,0.7,0.1)

text(0.5,0.4,"Sig. level = 0.05",cex=1.2)
text(0.5,0.9,"Power ~ 0.55",cex=1.2)


#The total number of positives is 825+425=1250. Our false discovery rate is 425 / 1250=0.34, or 34%.

#We can also calculate or false discovery rate as:

# FDR = (sig level)*(1-prob(real))  /  (Power*prob(real)+sig.level*(1-prob(real)))

# FDR = (0.05*(1-0.15))  /  (0.55*0.15 + 0.05*(1-0.15))

#This should look familiar. . . Basically, what we’re asking is what is the probability that we’re wrong, giventhat we got a “significant” result? 

#Prob(wrong|significant p), or Prob(null is correct|significant p).

#The likelihood that we detect significance erroniously 0.05. Or, we can think of it as Prob(significant p|null is correct).

#The prior probability that the null is correct is (1−0.15).

#The probability of the data (here, the data is significance),0.55×0.15 + 0.05×(1−0.15), the probability that we correctly reject the null, plus the probability that we incorrectly reject the null. Simply, all the ways that we might get a “significant” p value.7
