####Remember, This is only for supplementing your lecture notes! Be careful, this code is often not the fastsest or best way to do something!


####Wednesday####
p<-0.02
q<-0.98

p
p*p

p*q
q*p

p<-.5

q<-.5

(p^5)*(q^5)

choose(n = 10,k = 5)

((p^5)*(q^5))*choose(n = 10,k = 5)




forest<-349

###How likely am I to see 10? in forest.

(p^10)*(q^339)



349*348*347
choose(n = 349,k = 10)

(p^10)*(q^339)*choose(n = 349,k = 10)


the.probs<-numeric()#create an empty numeric vector
obs<-0:349
for (i in 1:length(obs)){ #starts a loop - each time i will increase by 1
  the.probs[i] <- p^obs[i] * q^(349-obs[i]) * choose(349,obs[i]) #calculate
  #the probability it is in exactly obs towns
}

plot(obs,the.probs,type = "h")

#PMF

plot(obs,the.probs,type = "h",xlim = c(0,20))


sum(the.probs)
?dbinom

dbinom(x = 4:8,size = forest,prob = .02,log = FALSE)


sumprob<-cumsum(x =the.probs )
plot(obs,sumprob,type = "h",xlim = c(0,20))


pbinom(q = 7,size = forest,prob = p,lower.tail = TRUE)
pbinom(q = 0,size = forest,prob = p,lower.tail = FALSE)

pbinom(q = 0,size = forest,prob = p,lower.tail = FALSE)

round(sum(dbinom(x = 8:349,size = forest,prob = .02,log = FALSE)),6)==round(pbinom(q = 7,size = forest,prob = p,lower.tail = FALSE),6)


####Friday
A<-2

A+A

a
a1<-1
B<-6

A^B

A==B
A<B
A>B
A<=B
A!=B


Z<-c(1,2,3,4,5)
newZ<-Z+2
newZ[3]
logicZ<-newZ<6
sum(newZ<6)

colors<-c("green","blue", "red","green","blue", "red")

colors=="green"

unique(colors)



Z
newZ

Zs<-cbind(Z,newZ)
Zs
Zs[2,2]
