library(ggplot2)
library(nycflights13)
flightdata<-flights
head(flightdata)
str(flightdata)
unique(flightdata$carrier)

####Scatter plot####

HAflights<-flightdata[flightdata$carrier=="HA", ]
mean(HAflights$dep_delay)
mean(HAflights$arr_delay)

ggplot(data = HAflights,mapping = aes(x=dep_delay,y=arr_delay))+
  geom_point(alpha=.2,col="purple")+
  theme_minimal()


#### Line Plot####
max(HAflights$dep_delay)
latestflighttime<-HAflights$time_hour[which(HAflights$dep_time==max(HAflights$dep_time))]

JFKWeather <- weather[weather$origin=="JFK",]
JFKFebWeather<-JFKWeather[JFKWeather$month==2,]

ggplot(data = JFKFebWeather,mapping = aes(x=time_hour,y=temp))+
  geom_line()+
  geom_vline(xintercept = latestflighttime,color="red",size=1.5,alpha=.5)


ggplot(data = JFKFebWeather,mapping = aes(x=time_hour,y=precip))+
  geom_line()+
  geom_vline(xintercept = latestflighttime,color="red",size=1.5,alpha=.5)
  


####Histograms####

ggplot(data = JFKFebWeather,mapping = aes(x=precip))+
  geom_histogram(boundary=0)+
  geom_vline(xintercept = 0.08,color="red",size=1.5)

ggplot(data = JFKWeather, mapping = aes(x = temp)) +
  geom_histogram(binwidth = 2, color = "white") +
  facet_wrap(~ month, nrow = 4)



####boxplot####

ggplot(data = JFKWeather, mapping = aes(x = factor(month), y = temp)) +
  geom_point(color="darkgreen",alpha=0.02)

ggplot(data = JFKWeather, mapping = aes(x = factor(month), y = temp)) +
  geom_violin()+#Makes the empty plot
  #Makes the boxes
  theme_classic()+
  xlab("Month")+
  ylab("How fast the air is vibrating")

str(JFKWeather)



####Bar plot####
unique(flightdata$origin)
ggplot(data = flightdata,mapping= aes(x=origin))+
  geom_bar()
