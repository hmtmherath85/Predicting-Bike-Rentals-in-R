# STAT 474 - Project - Summer 2021

setwd("E:/Academic_WIU/Summer_2021/STAT474G/Project")

data_bike <- read.csv("Bike_day.csv") #reading as csv file
attach(data_bike) # attaching data
data_bike


# assigning dummies for categorical data
data_bike$season <- factor(data_bike$season, levels=1:4, labels=c("winter", "spring",
                                                                  "summer", "fall)"))
data_bike$holiday <-as.factor(data_bike$holiday)
data_bike$weekday <-as.factor(data_bike$weekday)
data_bike$workingday <- as.factor(data_bike$workingday)
data_bike$weathersit <- factor(data_bike$weathersit, levels=1:4, labels=c("Clear","Mist",
                                                                  "LightSnow", "Heavy Rain"))
dev.new(width=6,height=6)
par(mfrow = c(3, 3))
#plot(data_bike)
plot(data_bike$season,data_bike$cnt, xlab='season', ylab='count')
plot(data_bike$holiday,data_bike$cnt, xlab = 'holiday', ylab='count')
plot(data_bike$weathersit,data_bike$cnt,xlab='weather_situation', ylab='count')
plot(data_bike$temp,data_bike$cnt, xlab='temp', ylab='count')
plot(data_bike$hum,data_bike$cnt, xlab='humidity', ylab='count')
plot(data_bike$windspeed,data_bike$cnt,xlab='wind_speed', ylab='count')
plot(data_bike$atemp,data_bike$cnt, xlab='feeling_temp', ylab='count')
plot(data_bike$weekday,data_bike$cnt, xlab='weekday', ylab='count')
plot(data_bike$workingday,data_bike$cnt, xlab='workingday', ylab='count')


# fitting poisson regression
fit_bike <- glm(cnt~season+holiday+weekday+weathersit+temp+atemp+hum+windspeed,
                    family = poisson, data = data_bike)
summary(fit_bike)

# checking multicolinearity of x variables
vif(fit_bike)

# remove 'atemp' from the model
fit_bike_red <- update(fit_bike, . ~ . - atemp)
#fit_bike_red <- glm(cnt~season+holiday+weekday+weathersit+temp+hum+windspeed,family = poisson, data = data_bike)
summary(fit_bike_red)

vif(fit_bike_red) # checking multicolinearity of X variables

#If Residual Deviance/df >1, then overdispersion 
deviance(fit_bike_red)/df.residual(fit_bike_red)

# fixed overdispersion with negative binomial
nb_fit_bike <- glm.nb(cnt~season+holiday+weekday+weathersit+temp+hum+windspeed, 
                      data = data_bike)
deviance(nb_fit_bike)/df.residual(nb_fit_bike)
summary(nb_fit_bike)

# fiding the best model with all possible variables 
#install.packages(MuMIn)
library(MuMIn)
nb_fit_bike2 <- glm.nb(cnt~season+holiday+weekday+weathersit+temp+hum+windspeed, 
                      data = data_bike, na.action = na.pass)
nb_fit_best <- dredge(nb_fit_bike2)
head(nb_fit_best)

# fit the model excluding 'weekday'
# nb_fit_bike3 <- glm.nb(cnt~season+holiday+weathersit+temp+hum+windspeed,data = data_bike)

nb_fit_bike3 <- update(nb_fit_bike, . ~ . - weekday)
anova(nb_fit_bike, nb_fit_bike3)

summary(nb_fit_bike3)

deviance(nb_fit_bike3)/df.residual(nb_fit_bike3) # checking overdispersion parameter

##Confidence Intervals
cbind(coef(nb_fit_bike3),confint.default(nb_fit_bike3,level=0.95))  #Confidence intervals for beta parameters in log[E(Y)]
exp(cbind(oddR = coef(nb_fit_bike3),confint.default(nb_fit_bike3,level=0.95))) ## odd ratios, Conf Intervals of beta parameters


###Finding Pearson Goodness of Fit Test( or lack of fit)
yhat<-nb_fit_bike3$fitted.values  ##The same fitted values
pv1<-1-pchisq(sum((data_bike$cnt-yhat)^2/yhat),df.residual(nb_fit_bike3)) #Pearson's GoodNess of Fit Test
pv1

pchisq(2 * (logLik(fit_bike) - logLik(nb_fit_bike3)), df = 1, lower.tail = FALSE)

##McFadden's R2

r2=with(nb_fit_bike3,1-deviance/null.deviance) #Produced McFadden's R2...
r2


# Residual analysis

dev.new(width=5,height=4)
par(mfrow = c(2, 2))

#Stadardized residuals and plots for outlier detection
rs=rstandard(nb_fit_bike3)
plot(rs,ylim=c(-3,3),ylab = "Standardized Residuals")
abline(h=c(-2,2),col="red",lty=2)

#Studentized residuals and plots for outlier detection
rst=rstudent(nb_fit_bike3)
plot(rst,ylim=c(-3,3),ylab="Studentized residuals")
abline(h=c(-2,2),col="red",lty=2)

###Leverage values h_ii from H matrix and plotting them
n=dim(data_bike)[1]
p=dim(data_bike)[2]
h1=3*(p/n)
hii=hatvalues(nb_fit_bike3)
plot(hii,ylim=c(0,2))
abline(h=h1,col="red",lty=2)


###Finding Cook's Distance for possible influencial points in the data
ck=cooks.distance(nb_fit_bike3)
plot(ck,ylim=c(0,2))
abline(h=0.5,col="red",lty=2)

