
#Reading csv dataset
df<-read.csv("F:\\ISB\\2 - Term 2 - (18-22 June) - H\\Statistical Analysis - 2\\SA2 Assignment - 23 July\\Wage2.csv")

#EDA
head(df,10)
dim(df)
str(df)
summary(df)
n = nrow(df)

#Covariance
cov(df$wage,df$IQ)

#Correlation
cor(df$IQ,df$wage)

#Scatterplot Matrix
pairs(df)

#Correlation Matrix
library(psych)
corPlot(df)

#Model 1
m1 <-lm(df$wage ~ df$IQ)
attributes(m1)
summary(m1)

#Plot
plot(df$IQ,df$wage)
abline(m1, col="red")

par(mfrow=c(2,2))
plot(m1)

#Hypothesis Testing
t_b1 = 8.3031/0.8364 
t_b1
t_LOS = qt(0.05, df=n-2)*-1
t_LOS

#Model 2
m2 <- lm(df$wage ~ df$IQ+df$educ)
summary(m2)

#OMV - Regressing education on IQ
summary(m2)
'y=beta0+beta1^*IQ+beta2^*educ'

m3 <- lm(df$educ ~ df$IQ) #Regressing education on IQ 
'y=delta0^ + delta1~ *IQ'
summary(m3)

m4 <- lm(df$wage ~ df$educ)
'y=beta0+ beta^~ * educ'
summary(m4)

#beta^~ is similar to Beta1^ + Beta2^ * delta1~
#60.214 = 5.1380 + 42.0576 * 0.075256

#Please note that this variable can be inverted if we wish to get Regression of IQ on education, like so - 

m5 <- lm(df$IQ ~ df$educ) #Regressing education on IQ 
'y=delta0^ + delta1~ *educ'
summary(m5)

#beta^~ is similar to Beta1^ + Beta2^ * delta1~
#8.3031 = 42.0576 + 5.1380 * 3.5338


#Q12
head(df,1)
wage_1st_ind = -128.8899 + 5.1380 * 93 + 42.0576 * 12
wage_1st_ind

