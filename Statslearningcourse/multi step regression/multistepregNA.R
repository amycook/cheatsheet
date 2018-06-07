## repeat regression experiment with NA values (1/3) in HGRAD 
setwd("~/OneDrive/shared files/Statslearningcourse/multi step regression")
library("plyr")
library("dplyr")
library('magrittr')

roll<- read.csv('roll.csv')
dim(roll)

summary(roll)
#choose 1/3 of HGRAD to be NA
NAs<- sample(nrow(roll), round(1/3*nrow(roll),0), replace=F)
roll$HGRAD[NAs]<- NA
#scatter plot matrix
pairs(~., data=roll)

#try lm for predicting ROLL given year

first<- lm(ROLL~YEAR, data=roll)
summary(first)  
plot(ROLL~YEAR, data=roll)
abline(first)
str(first)  
first$residuals

#add residuals to roll dataframe
roll$resid1<- first$residuals

#fit new lm to residuals and add more data so that you dont end up with a horizontal line!
pairs(~., data=roll)

#try HGRAD

#HGRAD alone
second<- lm(resid1~HGRAD, data=roll)
summary(second)
plot(resid1~HGRAD, data=roll)
abline(second)

#HGRAD alone
third<- lm(resid1~HGRAD + YEAR, data=roll)
summary(third)
plot(resid1~HGRAD, data=roll)
abline(third)

#test to see what would happen if we did YEAR + HGRAD to begin with

test<- lm(ROLL~YEAR + HGRAD, data=roll)
summary(test)

#find answers, compare multiple methods
#first find answers for and final error for 3 alternate methods

#using 'second'
reduced<- roll[complete.cases(roll),]
ans<- NULL
ans$answer<- reduced$ROLL
ans<- as.data.frame(ans)
ans$first.lm<- predict(first, reduced)
ans$second.lm<- predict(first, reduced) + predict(second, reduced)
ans$third.lm<- predict(first, reduced) + predict(third, reduced)
ans$test.lm<- predict(test, reduced)



#check RMSE's

RMSE<- function(answer, guess){
  error= answer-guess
  root.error= sqrt(mean(error^2))
  print(root.error)
}
RMSE(ans$answer, ans$first.lm)
RMSE(ans$answer, ans$second.lm)
RMSE(ans$answer, ans$third.lm)
RMSE(ans$answer, ans$test.lm)

#repeat experiment with NA values in 