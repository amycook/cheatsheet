## perform ROLL~variable individually for YEAR HGRAD and UNEM. 
# 1/2 NA values in HGRAD and UNEM
# predict to find response for full data set
# use linear regression to re-combine and find weights for each variable.
#why not just impute using linear regression? hm

setwd("~/OneDrive/shared files/Statslearningcourse/multi step regression")
library("plyr")
library("dplyr")
library('magrittr')

roll<- read.csv('roll.csv')
dim(roll)

#first check predictive power for 3 variables, this is what i want to look at now
# YEAR HGRAD AND UNEM to predict ROLL
firstup<- lm(ROLL~ YEAR + HGRAD + UNEM, data=roll)
rollers<- predict(firstup, roll)
RMSE(roll$ROLL, rollers)

summary(roll)
#choose 1/2 of HGRAD to be NA, other half to be UNEM NA
NAs<- sample(nrow(roll), round(1/2*nrow(roll),0), replace=F)
roll$HGRAD[NAs]<- NA
roll$UNEM[-NAs]<- NA
#scatter plot matrix
pairs(~., data=roll)

#try lm for predicting ROLL given year

first<- lm(ROLL~YEAR, data=roll)
summary(first)  
plot(ROLL~YEAR, data=roll)
abline(first)
#str(first)  
#first$residuals

#try HGRAD

#HGRAD alone
second<- lm(ROLL~HGRAD, data=roll)
summary(second)
plot(ROLL~HGRAD, data=roll)
abline(second)

#UNEM alone
third<- lm(ROLL~UNEM, data=roll)
summary(third)
plot(ROLL~UNEM, data=roll)
abline(third)

#find answers, compare multiple methods
#first find answers for and final error for 3 alternate methods

#using 'second'
full<- read.csv('roll.csv')
ans<- NULL
ans$answer<- full$ROLL
ans<- as.data.frame(ans)
ans$first.lm<- predict(first, full)
ans$second.lm<- predict(second, full)
ans$third.lm<- predict(third, full)


#check RMSE's

RMSE<- function(answer, guess){
  error= answer-guess
  root.error= sqrt(mean(error^2))
  print(root.error)
}
RMSE(ans$answer, ans$first.lm)
RMSE(ans$answer, ans$second.lm)
RMSE(ans$answer, ans$third.lm)

#now combine answers from first.lm, second.lm, third.lm to find weights
final.lm<- lm(answer~first.lm+second.lm+third.lm, data=ans)
ans$final.lm<- predict(final.lm, ans)

RMSE(ans$answer, ans$final.lm)

