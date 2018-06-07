## repeat regression experiment with NA values (1/2) in HGRAD (1/2) in INC
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
first$residuals

#add residuals to roll dataframe
roll$resid1<- first$residuals

#fit new lm to residuals and add more data so that you dont end up with a horizontal line!
pairs(~., data=roll)

#try HGRAD

#HGRAD alone
second<- lm(resid1~HGRAD + YEAR, data=roll)
summary(second)
plot(resid1~HGRAD, data=roll)
abline(second)
#make residuals 2 vector with resid1 where NA's should be
resid2<-  ifelse(rownames(roll) %in% names(second$residuals), second$residuals, first$residuals)
roll$resid2<- resid2

#UNEM alone
third<- lm(resid2~UNEM + YEAR, data=roll)
summary(third)
plot(resid1~UNEM, data=roll)
abline(third)


#find answers, compare multiple methods
#first find answers for and final error for 3 alternate methods

#using 'second'
full<- read.csv('roll.csv')
ans<- NULL
ans$answer<- full$ROLL
ans<- as.data.frame(ans)
ans$first.lm<- predict(first, full)
ans$second.lm<- predict(first, full) + predict(second, full)
ans$third.lm<- predict(first, full) + predict(third, full)
ans$test.lm<- predict(first, full) + predict(third, full) + predict(second, full)



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