## regression 
setwd("~/OneDrive/shared files/Statslearningcourse/multi step regression")
roll<- read.csv('roll.csv')
dim(roll)

summary(roll)
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
ans<- NULL
ans$answer<- roll$ROLL
ans<- as.data.frame(ans)
ans$first.lm<- predict(first, data=roll)
ans$second.lm<- predict(first, data=roll) + predict(second, data=roll)
ans$third.lm<- predict(first, data=roll) + predict(third, data=roll)
ans$test.lm<- predict(test, data=roll)



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






  