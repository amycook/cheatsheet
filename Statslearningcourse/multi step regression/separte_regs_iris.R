#work with iris dataset. we shall predict Petal.Width

setwd("~/OneDrive/shared files/Statslearningcourse/multi step regression")
library("plyr")
library("dplyr")
library('magrittr')

#predict Petal.Width using lm, find RMSE
RMSE<- function(answer, guess){
  error= answer-guess
  root.error= sqrt(mean(error^2))
  print(root.error)
}

#standard method with all vars in
pairs(~., data=iris)
firstup<- lm(Petal.Width~ Petal.Length + Sepal.Width + Sepal.Length + Species, data=iris)
rollers<- predict(firstup, iris)
RMSE(iris$Petal.Width, rollers)

#enforce no complete cases
#Petal.Length can stay full and Sepal.Width
#Sepal.Length and Species will both lose half their data
reduce<- iris
NAs<- sample(nrow(reduce), round(1/2*nrow(reduce),0), replace=F)
reduce$Sepal.Length[NAs]<- NA
reduce$Species[-NAs]<- NA
summary(reduce)

#now fit full variables, followed by incomplete variables individually

#full variables
first<- lm(Petal.Width~Petal.Length + Sepal.Width, data=reduce)
summary(first)  
plot(Petal.Width~Petal.Length + Sepal.Width, data=reduce)
abline(first)

#Sepal.Length - half NA
second<- lm(Petal.Width ~ Sepal.Length, data=reduce)
summary(second)
plot(Petal.Width ~ Sepal.Length, data=reduce)
abline(second)

#Species - half missing
# 3 separate species 
summary(reduce$Species)
third<- lm(Petal.Width ~ Species, data=reduce)
summary(third)
plot(Petal.Width ~ Species, data=reduce)

#compile results of all lm's
ans<- NULL
ans$answer<- iris$Petal.Width
ans<- as.data.frame(ans)
ans$first.lm<- predict(first, iris)
ans$second.lm<- predict(second, iris)
ans$third.lm<- predict(third, iris)


#check RMSE's

RMSE(ans$answer, ans$first.lm)
RMSE(ans$answer, ans$second.lm)
RMSE(ans$answer, ans$third.lm)

#now combine answers from first.lm, second.lm, third.lm to find weights
final.lm<- lm(answer~first.lm+second.lm+third.lm, data=ans)
final<- predict(final.lm, ans)
ans$final.lm<- final

RMSE(ans$answer, ans$final.lm)







