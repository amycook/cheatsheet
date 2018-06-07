setwd("C:/Users/n9232371/OneDrive/shared files/Statslearningcourse/SVR")
library('e1071', lib = 'C:/Progra~1/R/R-3.2.1/library')

df<- read.csv('regression.csv')
head(df)
plot(df, pch=16)
model<- lm(Y~X, data=df)
abline(model)

lmY<- predict(model, df)

#add predicted points to plot
plot(df, pch=16)
points(df$X, lmY, col='blue', pch=4)

#look at residuals
error<- model$residuals
RMSE<- sqrt(mean(error^2))

### NOW LETS DO SVR AND COMPARE!!

model<- svm(Y~X, data=df)
svmY<- predict(model, df)
plot(df, pch=16)
points(df$X, svmY, col='blue', pch=4)

error.svm<- df$Y - svmY
RMSE.svm<- sqrt(mean(error.svm^2))

#tuning SVM!!

tuneResult<- tune( svm, Y~X, data=df, 
                   ranges= list(epsilon = seq(0,1,0.1), cost= 2^(2:9)))
print(tuneResult)
plot(tuneResult)

#try again, keep epsilons between 0 and 0.2
tuneResult<- tune( svm, Y~X, data=df, 
                   ranges= list(epsilon = seq(0,.2,0.01), cost= 2^(2:9)))
print(tuneResult)
plot(tuneResult)

tunedModel<- tuneResult$best.model
tunedY<- predict(tunedModel, data=df)
error.tuned<- df$Y-tunedY
RMSE.tuned<- sqrt(mean(error.tuned^2))

#visualise

points(df$X, tunedY, col='red', pch=4)


