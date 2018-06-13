# glmnet lasso

library(glmnet)

data(QuickStartExample)

# to convert a dataframe to matrix:
mf <- model.frame(Act_Cu_Rec ~. -rowname, data=r)
y = model.response(mf)
x = model.matrix(Act_Cu_Rec ~. -rowname, mf)

fit <- glmnet(x,y)
plot(fit, label = TRUE)
print(fit)

#check the coefficients at a certain lambda. see which vars have been eliminated
coef(fit,s=0.1) #s = lambda

#predict
nx = matrix(rnorm(10*20),10,20)
predict(fit,newx=nx,s=c(0.1,0.05))

#cross validation
cvfit = cv.glmnet(x, y, nfolds = 10)
plot(cvfit)

# vertical lines
#lambda of left hand line - min MSE
cvfit$lambda.min
#coef of left hand line model
coef(cvfit, s = "lambda.min")
# predict with min lambda
predict(cvfit, newx = x[1:5,], s = "lambda.min")
# right hand line is 'lambda.1se', or 1 standard error above lambda min


##
## BINOMIAL
##

data(BinomialExample)
# y is integers 0 and 1
fit = glmnet(x, y, family = "binomial")
plot(fit, xvar = "dev", label = TRUE)
# example predictions using lambda = 0.05, 0.01
predict(fit, newx = x[1:5,], type = "response", s = c(0.05, 0.01))

cvfit = cv.glmnet(x, y, family = "binomial", type.measure = "auc", nfolds = 10)
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se
coef(cvfit, s = "lambda.min")
coef(cvfit, s = "lambda.1se")
predict(cvfit, newx = x[1:10,], s = "lambda.min", type = "response")
