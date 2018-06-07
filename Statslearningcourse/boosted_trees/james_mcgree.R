## For James McGree/Orica

library(randomForest)
library(caret)
library(gbm)
library(dplyr)


#create train/test set: k = 5 folds
# use smaller diamonds set for demo - this stuff takes a while
diamonds <- sample_n(diamonds, 5000)
#best to normalise price first - would usually normalise other variables too
diamonds$log.price <- log(diamonds$price)
folds <- createFolds(diamonds$price, k=5, list=T, returnTrain=F)
train <- diamonds[-folds[[1]],]
test <- diamonds[folds[[1]],]


#################
# Random Forest #
#################

#perform 5 fold xval using caret package to tune 'mtry' parameter
# use caret package with 'train' dataset. Build actual forest with test dataset once tuning parameters found
# caret only allows you to tune mtry, can't tune ntree because shouldn't matter - the more the better.
tc = trainControl("cv",5)
rf.grid = expand.grid(mtry= 1:9)
train.rf = train(log.price~. -price, data = train, method="rf",trControl=tc,
                 tuneGrid=rf.grid)
#plot tuning results
train.rf
plot(train.rf)
# seems like the more the better with mtry in this one - use mtry = 9

#make first single randomForest
# I've used the train data instead of test becasue just looking at variable importance. def need test for predictions
first.rf<- randomForest(log.price~. -price,
                        data= train, mtry=9, ntree=500, importance=TRUE)

# interrogate forest
plot(first.rf)
importance(first.rf, type = 1) # type 1 is the difference in MSE if you permute the variable. higher = more important
varImpPlot(first.rf, type = 1)



#################
# Boosted Trees #
#################


cv.Control <- trainControl(method = "cv",
                           number = 5,
                           summaryFunction= defaultSummary)

gbmGrid <- expand.grid(shrinkage = c(0.0005, 0.001, 0.005, 0.01),
                       n.trees = c(4000,6000,8000,10000), interaction.depth = c(3),
                       n.minobsinnode = c(20))
gbmFit <- train(log.price~. -price, data = train,
                method = "gbm", trControl = cv.Control, verbose = FALSE,
                bag.fraction = 0.5, tuneGrid = gbmGrid,
                metric = 'RMSE',
                na.action = na.pass) # na.action = pass means the missing variables are passed over on a tree by tree basis so overall all cases are included 

plot(gbmFit)
plot(gbmFit, plotType = 'level')
# choose shrinkage = 0.01, iterations = 6000 (dont see much benefit in doing 10,000 trees and a lot more comp expense)


gbmGrid <- expand.grid(shrinkage = c(0.01),
                       n.trees = c(6000), interaction.depth = c(1,2,3,4,5),
                       n.minobsinnode = c(10,20,30))
gbmFit2 <- train(log.price~. -price, data= train,
                method = "gbm", trControl = cv.Control, verbose = FALSE,
                bag.fraction = 0.5, tuneGrid = gbmGrid,
                metric = 'RMSE',
                na.action = na.pass)

plot(gbmFit2)
plot(gbmFit2, plotType = 'level')

#choose min node size = 10, and max tree depth = 5

first.boost <- gbm(log.price~. -price, 
               distribution = "gaussian", data=test,
               n.trees = 6000,
               shrinkage = 0.01,
               interaction.depth = 5,
               n.minobsinnode = 10,
               bag.fraction = 0.5)

# variable importance
print(first.boost)
summary(first.boost)
