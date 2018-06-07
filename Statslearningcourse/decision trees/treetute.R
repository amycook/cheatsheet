install.packages('tree')
library('ISLR', lib = 'C:/Progra~1/R/R-3.1.2/library')
library('tree', lib = 'C:/Progra~1/R/R-3.1.2/library')
library('ISLR')
library('tree')
library('gbm')
attach(Carseats)
head(Carseats)
hist(Sales)
dim(Carseats)
Carseats$High<-ifelse(Sales>8,"Yes","No")
tree.carseats<-tree(as.factor(High)~.-Sales,data=Carseats)
summary(tree.carseats)
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

#create train and test set
set.seed(1011)
train_ind <- sample(seq_len(400), size = 250)

train <- Carseats[train_ind, ]
test <- Carseats[-train_ind, ]

#create tree with training data only
tree.carseats<-tree(as.factor(High)~.-Sales,data=train)
summary(tree.carseats)
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.pred<-predict(tree.carseats, test,type='class')
#create confusion table:
with(test,table(tree.pred,High))
#find error rate
#(true positives+true negatives)/total # data points
(78+40)/150
#error rate is 1-.7867= 0.2133
1-.7867
#use cross validation to prune the tree. cv.tree does ten fold cross validation
cv.carseats<-cv.tree(tree.carseats,FUN=prune.misclass)
cv.carseats
plot(cv.carseats)
#plot plots the deviance against size.
#prune tree to size where deviance bottoms out--- 13
prune.carseats<-prune.misclass(tree.carseats,best=13)
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(prune.carseats)
text(prune.carseats,pretty=0)
prune.pred<-predict(prune.carseats, test,type='class')
#create confusion table:
with(test,table(prune.pred,High))
#(true positives+true negatives)/total # data points
(81+40)/150
#error rate is 1-.806667 = .193333
#not much of an improvement.. lets try random forests and boosting!!!
library(MASS)
set.seed(101)
dim(Boston)
head(Boston)
train_ind <- sample(seq_len(506), size = 300)

train <- Boston[train_ind, ]
test <- Boston[-train_ind, ]
#try to predict medv
rf.boston<-randomForest(medv~.,data=train)
rf.boston
#create vectors full of '0's
oob.err<-double(13)
test.err<-double(13)
for(mtry in 1:13){
        fit<-randomForest(medv~.,data=train,mtry=mtry,ntree=400)
        oob.err[mtry]<-fit$mse[400]
        test.err[mtry]<- mean((predict(fit,test)-test$medv)^2)
        cat(mtry," ")
}

qplot(1:mtry,test.err,geom=c("point","line"),color="pink")+
        geom_line(aes(1:mtry,oob.err),colour="blue")+
        geom_point(aes(1:mtry,oob.err),colour="blue")

#say m=7 is best :)!! now move onto finding variable importance
Boston.m7<-randomForest(medv~.,data=train,mtry=7,ntree=400,importance=TRUE)
impo<-as.data.frame(importance(Boston.m7))
order(impo$IncNodePurity)
impo<-impo[order(impo$IncNodePurity,decreasing=TRUE),]
impo

###now lets try boosted trees
boost.boston<-gbm(medv~.,data=train,distribution="gaussian",n.trees=5000,
                  shrinkage=0.01,interaction.depth=4,cv.folds=4)
summary(boost.boston)
boost.boston
plot(boost.boston,i="lstat")
plot(boost.boston,i="rm")

#run cross validation to choose number of trees - we must do that ourselves
gbm.treeperf<-gbm.perf(boost.boston,method="cv")
gbm.treeperf
#use cross validation to choose shrinkage parameter - another tuning parameter
#0.01 seems to be better than 0.001.. just playng with the formula.
#create vectors full of '0's
test.err<-double(5)
shrink.seq<-c(.001,.005,.01,.05,.1)
for(i in 1:5){
        fit<-gbm(medv~.,data=train,distribution="gaussian",n.trees=5000,
                 shrinkage=shrink.seq[i],interaction.depth=4,cv.folds=4)
        test.err[i]<- mean((predict(fit,test,n.tree=5000)-test$medv)^2)
        cat(i," ")
}
test.err
qplot(shrink.seq,test.err,geom=c("point","line"),color="pink")
#shows that shrinkage=0.01 is optimum, therefore keep boost.boston as is.

#need to choose how many trees to use in the prediction function as well! 
#lets try 100-10000
n.trees<-seq(from=100,5000,by=100)
#if predict with lots and lots of #tree options, predict function creates a matrix with
#all the tree# options

predmat<-predict(boost.boston,test,n.trees=n.trees)
dim(predmat)
#a matrix minus a vector recycles the vector: trick
Errormat<-(predmat-test$medv)^2
head(Errormat)
perr<-apply(Errormat,2,mean)
length(perr)
head(perr)
qplot(n.trees,perr,geom=c("line","point"))
perr[50]

ggplot
