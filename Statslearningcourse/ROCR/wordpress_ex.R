# You only need to install packages once per machine
# (plus maybe after upgrading R), but otherwise they persist across R sessions.
library('party',lib='C:/Progra~1/R/R-3.2.0/library')
library('ROCR',lib='C:/Progra~1/R/R-3.2.0/library')
library('rpart',lib='C:/Progra~1/R/R-3.2.0/library')
library('pROC', lib = 'C:/Progra~1/R/R-3.2.2/library')



# Split randomly
x <- kyphosis[sample(1:nrow(kyphosis), nrow(kyphosis), replace = F),]
x.train <- kyphosis[1:floor(nrow(x)*.75), ]
x.evaluate <- kyphosis[(floor(nrow(x)*.75)+1):nrow(x), ]

# Create a model using "random forest and bagging ensemble algorithms
# utilizing conditional inference trees."

x.model <- cforest(Kyphosis ~ Age + Number + Start, data=x.train,
                   control = cforest_unbiased(mtry = 3))

# Alternatively, use "recursive partitioning [...] in a conditional
# inference framework."
# x.model <- ctree(Kyphosis ~ Age + Number + Start, data=x.train)

# ctree plots nicely (but cforest doesn"t plot)
# plot (x.model)

# Use the model to predict the evaluation.
x.evaluate$prediction <- predict(x.model, newdata=x.evaluate)

# Calculate the overall accuracy.
x.evaluate$correct <- x.evaluate$prediction == x.evaluate$Kyphosis
print(paste("% of predicted classifications correct", mean(x.evaluate$correct)))

# Extract the class probabilities.
x.evaluate$probabilities <- 1- unlist(treeresponse(x.model,
                                                   newdata=x.evaluate), use.names=F)[seq(1,nrow(x.evaluate)*2,2)]

# Plot the performance of the model applied to the evaluation set as
# an ROC curve.
require(ROCR)
pred <- prediction(x.evaluate$probabilities, x.evaluate$Kyphosis)
perf <- performance(pred,"tpr","fpr")
plot(perf, main="ROC curve", colorize=T)
auc<- performance(pred, "auc")
auc@y.values[[1]]


# And then a lift chart
perf <- performance(pred,"lift","rpp")
plot(perf, main="lift curve", colorize=T)

#try making ROC curve wiht pROC package

x.evaluate$Kyphosis<- factor(x.evaluate$Kyphosis, levels = c("absent", "present"), ordered= TRUE)
pred.p<- roc(x.evaluate$Kyphosis, x.evaluate$probabilities,
             levels = c("absent", "present"))
plot(pred.p)
auc(pred.p)


