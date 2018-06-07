library('magrittr',lib='C:/Progra~1/R/R-3.2.1/library')
library('plyr',lib='C:/Progra~1/R/R-3.2.1/library')
library('dplyr',lib='C:/Progra~1/R/R-3.2.1/library')
library('caret',lib='C:/Progra~1/R/R-3.2.1/library')
library('FNN',lib='C:/Progra~1/R/R-3.2.1/library')

#telecom churn dataset
library('C50', lib = 'C:/Progra~1/R/R-3.2.1/library')
data(churn)
str(churnTrain)
summary(churnTrain)
churnTrain<- churnTrain %>% select(-state, -area_code, -account_length)

set.seed(2)
#create training and testing set 
ind= sample(2, nrow(churnTrain), replace=TRUE, prob=c(0.7,0.3))
trainset<- churnTrain[ind==1,]
testset<- churnTrain[ind==2,]
dim(trainset)
dim(testset)

#replace yes and no of the voice_mail_plan and international_plan witih 1 and 0
levels(trainset$international_plan) = list('0' = 'no', '1'='yes')
levels(testset$international_plan) = list('0' = 'no', '1'='yes')
levels(trainset$voice_mail_plan) = list('0' = 'no', '1'='yes')
levels(testset$voice_mail_plan) = list('0' = 'no', '1'='yes')
trainset$international_plan<- as.numeric(trainset$international_plan)
testset$international_plan<- as.numeric(testset$international_plan)
testset$voice_mail_plan<- as.numeric(testset$voice_mail_plan)
trainset$voice_mail_plan<- as.numeric(trainset$voice_mail_plan)

churn.knn = knn(trainset %>% select(-churn), testset %>% select(-churn),
                trainset$churn, k=3)
summary(churn.knn)
table(churn.knn,testset$churn)
# below doesnt work 
table(testset$churn, churn.knn) %>% confusionMatrix 

indices<- attr(churn.knn,  "nn.index")
dim(indices)
length(testset$churn)


