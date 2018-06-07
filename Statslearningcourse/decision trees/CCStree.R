#load CCS data set using the 'X' program...
library("rpart",lib = 'C:/Program Files/R/R-3.1.2/library')
library("rpart")
library('RcmdrPlugin.BCA')
head(CCS)
set.seed(20)
dim(CCS)
summary(CCS)

train_ind <- sample(seq_len(1600), size = 800)

train <- CCS[train_ind, ]
test <- CCS[-train_ind, ]
head(train)
head(test)

#create decision tree with train data set

tree1CCS<-rpart(MonthGive~.,data=train,control=rpart.control(cp=0.01),method="class")
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(tree1CCS,uniform=TRUE)
text(tree1CCS, use.n = TRUE)
tree1CCS
fancyRpartPlot(tree1CCS,cex=0.75)

#fancy plot
library('rattle', lib = 'C:/Progra~1/R/R-3.1.2/library')
library('rpart.plot ', lib = 'C:/Progra~1/R/R-3.1.2/library')
library('RColorBrewer', lib = 'C:/Progra~1/R/R-3.1.2/library')
library('rpart.plot')
library('rattle')
library('RColorBrewer')

fancyRpartPlot(tree1CCS)

#predict whether a new donor with NewDonor=yes AveDonAmt=19 and AveIncEA=$85000 would join program

new.people<-data.frame("NewDonor"=c("Yes","Yes","Yes"),"AveDonAmt"=c(19,9,22),"AveIncEA"=c(85000,NA,52000),
                       "DonPerYear"=c(0,0.04,0.04), "SomeUnivP"=c(NA,NA,0.25))
mean(train$AveIncEA)
new.people$AveIncEA[is.na(new.people$AveIncEA)]<-mean(train$AveIncEA)
new.people$SomeUnivP[is.na(new.people$SomeUniv)]<-mean(train$SomeUnivP)
new.people
#create new decision tree without missing variables..... 
tree2CCS<-rpart(MonthGive~NewDonor+AveDonAmt+AveIncEA+DonPerYear+SomeUnivP,
                data=train,control=rpart.control(cp=0.01),method="class")
predict.newpeople<-predict(tree2CCS,new.people,type="class")
predict.newpeople
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(tree2CCS)
text(tree2CCS, use.n = TRUE)

#overfitting excercise
#create tree with cp=0.001 and get rid of two strongest variables for some reason
noAorLDA<-rpart(MonthGive~YearsGive+DonPerYear+NewDonor+Age20t29+
                        Age20t39+Age60pls+Age70pls+Age80pls+AdultAge+SomeUnivP+FinUnivP+
                        hh1t2mem+hh1mem+AveIncEA+DwelValEA+EngPrmLang+Region,
                data=train,control=rpart.control(cp=0.001),
                method="class")
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(noAorLDA,uniform=TRUE)
text(noAorLDA, use.n = TRUE)
printcp(noAorLDA)
plotcp(noAorLDA)

#create 3 trees for different R values
noAorLDA016<-rpart(MonthGive~YearsGive+DonPerYear+NewDonor+Age20t29+
                           Age20t39+Age60pls+Age70pls+Age80pls+AdultAge+SomeUnivP+FinUnivP+
                           hh1t2mem+hh1mem+AveIncEA+DwelValEA+EngPrmLang+Region,
                  data=train,
                  control=rpart.control(cp=0.016),method="class")
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(noAorLDA016,uniform=TRUE)
text(noAorLDA016, use.n = TRUE)

noAorLDA013<-rpart(MonthGive~YearsGive+DonPerYear+NewDonor+Age20t29+
                           Age20t39+Age60pls+Age70pls+Age80pls+AdultAge+SomeUnivP+FinUnivP+
                           hh1t2mem+hh1mem+AveIncEA+DwelValEA+EngPrmLang+Region,
                data=train,
                control=rpart.control(cp=0.013),method="class")
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(noAorLDA013,uniform=TRUE)
text(noAorLDA013, use.n = TRUE)

noAorLDA010<-rpart(MonthGive~YearsGive+DonPerYear+NewDonor+Age20t29+
                           Age20t39+Age60pls+Age70pls+Age80pls+AdultAge+SomeUnivP+FinUnivP+
                           hh1t2mem+hh1mem+AveIncEA+DwelValEA+EngPrmLang+Region,
                data=train,
                control=rpart.control(cp=0.010),method="class")
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(noAorLDA010,uniform=TRUE)
text(noAorLDA010, use.n = TRUE)

#compare models with lift chart even though they all look the same.. 

#on training data
lift.chart(modelList=c("noAorLDA016","noAorLDA013","noAorLDA010","noAorLDA"),data=train,
           targLevel="Yes",trueResp=0.01,type="cumulative",sub="testdata")
#on test data
lift.chart(modelList=c("noAorLDA016","noAorLDA013","noAorLDA010","noAorLDA"),data=test,
           targLevel="Yes",trueResp=0.01,type="cumulative",sub="testdata")

#try doing the same as above but use all the variables

Full001<-rpart(MonthGive~YearsGive+AveDonAmt+LastDonAmt+DonPerYear+NewDonor+Age20t29+
                        Age20t39+Age60pls+Age70pls+Age80pls+AdultAge+SomeUnivP+FinUnivP+
                        hh1t2mem+hh1mem+AveIncEA+DwelValEA+EngPrmLang+Region,
                data=train,control=rpart.control(cp=0.001),
                method="class")
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(Full001,uniform=TRUE)
text(Full001, use.n = TRUE)
printcp(Full001)
plotcp(Full001)

#set cp to simplest possible with least xerror
Full001<-rpart(MonthGive~YearsGive+AveDonAmt+LastDonAmt+DonPerYear+NewDonor+Age20t29+
                       Age20t39+Age60pls+Age70pls+Age80pls+AdultAge+SomeUnivP+FinUnivP+
                       hh1t2mem+hh1mem+AveIncEA+DwelValEA+EngPrmLang+Region,
               data=train,control=rpart.control(cp=0.02),
               method="class")
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(tree1CCS,uniform=TRUE)
text(tree1CCS, use.n = TRUE)

#plot next most complex model - cp=0.015
Full05<-rpart(MonthGive~YearsGive+AveDonAmt+LastDonAmt+DonPerYear+NewDonor+Age20t29+
                       Age20t39+Age60pls+Age70pls+Age80pls+AdultAge+SomeUnivP+FinUnivP+
                       hh1t2mem+hh1mem+AveIncEA+DwelValEA+EngPrmLang+Region,
               data=train,control=rpart.control(cp=0.05),
               method="class")
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(Full05,uniform=TRUE)
text(Full05, use.n = TRUE)

Full013<-rpart(MonthGive~YearsGive+AveDonAmt+LastDonAmt+DonPerYear+NewDonor+Age20t29+
                       Age20t39+Age60pls+Age70pls+Age80pls+AdultAge+SomeUnivP+FinUnivP+
                       hh1t2mem+hh1mem+AveIncEA+DwelValEA+EngPrmLang+Region,
               data=train,control=rpart.control(cp=0.013),
               method="class")
par(mfrow=c(1,1),xpd=NA,cex=.75)
plot(Full013,uniform=TRUE)
text(Full013, use.n = TRUE)

#compare on lift chart - tree models including all the variables
lift.chart(modelList=c("Full001","Full05","Full013","noAorLDA"),data=test,
           targLevel="Yes",trueResp=0.01,type="cumulative",sub="testdata")
