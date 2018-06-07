#load CCS data set using the 'X' program...
head(CCS)
set.seed(1)
dim(CCS)

train_ind <- sample(seq_len(1600), size = 800)

train <- CCS[train_ind, ]
test <- CCS[-train_ind, ]
head(train)
head(test)

#change Yes and No to ) 1 and 0
train$MonthGiveLog<-train$MonthGive
train$MonthGiveLog<-as.character(train$MonthGiveLog)
train$MonthGiveLog[train$MonthGive=="Yes"]<-1
train$MonthGiveLog[train$MonthGive=="No"]<-0

test$MonthGiveLog<-test$MonthGive
test$MonthGiveLog<-as.character(test$MonthGiveLog)
test$MonthGiveLog[test$MonthGive=="Yes"]<-1
test$MonthGiveLog[test$MonthGive=="No"]<-0
head(as.numeric(test$MonthGiveLog))
train$MonthGiveLog<-as.numeric(train$MonthGiveLog)
test$MonthGiveLog<-as.numeric(test$MonthGiveLog)
qplot(AveDonAmt,MonthGiveLog,data=train)

#bin AveDonAmt into 4 equal sized bins
train$AveDonBin<-(cut_number(train$AveDonAmt,n=4))
head(train$AveDonBin)
bwplot(AveDonAmt~AveDonBin,data=train)
boxplot(MonthGiveLog~AveDonBin,data=train)
summary(cut_number(train$AveDonAmt,n=4))

qplot(AveDonBin, MonthGiveLog,data=train) + 
        stat_summary(fun.data="mean_cl_boot",colour="red")

#plot Region vs AveDonBin
qplot(Region, MonthGiveLog,data=train) + 
        stat_summary(fun.data="mean_cl_boot",colour="red")

#plot YearsGive vs AveDonAmt
qplot(YearsGive, AveDonAmt,data=train) +
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="line")
#YearsGive doesn't really affect how much they donate

#plot YearsGive vs MonthGiveLog
qplot(YearsGive, MonthGiveLog,data=train) +
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="line")
#YearsGive has a parabolic relationship with MonthGiveLog - do they convert to monthly giving?

#plot AveDonAmt vs LastDonAmt
qplot(AveDonAmt, LastDonAmt,data=train)+geom_abline(intercept = 0,slope=1)
#pretty evenly spread around 45 degree line.
#plot LastDonAmt vs MonthGiveLog
qplot(cut(LastDonAmt,6), MonthGiveLog,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red")
#appears to follow a logistic curve, the higher the last amount donated, the more likely to change to monthly

#plot DonPerYear vs MonthGiveLog
qplot(cut(DonPerYear,6), MonthGiveLog,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red")
#seems to follow a logisit curve. makes sense. Ie the more times you give per year on average,
#the more you are likely to switch to a monthly donating program.

#plot DonPerYear vs AveDonAmt
qplot(cut(DonPerYear,6), AveDonAmt,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red")
table(cut(train$DonPerYear,6))
qplot(cut_number(DonPerYear,6), AveDonAmt,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red")
table(cut_number(train$DonPerYear,6))
#slightly increase total AveDonAmt with higher number of donations per year

#plot NewDonor vs AveDonAmt
qplot(NewDonor, AveDonAmt,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
#plot NewDonor vs MonthGiveLog
qplot(NewDonor, MonthGiveLog,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red")
table(train$NewDonor)
#13 new donors. 100% converted to monthly payments. maybe this was the only option? 
#just under 50% of old donors coverted.. thats how we selected our data.....

#plot Age percentages vs AveDonAmt
p1<-qplot(cut_number(Age20t29,6), AveDonAmt,data=train,stat="summary",fun.y="mean")+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
p2<-qplot(cut_number(Age20t39,6), AveDonAmt,data=train,stat="summary",fun.y="mean")+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
p3<-qplot(cut_number(Age60pls,6), AveDonAmt,data=train,stat="summary",fun.y="mean")+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
p4<-qplot(cut_number(Age70pls,6), AveDonAmt,data=train,stat="summary",fun.y="mean")+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
p5<-qplot(cut_number(Age80pls,6), AveDonAmt,data=train,stat="summary",fun.y="mean")+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
library("gridExtra",lib = 'C:/Progra~1/R/R-3.1.2/library')
grid.arrange(p1,p2,p3,p4,p5,ncol=2)

#plot Age percentages vs MonthGiveLog
p6<-qplot(cut_number(Age20t29,6), MonthGiveLog,data=train,stat="summary",fun.y="mean")+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
p7<-qplot(cut_number(Age20t39,6), MonthGiveLog,data=train,stat="summary",fun.y="mean")+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
p8<-qplot(cut_number(Age60pls,6), MonthGiveLog,data=train,stat="summary",fun.y="mean")+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
p9<-qplot(cut_number(Age70pls,6), MonthGiveLog,data=train,stat="summary",fun.y="mean")+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
p10<-qplot(cut_number(Age80pls,6), MonthGiveLog,data=train,stat="summary",fun.y="mean")+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
grid.arrange(p6,p7,p8,p9,p10,ncol=2)
#old age seems to deter people from donating much and from joining the monthly giving

#plot AdultAge vs AveDonAmt
qplot(cut_number(AdultAge,6), AveDonAmt,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
#plot AdultAge vs MonthGiveLog
qplot(cut_number(AdultAge,6), MonthGiveLog,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red")
#age is inversely related to probability of converting, and amount donating

#plot AveIncEA vs AveDonAmt
qplot(cut_number(AveIncEA,6), AveDonAmt,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
#plot AveIncEA vs MonthGiveLog
qplot(cut_number(AveIncEA,6), MonthGiveLog,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red")
#Average Income doesnt seem to affect anything

#plot DwelValEA vs AveDonAmt
qplot(cut_number(DwelValEA,6), AveDonAmt,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
#plot DwelValEA vs MonthGiveLog
qplot(cut_number(DwelValEA,6), MonthGiveLog,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red")
#Average Income doesnt seem to affect anything

#plot EngPrmLang vs AveDonAmt
qplot(cut(EngPrmLang,6), AveDonAmt,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red",geom="crossbar",width=0.2)
#plot EngPrmLang vs MonthGiveLog
qplot(cut(EngPrmLang,6), MonthGiveLog,data=train)+
        stat_summary(fun.data="mean_cl_boot",colour="red")
#Average Income doesnt seem to affect anything

#create generalised linear model
LinearCCS<-glm(MonthGive~Age20t29+Age70pls+AveDonAmt+AveIncEA+DonPerYear+EngPrmLang+FinUnivP+LastDonAmt+Region+YearsGive,
    family=binomial(link="logit"),data=train)
summary(LinearCCS)
#McFadden R2
1-(LinearCCS$deviance/LinearCCS$null.deviance)

#ANOVA test to have a look at Region categories
#aov(LinearCCS)
#cant get it to work with a binomial output.........................

#logarithmic transformation of AveDonAmt, LastDonAmt, DonPerYear, YearsGive
summary(train)
#do DonPerYear or YearsGive have zeros? yes..for these take log(X+1)
train$Log.AveDonAmt<-log(train$AveDonAmt)
train$Log.LastDonAmt<-log(train$LastDonAmt)
train$Log.DonPerYear<-log(train$DonPerYear + 1)
train$Log.YearsGive<-log(train$YearsGive + 1)
LogCCS<-glm(MonthGive~Age20t29+Age70pls+Log.AveDonAmt+AveIncEA+Log.DonPerYear+EngPrmLang+FinUnivP+Log.LastDonAmt+
                    Region+Log.YearsGive,
               family=binomial(link="logit"),data=train)
summary(LogCCS)
1-(LogCCS$deviance/LogCCS$null.deviance)
#compare p values from linear and log model of CCS
data.frame("LinearCCS.pval"=round(summary(LinearCCS)$coefficients[,4],4),"LogCCS.pval"=round(summary(LogCCS)$coefficients[,4],4))
#delete Age70pls, AveIncEA,FinUnivP,
#keep log of none of them - none of the log variables improved the coefficients.
#redo glm and delete above variables. transform regions into two regions: R2R3="VanFraser" vs R1R4R5R6 "other"
train$New.Region<-train$Region
train$New.Region<-as.character(train$New.Region)
train$New.Region[train$Region %in% c("R2","R3")]<-"VanFraser"
train$New.Region[train$Region %in% c("R1","R4","R5","R6")]<-"Other"
head(train$New.Region,20)

#new glm model - MixedCCS
MixedCCS<-glm(MonthGive~AveDonAmt+DonPerYear+EngPrmLang+Log.LastDonAmt+New.Region+YearsGive,
               family=binomial(link="logit"),data=train)
summary(MixedCCS)
1-(LogCCS$deviance/LogCCS$null.deviance)

#use prediction models to predict accuracy of each model.. 
lift.chart(modelList=c("LinearCCS","LogCCS","MixedCCS"),data=train,targLevel="Yes",trueResp=0.01,type="cumulative",
           sub="testing")

#add columns to test data: New.Region,Log.AveDonAmt, Log.DonPerYear, Log.LastDonAmt, Log.YearsGive
test$New.Region<-test$Region
test$New.Region<-as.character(test$New.Region)
test$New.Region[test$Region %in% c("R2","R3")]<-"VanFraser"
test$New.Region[test$Region %in% c("R1","R4","R5","R6")]<-"Other"
head(test$New.Region,20)
test$Log.AveDonAmt<-log(test$AveDonAmt)
test$Log.LastDonAmt<-log(test$LastDonAmt)
test$Log.DonPerYear<-log(test$DonPerYear + 1)
test$Log.YearsGive<-log(test$YearsGive + 1)

lift.chart(modelList=c("LinearCCS","LogCCS","MixedCCS"),data=test,targLevel="Yes",trueResp=0.01,type="cumulative",
           sub="testdata")
lift.chart(modelList=c("LinearCCS","LogCCS","MixedCCS"),data=test,targLevel="Yes",trueResp=0.01,type="incremental",
           sub="testdata")
