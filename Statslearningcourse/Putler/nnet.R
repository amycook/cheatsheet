###neural networks
library('RcmdrPlugin.BCA',lib='C:/Progra~1/R/R-3.2.1/library')
data(CCS)
head(CCS)
library('magrittr',lib='C:/Progra~1/R/R-3.2.1/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.1/library')
library("rpart",lib = 'C:/Program Files/R/R-3.2.1/library')
library('rpart.plot',lib='C:/Progra~1/R/R-3.2.1/library')
library('neuralnet',lib='C:/Progra~1/R/R-3.2.1/library')
library('nnet',lib='C:/Progra~1/R/R-3.2.1/library')


#engineer variables
nrow(CCS)
CCS$Log.AveDonAmt<- log(CCS$AveDonAmt)
CCS$Log.LastDonAmt<- log(CCS$LastDonAmt)
CCS$New.Region<- as.character(CCS$Region)
CCS$New.Region[CCS$Region %in% c('R2','R3')]<- "VanFraser"
CCS$New.Region[CCS$Region %in% c("R1","R4","R5","R6")]<- "Other"
CCS$New.Region<- as.factor(CCS$New.Region)

#create training and validation 50/50
train.index<- sample(1600,800, replace=F)
train<- CCS[train.index,]
test<- CCS[-train.index,]

#first make glm and rpart for future comparisons

# glm

MixedCCS2<- glm(MonthGive ~ AveIncEA + DonPerYear + Log.AveDonAmt + Log.LastDonAmt + New.Region,
                family= binomial(link='logit'), data= train)

#rpart

Full011<- rpart(MonthGive ~ .-Log.AveDonAmt -Log.LastDonAmt,
                cp=0.011, data= train)
prp(Full011,
    type = 0, 
    varlen = 0, 
    faclen = 0,
    tweak = 1,
    gap = 0,
    space =0,
    extra = 2,
    Margin = 0,
    fallen.leaves = FALSE)

# neural net

#convert all variables to numeric using model.matrix function

m.CCS<- model.matrix(~MonthGive + AveIncEA +  DonPerYear + AveDonAmt + LastDonAmt + Region, data= CCS)
colnames(m.CCS)[colnames(m.CCS) %in% 'MonthGive[T.Yes]']<-'MonthGive'
colnames(m.CCS)[colnames(m.CCS) %in% 'Region[T.R2]']<-'R2'
colnames(m.CCS)[colnames(m.CCS) %in% 'Region[T.R3]']<-'R3'
colnames(m.CCS)[colnames(m.CCS) %in% 'Region[T.R4]']<-'R4'
colnames(m.CCS)[colnames(m.CCS) %in% 'Region[T.R5]']<-'R5'
colnames(m.CCS)[colnames(m.CCS) %in% 'Region[T.R6]']<-'R6'

m.train<- m.CCS[train.index,]
m.test<- m.CCS[-train.index,]


#hidden is the number of hidden neurons in each layer
NN.HL2<- neuralnet(MonthGive ~ AveIncEA + DonPerYear + AveDonAmt + LastDonAmt + R2 + R3 + 
                           R4 + R5 + R6,
                   data=m.train,
                   hidden=2
                   )

NN.HL2<- nnet(x=train[,colnames(train) %in% c('AveIncEA', 'DonPerYear', 'AveDonAmt', 'LastDonAmt','Region')],
              y=train[,'MonthGive'],
              decay=0.1,
              size=2)





