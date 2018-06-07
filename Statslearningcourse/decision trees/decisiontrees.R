##decision tree
head(hilti.t10)
gum <- rpart(IPCLine ~ 
                     NumberEmployees + 
                     EngagementStatus+
                     PotentialClass+
                     SalesChannel+
                     FleetUser+
                     HOLUser+
                     VisitFrequency+
                     Territory+
                     NumberPurchases+
                     TotalSales,
             data=hilti.t10,
             cp=.001)
#widens space
par(mfrow=c(1,1),xpd=NA,cex=.75)

plot(gum)
text(gum)
summary(gum)
#expected loss consistently around 90% not sure that's useful. At cp=0.0001, some nodes have 
#an expected loss down at 60%


#predict IPCClass
eucal <- rpart(IPCClass ~ 
                     NumberEmployees + 
                     EngagementStatus+
                     PotentialClass+
                     SalesChannel+
                     FleetUser+
                     HOLUser+
                     VisitFrequency+
                     Territory+
                     NumberPurchases+
                     TotalSales,
             data=hilti.t10,
             cp=.001)
#widens space
par(mfrow=c(1,1),xpd=NA,cex=.75)

plot(eucal)
text(eucal)
summary(eucal)
#expected loss consistently around 90% not sure that's useful

##decision tree, try hilti.t20
head(hilti)
gum <- rpart(IPCLine ~ 
                     NumberEmployees + 
                     EngagementStatus+
                     PotentialClass+
                     FleetUser+
                     HOLUser+
                     VisitFrequency+
                     Territory+
                     NumberPurchases+
                     TotalSales,
             data=hilti,
             cp=.0001)
#widens space
par(mfrow=c(1,1),xpd=NA,cex=.75)

plot(gum)
text(gum)
summary(gum)
#expected loss consistently around 90% not sure that's useful. At cp=0.0001, some nodes have 
#an expected loss down at 60%

### work with full data set
head(hilti)
#training set
set.seed(1011)
train<-hilti[sample(1:nrow(hilti),700000),]
head(train)
gumtree<-rpart(IPCLine ~ 
                       NumberEmployees + 
                       EngagementStatus+
                       PotentialClass+
                       FleetUser+
                       HOLUser+
                       VisitFrequency+
                       Territory+
                       NumberPurchases+
                       TotalSales,
               data=train,
               cp=.0001)
plot(gumtree)
text(gumtree)
summary(gumtree)
#predict with other half of full training set
test<-hilti[-train,];head(test)
tree.pred<-predict(gumtree,hilti.t20,type="class")
results<-with(hilti.t20,table(tree.pred,IPCLine))
cv.gumtree<-printcp(gumtree)
plotcp(gumtree)
prunedgum<-prune(gumtree, cp=   gumtree$cptable[which.min(gumtree$cptable[,"xerror"]),"CP"])
plot(prunedgum)
par(mfrow=c(1,1),xpd=NA,cex=.75)
text(prunedgum)


###random forest
gumforest<-randomForest(IPCClassDistinct~
                                NumberEmployees + 
                                EngagementStatus+
                                PotentialClass+
                                FleetUser+
                                HOLUser+
                                VisitFrequency+
                                NumberPurchases+
                                TotalSales,
                        data=hilti.t20,ntree=400,mtry=4)

gumforest

## pretty plot

prp(fit, box.palette = "RdYlGn",
    varlen = 0, digits = 3, suffix = " kt")


