library("car", #repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"), 
        lib='C:/Progra~1/R/R-3.2.0/library')
library('RcmdrPlugin.BCA',lib='C:/Progra~1/R/R-3.2.0/library')
library("rpart",lib = 'C:/Program Files/R/R-3.2.0/library')
library('rpart.plot',lib='C:/Progra~1/R/R-3.2.0/library')
library('magrittr',lib='C:/Progra~1/R/R-3.2.0/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.2.0/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.0/library')

data(CCS)
head(CCS)

#create sample set

CCS$Sample<- create.samples(CCS, est=0.4, val= 0.4)
CCSEst<- CCS[CCS$Sample == 'Estimation',] %>% select(-Sample)
CCSVal<- CCS[CCS$Sample == 'Validation',] %>% select(-Sample)

#create first rpart model

tree1CCS <- rpart(MonthGive ~ .,
                  data=CCSEst,
                  method = 'class',
                  control = rpart.control(cp= 0.01))
plot(tree1CCS)
par(mfrow = c(1,1), xpd = NA, cex=1.5, mar = rep(2, 4))
text(tree1CCS, use.n = TRUE)

# need to make branch length inidicate importance.
# also want leaf to state Yes or No and fraction out of total that are correct

prp(tree1CCS,
    #type indiciates how to print labels and leaves
    type = 0, 
    # how many characters to print in variable - 0= all
    varlen = 0, 
    # how many characters to print in factor level names - 0= all
    faclen = 0,
    #tweak = multiply calculated text size by this amount. 1 = same
    #tweak = 1.2,
    # gap = minimum horizontal gap btwn boxes in charac widths
    gap = 0,
    #horizontal space to the box border
    space =0,
    # display extra info at the nodes. 2 = correct classification rate at each node
    extra = 2,
    #display the leaves at the bottom of the graph
    fallen.leaves = TRUE)
    
    
#use the tree model to predict whether or not the following new donors are likely to join the
#monthly giving program

# donor 1: AveDonAmt = $19, AveIncEA = $85000 
# donor 2: AveDonAmt = $9, DonPerYear = 0.04 
# donor 3: AveDonAmt = $22, AveIncEA = $52,000, DonPerYear = 0.04, SomeUniv = 0.25

#answers - follow tree

#donor 1 - No
#donor 2 - Yes
#donor 3 - Yes


# redraw tree with equal length branches.

prp(tree1CCS,
    #type indiciates how to print labels and leaves
    type = 0, 
    # how many characters to print in variable - 0= all
    varlen = 0, 
    # how many characters to print in factor level names - 0= all
    faclen = 0,
    #tweak = multiply calculated text size by this amount. 1 = same
    tweak = 1.2,
    # gap = minimum horizontal gap btwn boxes in charac widths
    gap = 0,
    #horizontal space to the box border
    space =0,
    # display extra info at the nodes. 2 = correct classification rate at each node
    extra = 2,
    #display the leaves at the bottom of the graph
    fallen.leaves = FALSE)
    

# correlated variables. From regression, we know that AveDonAmt and LastDonAmt are correlated
# try deleting AveDonAmt and replot. LastDonAmt now shines through
#this does not affect the predicitive power of the tree

cor.test<- CCSEst %>% select(-AveDonAmt)
tree2CCS <- rpart(MonthGive ~ .,
                  data=cor.test,
                  control = rpart.control(cp= 0.01))
prp(tree2CCS,
    type = 0, 
    varlen = 0, 
    faclen = 0,
    tweak = 1.2,
    gap = 0,
    space =0,
    extra = 2,
    fallen.leaves = FALSE)

#lets look at cross validation
# delete AveDonAmt and LastDonAmt because the tree is too simple with them.

noAorLDA <- CCSEst %>% select(-AveDonAmt, -LastDonAmt, -Sample)
tree3CCS <- rpart(MonthGive ~ .,
                  data=noAorLDA,
                  control = rpart.control(cp= 0.001))


print(tree3CCS)
prp(tree2CCS,
    type = 0, 
    varlen = 0, 
    faclen = 0,
    tweak = 1.2,
    gap = 0,
    space =0,
    extra = 2,
    Margin = 0,
    fallen.leaves = FALSE)

#print pruning table and associated plot
printcp(tree3CCS)
# CP = complexity parameter. Stops when row = CP
# rel error = decrease in error from first root node - which is set at 1
#               - error continually decreases and at some point it will be overfit. 
# xerror - at each split, rpart is programmed to split the input data into its own estimation and 
# validation set, re-estimates the model and predicts using the validation set. The error between
# known and predicted is averaged over several iterations. called the cross validation error. xerror
# this typically decreases and starts increasing again as the model becomes overfit.
# In our case, xerror is minimum at 11 splits.

# plot the xerror

plotcp(tree3CCS)

# from plot and dashed line indicating std error, we can see that the split at 2 splits is as accurate
# as possible to statistical significance
# dashed line is xstd above lowest xerror. ie 0.70064 + 0.038266 = 0.7388
# xstd exists because the xerror is an estimate averaged over several iterations. there is uncertainty as to the 
# true xerror value! sample from population etc.

# re-run model with cp value to generate 2 split tree - cp = between .04 and .0668 choose 0.05

tree4CCS <- rpart(MonthGive ~ .,
                  data=noAorLDA,
                  control = rpart.control(cp= 0.05))
prp(tree4CCS,
    type = 0, 
    varlen = 0, 
    faclen = 0,
    tweak = 1,
    gap = 0,
    space =0,
    extra = 2,
    Margin = 0,
    fallen.leaves = FALSE)

#create two more models with 3 splits and 4.

tree5CCS <- rpart(MonthGive ~ .,
                  data=noAorLDA,
                  control = rpart.control(cp= 0.03))
prp(tree5CCS,
    type = 0, 
    varlen = 0, 
    faclen = 0,
    tweak = 1,
    gap = 0,
    space =0,
    extra = 2,
    Margin = 0,
    fallen.leaves = FALSE)

tree6CCS <- rpart(MonthGive ~ .,
                  data=noAorLDA,
                  control = rpart.control(cp= 0.0175))
prp(tree6CCS,
    type = 0, 
    varlen = 0, 
    faclen = 0,
    tweak = 1,
    gap = 0,
    space =0,
    extra = 2,
    Margin = 0,
    fallen.leaves = FALSE)

#now have three trees to compare tree3CCS, tree4CCS, tree5CCS, tree6CCS
# graph lift charts using estimation sample set, ie CCSEst

#quick generation of the four models

a<- function (y = "MonthGive", data = noAorLDA, c.p = c(.001, 0.05, 0.03, 0.0175), tag='CCS') {
        vec = rep(NA, length(c.p))
        formula = paste( y,"~ .", sep = ' ')
        for (i in 1:length(c.p)) {
                assign(paste(tag, c.p[i], sep=""),
                       rpart(as.formula(formula), data= data,
                             method = 'class',
                             control = rpart.control(cp= c.p[i])),
                       envir=.GlobalEnv)
                
                #create an output vector with names of all rpart models
                vec[i] = paste(tag, c.p[i], sep="")
                cat(i," ")
        }
        vec
        
}

trees<- a(y = "MonthGive", data = noAorLDA, c.p = c(.001, 0.05, 0.03, 0.0175))

### CREATE ROC CHART ####

#make dataframe of models and colours
leg<- data.frame('models' = trees, 'colours' = c('blue','red','green','black'))
leg$colours <- as.character(leg$colours)


ROC.add<- function(model= trees2[1], val.df = CCSVal, y.var = "MonthGive", ad.d=FALSE, colour = leg[1,2]) {
        p.1= data.frame('prediction' = predict(model %>% get, val.df, type='class'))
        #calculate the overall accuracy
        p.1$answer= val.df[,y.var]
        p.1$correct= p.1[,'prediction'] == p.1[,'answer']
        
        #extract the class 'Yes' probabilities for each answer
        p.1$probs= predict(model %>% get, CCSVal, type = 'prob')[,2] 
        
        #prepare vectors for ROCR
        pred<- prediction(p.1$probs, p.1$answer)
        perf<- performance(pred, 'tpr', 'fpr')
        plot(perf, main = 'ROC Curve', add=ad.d, col= colour)
        
        #AUC
        temp = performance(pred, "auc")
        auc.print = as.numeric(temp@y.values) %>% round(3)
        mean.print = mean(p.1$correct) %>% round(3)
        
        paste(model, " ... ", 
              "mean=", mean.print, 
              "AUC=", auc.print, sep= " ") %>% print
        
}

ROC.add(model= trees[1], val.df = CCSVal, y.var = "MonthGive", ad.d=FALSE, colour = leg[1,2])
ROC.add(model= trees[2], val.df = CCSVal, y.var = "MonthGive", ad.d=TRUE, colour = leg[2,2])
ROC.add(model= trees[3], val.df = CCSVal, y.var = "MonthGive", ad.d=TRUE, colour = leg[3,2])
ROC.add(model= trees[4], val.df = CCSVal, y.var = "MonthGive", ad.d=TRUE, colour = leg[4,2])
abline(a=0, b=1, col = "gray60")

#insert legend
legend(0.7,0.7, leg$models, col=leg$colours, cex = .6, lwd=1)


#now repeat using all response variables!! 

Full001<- rpart(MonthGive ~ .,
                         data=CCSEst,
                         control = rpart.control(cp= 0.001))
plotcp(Full001)
printcp(Full001)

#therefore try, cp = 0.02, 0.016, 0.001

trees2<- a(y = "MonthGive", data = CCSEst, c.p = c(.001, 0.02, 0.016), tag='FULL')
#make dataframe of models and colours
leg2<- data.frame('models' = c(trees2,trees[1]), 'colours' = c('blue','red','green','black'))
leg2$colours <- as.character(leg$colours)

ROC.add(model= trees2[1], val.df = CCSVal, y.var = "MonthGive", ad.d=FALSE, colour = leg2[1,2])
ROC.add(model= trees2[2], val.df = CCSVal, y.var = "MonthGive", ad.d=TRUE, colour = leg2[2,2])
ROC.add(model= trees2[3], val.df = CCSVal, y.var = "MonthGive", ad.d=TRUE, colour = leg2[3,2])
ROC.add(model= trees[1], val.df = CCSVal, y.var = "MonthGive", ad.d=TRUE, colour = leg2[4,2])
abline(a=0, b=1, col = "gray60")

#insert legend

legend(0.7,0.7, leg2$models, col=leg2$colours, cex = .6, lwd=1)

#YAY!






    