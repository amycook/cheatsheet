
# Bayesian non-parametrics

# Dirchilet processes

# Chinese Restaurant model
# restaurant with infinite number of tables (clusters)
# each table has an infinite number of seats
# as each customer arrives, they select a table with diff probabilities.

# second customer sits at first customer table with probability alpha/(1 + alpha)
# second cstomer sits at new table with probability 1/(1+ alpha)
# nth customer sits at existing table with probability nk / (n + alpha) - not sure what nk is

crp = function(num.customers = 10, alpha = .5){
        table <- c(1)
        next.table <- 2
        
        for(i in 1:num.customers){
                
                #check if sits at new table
                if(runif(1,0,1) < (alpha/(i + alpha))){
                        table <- c(table, next.table)
                        next.table <- next.table + 1
                } else {
                        select.table <- table[sample(1:length(table %>% unique), 1)]
                        table <- c(table, select.table)
                }
        }
        
        table
}

clusts <- crp(100, alpha = 0.8)

table(clusts)


# stick breaking process
# generate proportions in each group directly ..

# start with stick of length 1
# generate a random variable, beta (beta distribution ~(1, alpha)). 
#    this will be a number between 0 and 1 with expected value 1/(1 +alpha)
# break the stick at beta. w1 is length of stick on left. 
# take right hand side. break at newly sampled beta. length of stick on left is w2.
# this is another way of doing chinese restaurant, except directly calculating proportions
# assign customers to table 1 with probability w1.

stick_bp <- function(num_weights, alpha){
        
        betas <- rbeta(num_weights, 1, alpha)
        remaining_stick <- c(1, cumprod(1-betas))[1:num_weights] # as proportion of piece length
        weights = remaining_stick*betas
        weights
        
}




# R package example PReMiuM
library(PReMiuM)
inputs <- generateSampleDataFile(clusSummaryBernoulliDiscrete())
preds <- data.frame(matrix(c(2, 2, 2, 2, 2, 0, 0, NA, 0, 0), ncol = 5,
                              byrow = TRUE))
colnames(preds) <- names(inputs$inputData)[2:(inputs$nCovariates+1)]
runInfoObj <- profRegr(yModel = inputs$yModel, xModel = inputs$xModel,
                          nSweeps = 1000, nBurn = 1000, data = inputs$inputData,
                          output = "output", covNames = inputs$covNames, predict = preds,
                          fixedEffectsNames = inputs$fixedEffectNames)
dissimObj <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(dissimObj)


runInfoObj <- profRegr(xModel = inputs$xModel,excludeY= T,
                          nSweeps = 1000, nBurn = 1000, data = inputs$inputData[,2:6],
                          output = "output", covNames = inputs$covNames)
dissimObj <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(dissimObj)

riskProfileObj <- calcAvgRiskAndProfile(clusObj)
predictions <- calcPredictions(riskProfileObj,
                                  fullSweepPredictions = TRUE, fullSweepLogOR = TRUE)
plotPredictions(outfile = "predictiveDensity.pdf",
                   runInfoObj = runInfoObj, predictions = predictions, logOR = TRUE)


# example for Poisson outcome and Discrete covariates
inputs <- generateSampleDataFile(clusSummaryPoissonDiscrete())
runInfoObj<-profRegr(yModel=inputs$yModel,
                     xModel=inputs$xModel, nSweeps=10, nClusInit=20,
                     nBurn=20, data=inputs$inputData, output="output",
                     covNames = inputs$covNames, outcomeT = inputs$outcomeT,
                     fixedEffectsNames = inputs$fixedEffectNames)
dissimObj<-calcDissimilarityMatrix(runInfoObj)
clusObj<-calcOptimalClustering(dissimObj)
riskProfileObj<-calcAvgRiskAndProfile(clusObj)
clusterOrderObj<-plotRiskProfile(riskProfileObj,"summary.png")

# try mcdonalds example
maccas <- read.table("/Users/yam/OneDrive/shared files/Statslearningcourse/dirichlet_clustering/maccas.txt",
                     header = T)
# example for Poisson outcome and Discrete covariates
setwd("/Users/yam/OneDrive/shared files/Statslearningcourse/dirichlet_clustering/")
runInfoObj<-profRegr(excludeY= T,
                     xModel= "Normal", nSweeps=1000,nClusInit=20,
                     nBurn=1000, data=maccas[,1:14], output="output",
                     covNames = names(maccas)[1:14])

dissimObj<-calcDissimilarityMatrix(runInfoObj)
clusObj<-calcOptimalClustering(dissimObj)
# riskProfileObj<-calcAvgRiskAndProfile(clusObj)
# clusterOrderObj<-plotRiskProfile(riskProfileObj,"summary.png")
# add clusters to dataframe
maccas$cluster <- clusObj$clustering
maccas %>% dplyr::select(cluster, name) %>% arrange(cluster)

# plot on ggbiplot
p<- prcomp(~.-name - cluster, data=maccas, centre=T)
q<- ggbiplot(p, obs.scale = 1, var.scale = 1,
             groups = maccas$cluster %>% as.factor, ellipse = F,
             labels = maccas$name, labels.size = 2,
             circle = TRUE) %>% 
    ggplotly()


# compare to kmeans
compare <- kmeans(maccas[,1:14], 7)
maccas2 <- maccas %>% mutate(
    cluster = compare$cluster
)
maccas2 %>% dplyr::select(cluster, name) %>% arrange(cluster)
ggbiplot(p, obs.scale = 1, var.scale = 1,
         groups = maccas2$cluster %>% as.factor, ellipse = F,
         labels = maccas2$name, labels.size = 1,
         circle = TRUE) %>% 
    ggplotly()


# grepping to test maccas
comparer <- function(string = "withoutchicken"){
    print('DP')
    print(
    maccas %>% filter(grepl(string, name, ignore.case = T)) %>% dplyr::select(name, cluster)
    )
    print('kmeans')
    print(
    maccas2 %>% filter(grepl(string, name,ignore.case = T)) %>% dplyr::select(name, cluster)
    )
}
comparer('withoutchicken')
comparer("SugarFree")
comparer("Nonfat")
comparer("Sauce")
comparer("cheese")


