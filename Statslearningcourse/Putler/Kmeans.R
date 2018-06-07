#K means clustering of Athletic, will compare to wards method

#assess choice of 4 clusters using 
# Rands Index (reproducability)
# and Calinski-Harabasz index (within cluster homogeneity and across cluster separation)

# calinski index first using cascadeKM package in vegan
library('fpc')
library('vegan')
library('reshape2')
library('ggplot2')
library('magrittr')
library("plyr")
library("dplyr")

Ath.km<- cascadeKM( Athletic, 2, 7, iter= 100)
Ath.km
plot(Ath.km, sortg = TRUE, grpmts.plot = TRUE)

#create plots of SSE index (distance within clusters) 
# Cluster cohesion: create plots of SSE index (distance within clusters - sum of squared error) 
# then calinksi (index of cluster homogeneity and distance where higher number is better)
# calinski: between cluster distance/within cluster distance
ind.plot<- melt(Ath.km$results)
ggplot(ind.plot %>% filter(Var1 == 'SSE'), aes(x=Var2, y=value, group=1)) + 
        geom_line( )
ggplot(ind.plot %>% filter(Var1 == 'calinski'), aes(x=Var2, y=value, group=1)) + 
        geom_line( )

#fpc package to find rand solution
#make two 4 cluster solutions
t1<- kmeans(Athletic, 4)
t2<- kmeans(Athletic, 4)
cluster.stats(dist(Athletic), t1$cluster, t2$cluster, compareonly= T)


# create biplot with 4 clusters using kmeans
Athletic$groupsk<- as.integer(t1$cluster)
p<- prcomp(~.-groups - groupsk, data=Athletic, centre=T)
summary(p)
biplot(p, xlabs = as.character(Athletic$groupsk))
biplot(p, xlabs = as.character(Athletic$groups))

#summary of kmeans clusters t1
ddply(Athletic, .(groupsk), numcolwise(mean))
ddply(Athletic, .(groups), numcolwise(mean))
table(Athletic$groups, Athletic$groupsk)










