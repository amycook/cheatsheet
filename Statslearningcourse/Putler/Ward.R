library("car", #repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"), 
        lib='C:/Progra~1/R/R-3.2.2/library')
library('RcmdrPlugin.BCA',lib='C:/Progra~1/R/R-3.2.2/library')
library('car',lib='C:/Progra~1/R/R-3.2.2/library')
library('RcmdrPlugin.BCA')

library('magrittr',lib='C:/Progra~1/R/R-3.2.0/library')
library("plyr",lib = 'C:/Progra~1/R/R-3.2.2/library')
library("dplyr",lib = 'C:/Progra~1/R/R-3.2.2/library')

#Athletic data is the relative importance that stakeholder groups at a university place on seven different measures of 
# athletic program performance

data(Athletic)
head(Athletic)
summary(Athletic)
nrow(Athletic)

#Ward's clustering

x<- dist(Athletic, method = 'euclidean')
fit<- hclust(x, method = 'ward.D2')
plot(fit)
rect.hclust(fit, k = 4, border = "red")

#assign a cluster number between 1 and 4 to all rows in data frame
Athletic$groups<- as.factor(cutree(fit, k=4))
head(Athletic)

#summarise how many items in each cluster
summary(as.factor(Athletic$groups))

#cluster centroids - the averages of each column for each cluster
# use ddply

centroid <- ddply(Athletic, .(groups), numcolwise(mean))

#print centroid. The highest mean value is what that cluster of stakeholders values most
# ie group 1 values Winning! 0.26689
# group 2 values Graduating - 0.295
# group 3 values Violating ? - .3735
# group 4 values Finan ? 0.38121

#visualise means of Grad with boxplots
#standard errors
Grad.plot<- ddply(Athletic, .(groups), function(x) {
        CI(x$Grad) })

ggplot(Grad.plot, aes(x= groups, y = mean)) + 
        geom_point()+
        geom_errorbar(aes(ymax= Grad.plot$upper, ymin= Grad.plot$lower))

#the high level of caring about Grad is statistically significnatly higher in group 2 than the 
# other three groups

# principal component analysis
#convert cluster groups to integer

Athletic$groups<- as.integer(Athletic$groups)
p<- prcomp(~.-groups, data=Athletic, centre=T)
summary(p)
biplot(p, xlabs = as.character(Athletic$groups))
#putlers in-house biplot shows centroids
bpCent(p, clsAsgn = Athletic$groups, centroids=TRUE, xlabs = as.character(Athletic$groups))


#now look at 3D - triplot - using prcomp
#scores stored as 'x' variable
p$x
plot3d(p$x[,1:3], col=as.character(Athletic$groups))
text3d(p$x[,1:3], texts = as.character(Athletic$groups), color=as.character(Athletic$groups), adj=2)
text3d(p$rotation[,1:3], text = rownames(p$rotation), color= 'red')
coords <- NULL
for (i in 1:nrow(p$rotation)) {
        coords <- rbind(coords, rbind(c(0,0,0), p$rotation[i,1:3]))
}
lines3d(coords, col="red", lwd=4)



