M <- gvisMotionChart(Fruits, "Fruit", "Year", options = list(width = 600, height = 400))
plot(M)

setwd("C:/Users/n9232371/Google Drive/shared files/Statslearningcourse/plotly")
#two lines example
py<-plotly()
trace0<-list(
        x=c(1,2,3,4),
        y=c(10,15,13,17)
        )
trace1<-list(
        x=c(1,2,3,4),
        y=c(16,5,11,9)
        )
response<-py$plotly(trace0,trace1,kwargs=list(filename="basic-line",
                                              fileopt="overwrite"))
response$url

#diamonds example
dsamp<-diamonds[sample(nrow(diamonds),1000),]
qplot(carat,price,data=dsamp,colour=clarity)+coord_flip()
py<-plotly()
py$ggplotly()

#coursera example
load("courseraData.rda")
head(myData)
g<-ggplot(myData,aes(y=enrollment,x=class,fill=offering))
g<-g+geom_bar(stat='identity')
g

#lets get it into plot.ly
py<-plotly()
out<-py$ggplotly(g)
out$response$url

# plotly example
# want to try plotting 'big data' time series - like Tronox.

library(plotly)
packageVersion('plotly')

p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)




