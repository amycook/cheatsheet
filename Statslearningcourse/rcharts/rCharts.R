library('rCharts',lib='C:/Progra~1/R/R-3.1.2/library')
haireye<-as.data.frame(HairEyeColor)
head(haireye)
n1<-nPlot(Freq~Hair,group='Eye',type='multiBarChart',
      data=subset(haireye, Sex=='Male'))
n1
n1$render
n1$save('n1.html',cdn=FALSE)

require(rCharts)
n1 <- nPlot(mpg ~ wt, data = mtcars, type = 'scatterChart')
n1
print(n1)

#example 1
names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')

#example 2
hair_eye = as.data.frame(HairEyeColor)
rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')

#example 3
r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = "point", color = "gear")
r1$print("chart1")
r1
graph_chart1.addHandler(function(type, e){
        var data = e.evtData;
        if (type === 'click'){
                return alert("You clicked on car with mpg: " + data.mpg.in[0]);
        }
})


#example 4
data(economics, package = "ggplot2")
econ <- transform(economics, date = as.character(date))
m1 <- mPlot(x = "date", y = c("psavert", "uempmed"), type = "Line", data = econ)
m1$set(pointSize = 0, lineWidth = 1)
m1$print("chart2")
m1

#example 5
hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart")
n1$print("chart3")
n1
