# GEN DATA -----------------------------------------------

A = c(3, 4, 3, 5)
B = c(2, 2, 1, 4)
Z = c(1, 2, 1, 2)
R = c(-2, -1, -3, 0)
S = c(7,7,7,9)
mydata = data.frame(cbind(A,B,Z,R,S))
dates = c("2014-01-01","2014-02-01","2014-03-01","2014-04-01")
mydata$date = as.Date(dates)
mydata.m = melt(mydata,id="date")
names(mydata.m) = c("variable", "category","value")







# SERVER -----------------------------------------------
shinyServer(function (input, output) {
        
        
        # DATA
        sub10<- sub10
        
        output$caption<- renderText({as.character(input$daterange[2])})
        
        data.r = reactive({
                a = subset(sub10, Start.Date >= input$daterange[1] & Start.Date <= input$daterange[2])
                return(a)
        })
        
        
        
        # GGPLOT
        
        
        
        output$myplot = renderPlot({
                
                dd<-data.r()
                # ggplot with proper reference to reactive function <<data.r()>>
                s = ggplot(dd, aes(x=return.cut, y=percent.hrs, colour=variable))  +  
                        
                        # bars for categories A, B, Z: needs subsetting the data... but how?
                        stat_summary(fun.y= mean, geom='point')+ 
                        stat_summary(fun.y= mean, geom='line') +
                        theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
                              text=element_text(size=12)) +
                        scale_x_continuous(breaks = seq(1, 10, by = 1), limits=c(1,10))+ 
                        scale_y_continuous(breaks = seq(0, 100, by = 10), limits=c(0,100))
                
                print(s)
        })
})