


shinyServer(function(input, output) {
        
        # Compute the forumla text in a reactive function since it is 
        # shared by the output$caption and output$mpgPlot functions
        formulaText1<- reactive({
                paste("Monthly Summary for", input$disc, sep=" ")
        })
        
        # Return the formula text for printing as a caption
        output$caption <- renderText( {
                formulaText1()
        })
        
        formulaText <- reactive({
                paste(input$y, "~", "return.cut")
        })
        
        #check for the input variable
        data.r= reactive ({
                if(input$disc == 'All') {
                        posData<- month.a}
                
                else{
                        posData<- month.a %>% filter(Discipline == input$disc)
                }
                return(posData)
                
        })
        
        #subset data based on slider inputs for Date.
        
        data.r2 = reactive({
                a = subset(data.r(), Date >= input$date[1] & Date <= input$date[2])
                return(a)
        })

        
        output$myChart <- renderChart({

                p1 <- nPlot(as.formula(formulaText()), group = input$variable, data = data.r2(),
                            type = 'multiBarChart')
                
                p1$addParams(dom = 'myChart')
                p1$xAxis( rotateLabels = -45, axisLabel = "Return per Dollar (Balance/Cost)",
                          categories= levels(data.r2()$return.cut))
                p1$chart( xDomain = levels(month.a$return.cut),
                          reduceXTicks = FALSE)
                p1$params$width <- 800
                p1$params$height <- 350
#                 p1$plotOptions(column=list(stacking = "normal"))
                

                return(p1)
                
        })
})