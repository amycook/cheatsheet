#library('UsingR',lib='C:/Progra~1/R/R-3.1.2/library')
library('shiny')
library('ggplot2')

shinyServer(
        function(input,output){
                output$text1<-renderText({input$text1})
                output$text2<-renderText({input$text2})
                output$text3<-renderText({
                        if (input$goButton==0)"you havent done anything yet ya tard!"
                        else if(input$goButton==1)"finally.. ya tard!"
                        else "what are you A.D.D.? trollol"
                })
        }
)
