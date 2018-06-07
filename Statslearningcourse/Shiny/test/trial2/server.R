#library('UsingR',lib='C:/Progra~1/R/R-3.1.2/library')
library('shiny')
library('ggplot2')
library('magrittr')

diamonds= read.csv('manmade.csv')


shinyServer(
        function(input,output){
                
                # Reactive expression to generate the requested distribution.
                # This is called whenever the inputs change. The output
                # functions defined below then all use the value computed from
                # this expression

                a <- reactive({
                        a = diamonds[diamonds$price<= input$price,]
                        return(a)
                })
                
                output$plot.di<- renderPlot({
                        
                        p<- ggplot(a(), aes_string(x= input$xaxis, y="price")) +
                                geom_point(aes_string(colour= 'cut'))
                                
                        print(p)
                                
                        })
                
        }
)



