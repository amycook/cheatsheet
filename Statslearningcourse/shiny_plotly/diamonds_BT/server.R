
library('ggplot2')
library("dplyr")
library("plyr")
library('magrittr')
library('reshape2')
library('shiny')


#shiny function
levels(diamonds$clarity) <- c("SI1", "SI2", "VS1","VS2","VVS1","VVS2","I1","IF")

shinyServer(
        function(input,output, session){
                
                # Compute the forumla text in a reactive function since it is 
                # shared by the output$caption and output$mpgPlot functions
                formulaText<- reactive({
                        paste("diamonds filled with", input$variable, sep=" ")
                })
                
                # Return the formula text for printing as a caption
                output$caption <- renderText( {
                        formulaText()
                })
                


                
                #generate a plot of requested variable
                
                
                output$pos.plot<- renderGraph( {

                                                      
                        s<- ggplot(diamonds[1:3000,], aes_string(x='clarity', y= input$y, fill= input$variable)) +
                                geom_bar(stat='identity')+
                                theme(legend.position="right", axis.text.x=element_text(angle=45,hjust=1),
                                      text=element_text(size=12)) +
                                labs(x = 'Return per Dollar') 
                        
                        
                        # convert from ggplot->plotly
                        gg <- gg2list(s)
                        
                        gg$layout<- list(hovermode='closest')
                        gg$layout$xaxis <- list(tickangle = -45, autorange= FALSE)
                        gg$layout$barmode <- 'stack'
                        




                        
                        
                        # Send this message up to the browser client, which will get fed through to
                        # Plotly's javascript graphing library embedded inside the graph
                        return(list(
                          list(
                            id = "pos.plot",
                            task = "newPlot",
                            data = gg$data,
                            layout= gg$layout
                          )
                        ))







                })
                
        }
)

