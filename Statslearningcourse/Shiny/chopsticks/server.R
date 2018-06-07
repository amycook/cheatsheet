library('shiny')
library('ggplot2')


chops <- read.csv('data/chopstick-effectiveness.csv')
chops$Chopstick.Length <- as.factor(chops$Chopstick.Length)

shinyServer(
        function(input,output){
                
                a <- reactive({
                        a = chops[chops$Chopstick.Length %in% input$lengths, ]
                        return(a)
                })
                
                output$plot.dens<- renderPlot({
                        
                        p<- ggplot(a(), aes(x = Food.Pinching.Effeciency, 
                                            y = ..density..,
                                            fill = Chopstick.Length, 
                                            colour = Chopstick.Length)) +
                                geom_density(alpha = 0.5)
                        
                        if(input$facet == 'True'){
                                p <- p + facet_wrap(~Chopstick.Length)
                        }
                                
                        print(p)
                                
                        })
                
        }
)



