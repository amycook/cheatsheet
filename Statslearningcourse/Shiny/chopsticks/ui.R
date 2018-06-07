library('shiny')
library('ggplot2')

chops <- read.csv('data/chopstick-effectiveness.csv')
chops$Chopstick.Length <- as.factor(chops$Chopstick.Length)

#Define UI
shinyUI(fluidPage(
        
        #Application title
        titlePanel("Food Pinching Efficiency of Chopsticks"),
        
        sidebarLayout(
                
                # side bar panel with
                # two checkbox menu and radio buttons menu
                sidebarPanel(
                       checkboxGroupInput(inputId='lengths',
                                     label="Chopstick Length (mm):",
                                     choices = levels(chops$Chopstick.Length),
                                     selected = levels(chops$Chopstick.Length)),
                       
                       radioButtons(inputId = "facet",
                                    label = 'Facetted Plot:',
                                    choices = c('True', 'False'),
                                    selected = 'False'),
                       
                       submitButton("Go!")
                ),
                
                # Show a plot of the generated distribution in main Panel
                mainPanel(
                        
                        plotOutput("plot.dens"))
        )
                          
))



