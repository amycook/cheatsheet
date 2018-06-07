#library('shiny',lib='C:/Progra~1/R/R-3.1.2/library')
#setwd("C:/Users/n9232371/Google Drive/shared files/Statslearningcourse/Shiny")
library('shiny')
library('ggplot2')

#Define UI
shinyUI(fluidPage(
        
        #Application title
        titlePanel("Diamond manmade"),
        
        sidebarLayout(
                
                # side bar panel with
                # two drop down menu and slider bar
                sidebarPanel(
                       selectInput(inputId='xaxis',label="X Axis Variable:",
                                   c("Certified carat" = "carat",
                                     "Certified depth" = "depth")),
                       sliderInput("price",
                                   "Maximum price:",
                                   min=350,
                                   max= 19000,
                                   value = 15000),
                       submitButton("Go!")
                ),
                
                # Show a plot of the generated distribution in main Panel
                mainPanel(
                        
                        plotOutput("plot.di"))
        )
                          
))