
# library('ggplot2')
# library("dplyr")
# library("plyr")
# library('magrittr')
# library('reshape2')
# library('shiny')



shinyUI(pageWithSidebar(
        #theme="amelia.css",
        
        #Application title
        headerPanel("Diamonds"),
        
        # Sidebar with controls to select the variable to plot against positions
        sidebarPanel(
                
                selectInput("variable", "Fill Variable:",
                            list('Cut' = 'cut',
                                 "Color"= 'color'
                                 )),

                
                selectInput("y", "Y Axis:",
                            list("Depth" = "depth", 
                                 "Price" = "price")),
                
                submitButton(text="Update")
                
        ),
        
        #main panel. show plot of each position and percentage within selected variable
        mainPanel(
                h3(textOutput("caption")),
                
                graphOutput("pos.plot")
                
        )
        
                          
))