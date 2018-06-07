# INPUT PART

shinyUI(pageWithSidebar(
        # Application title
        headerPanel("My App"),
        
        sidebarPanel( 
                
                dateInput("daterange", "Date range:",
                               start  = "2003-10-25",
                               end    = "2014-12-20",
                               min    = "2003-10-30",
                               max    = "2014-12-20",
                               format = "dd/mm/yyyy",
                               separator = "to"),
                
                submitButton(text="Update!")
        ),
        # -----------------------------------------------
        
        # OUTPUT PART
        
        mainPanel(
                tabsetPanel(
                        tabPanel("Tab 1", h4("Head 1"),plotOutput("myplot"))
                ),
                h3(textOutput("caption"))
        )
))