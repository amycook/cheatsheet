#library('shiny',lib='C:/Progra~1/R/R-3.1.2/library')
#setwd("C:/Users/n9232371/Google Drive/shared files/Statslearningcourse/Shiny")
library('shiny')
library('ggplot2')

shinyUI(fluidPage(
        theme="amelia.css",
        titlePanel("Hello Shiny!"),
        fluidRow(
                wellPanel(
                column(4,
                       textInput(inputId='text1',label="Input Text1"),
                       textInput(inputId='text2',label="Input Text2"),
                       actionButton(inputId="goButton",label="Go!")
                )),
                
                wellPanel(
                column(8,
                       p('Output text1'),
                       textOutput('text1'),
                       p('Output text2'),
                       textOutput('text2'),
                       p('Output text3'),
                       textOutput('text3')
                ))
        )
                          
))