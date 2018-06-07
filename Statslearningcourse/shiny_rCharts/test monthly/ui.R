
# library('rHighcharts',lib='C:/Progra~1/R/R-3.2.0/library')

shinyUI(fluidPage(
        titlePanel("Monthly Review"),
        
        fluidRow(
                column(3,
                selectInput("disc", "Discipline:",
                            list("Civil" = "Civil", 
                                 "Structural" = "Structural", 
                                 "Environmental Planning" = "Environmental Planning",
                                 "Water" = "Water",
                                 "All"= "All")),
                
                selectInput("variable", "Fill Variable:",
                            list('Client Sector' = 'Biz.type',
                                 "Job Number Code"= 'code.jobnum',
                                 "Total Timespan" = "timespan.cut", 
                                 "Client Code" = "code.client",
                                 "Director Code" = "code.director",
                                 "Project Engineer Code"= "code.ProjEng",
                                 "Billing Type"= "Billing.Type",
                                 "Discipline"= "Discipline",
                                 "Primary Job Type"= 'JD.Primary',
                                 "Client Count"= 'clientcount.cut',
                                 "Business"= 'Business',
                                 "Client no. Employees" ='clientemps.cut',
                                 'Client Size' = 'Biz.size',
                                 'Secondary Job Type'='JD.Second',
                                 'Job Type'='Type')),
                
                selectInput("y", "Y Axis:",
                            list("Invoiced Amount ($)" = "Invoiced.Amount", 
                                 "Balance ($)" = "bal.mon")),
                
                dateRangeInput('date', 'Date', start = '2008-12-31', end = '2009-01-31', min = '2008-12-31', max = '2014-12-15', 
                               format = "dd-mm-yyyy", startview = "month"),
                
                submitButton(text="Update")
                ),
                
                column(9,
                       
                h3(textOutput("caption")),
                
                showOutput("myChart", "nvd3")
#                 HTML('<style>.rChart {width: 100%; height: 600px}</style>')
                
        )
)))



