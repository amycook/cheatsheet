library('devtools',lib='C:/Progra~1/R/R-3.2.2/library')
with_libpaths(new = 'C:/Progra~1/R/R-3.2.2/library', install_github("rstudio/shinyapps"))
library('shinyapps', lib='C:/Progra~1/R/R-3.2.2/library')

shinyapps::setAccountInfo(name='amycookie', token='73463E0A91B0FC563B12F2E50F6A8697',
                          secret='2//T3Z//rfH7Mu9BKykSw2lLFOh7130qpt16SU8u')



#set working directory
setwd("C:/Users/n9232371/OneDrive/shared files/Statslearningcourse/Shiny/test/trial2")
runApp()
deployApp()
