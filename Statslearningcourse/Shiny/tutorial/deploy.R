library('devtools')
install_github('rstudio/shinyapps')
library('shinyapps')

shinyapps::setAccountInfo(name='amycookie', token='73463E0A91B0FC563B12F2E50F6A8697',
                          secret='2//T3Z//rfH7Mu9BKykSw2lLFOh7130qpt16SU8u')

#set working directory
runApp()
deployApp()
