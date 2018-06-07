# instructions: https://www.shinyapps.io/admin/#/dashboard

# install.packages('rsconnect')
library('rsconnect')
rsconnect::setAccountInfo(name='amycook', token='48C9B6CB5ABADEDD001330DFA1824948', 
                          secret='M2qUvTuHSpScpzLCUahXy5TxwkM7USUfHB01qv1q')

rsconnect::deployApp('/Users/yam/OneDrive/shared files/Statslearningcourse/Shiny/chopsticks',
                     account = 'amycook')

