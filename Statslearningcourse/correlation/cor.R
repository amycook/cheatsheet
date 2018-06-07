### TESTING FOR CORRELATION ###
library('ggplot2', lib = 'C:/Progra~1/R/R-3.2.0/library')
library('corrgram', lib = 'C:/Progra~1/R/R-3.2.0/library')

data(faithful)
head(faithful)
cor(faithful$eruptions, faithful$waiting,
    method = c('pearson', 'kendall', 'spearman')) 
#pearson default method - linear assocation btwn two cont variables
# if both variables are normally distributed, pearsons tests for an association of any kind, not just linear

cor(faithful$eruptions, faithful$waiting,
    method = c('spearman'))
cor(faithful$eruptions, faithful$waiting,
    method = c('kendall'))

#are variables normally distributed????
ggplot(faithful, aes(x = eruptions, y= waiting)) + geom_point()
ggplot(faithful, aes(x= eruptions)) + geom_histogram(aes(y=..density..))
ggplot(faithful, aes(x= eruptions)) + geom_freqpoly(aes(y=..density..))
ggplot(faithful, aes(x= waiting)) + geom_histogram(aes(y=..density..))
#not normal at all

#obtain p values
cor.test(faithful$eruptions, faithful$waiting,
    method = c('spearman'))
cor.test(faithful$eruptions, faithful$waiting,
         method = c('pearson'))

#create a corrgram
corrgram(faithful, order=T, 
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         text.panel = panel.txt,
         cor.method = 'pearson')
corrgram(faithful, order=T, 
         lower.panel = panel.shade,
         upper.panel = panel.pts,
         text.panel = panel.txt,
         cor.method = 'spearman')


