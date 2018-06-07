library('gRain', lib= 'C:/Progra~1/R/R-3.2.2/library')
library('gRbase', lib= 'C:/Progra~1/R/R-3.2.2/library')

#distribution of good and bad drivers

d<- cptable(~driver, values= c(0.75,0.25),
            levels= c("good", "bad"))

