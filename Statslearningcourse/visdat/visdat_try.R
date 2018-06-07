library('devtools',lib='C:/Progra~1/R/R-3.2.3/library')
library('withr', lib = 'C:/Progra~1/R/R-3.2.3/library')
withr::with_libpaths(new = 'C:/Progra~1/R/R-3.2.3/library', install_github("tierneyn/visdat"))
library('visdat',lib='C:/Progra~1/R/R-3.2.3/library')
library('ggplot2',lib='C:/Progra~1/R/R-3.2.3/library')

all10mice <- read.csv('C:/Users/n9232371/OneDrive/shared files/Bligh Tanner/masters/data/all10mice.csv')[,-1]
p<- visdat::vis_dat(all10mice)
p

