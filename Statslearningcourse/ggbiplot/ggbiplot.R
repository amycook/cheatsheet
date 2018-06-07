##try ggbiplot
library(devtools,lib='C:/Progra~1/R/R-3.2.1/library')
library('ggbiplot',lib='C:/Progra~1/R/R-3.2.1/library')
#visualise clusters
q<- ggbiplot(p, obs.scale = 1, var.scale = 1, 
             groups = all7c$group.num %>% as.factor, ellipse = TRUE,
             labels = all7c$group.num,
             circle = TRUE)
q <- q + scale_color_discrete(name = '')
q <- q + theme(legend.direction = 'vertical', 
               legend.position = 'right')
q



