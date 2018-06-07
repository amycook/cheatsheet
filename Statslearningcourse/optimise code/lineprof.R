
library('lineprof', lib= 'C:/Progra~1/R/R-3.2.3/library')
source("C:/Users/n9232371/Documents/AACo/sourcetree/forward_model/AACo/R/fwd_model.R")
l <- lineprof(property.list(prop.vecs = a.prop.vec2, births.loc = breed.locs, 
                            prop.char.df = prop.chars, feedlots = 'Custom Feeding Non Wagyu',
                            mvmts.df = mvmts.1824 %>% filter(day<= days.max), days = days.max))
l
shine(l)
