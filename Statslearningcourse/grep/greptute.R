# grep training
#grepl is grep and output is logical vector
df<- data.frame('actor'= c("military forces of guinea-bissau",
                           "mutiny of military forces of guinea-bissau" ,
                           "unidentified armed group (guinea-bissau)" ,
                           "mfdc: movement of democratic forces of casamance"),
                'country'=c(rep("guinea-bissau",4)))
df

df$actor<- as.character(df$actor)
df$country<- as.character(df$country)

#grepl multiple exclusions and inclusions together
ifelse(!grepl('mutiny of',df$actor) &
               grepl('military forces of',df$actor)&
               apply(df,1, function(x) grepl(x[2], x[1])),
       1,df$actor)

#more grep training
#grepl military OR mutiny
ifelse(grepl('mutiny|military',df$actor),
       1, df$country)
#OR
ifelse(grepl(paste(c("mutiny","military"),collapse="|"),df$actor),
       1, df$country)





