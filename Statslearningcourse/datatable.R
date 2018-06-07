#data.table tute
#written in C so much faster than dataframes. 
   
DF = data.frame(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DF,3)
DT = data.table(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
tables()
DT[DT$y=='a']
#subsets based on rows if you use only one index in subset []
DT[c(2,3)]
#can't subset columns like you usually would in a data frame. but you can perform functions on columns in the 
#place you would normally have the column name, for example..
#also dont have to write "" around the column names...
DT[,list(mean(x),sum(z))]
DT[,table(y)]
#adds new columns really fast, use := symbol
DT[,w:=z^2]
DT
#if making a copy of a data table, ie B<-DT. make sure you make a 'copy' or if you change DT, B will change as well.
#can perform multi step operations. separate each step with a ;
DT[,m:= {tmp <- (x+z); log2(tmp+5)}]
DT
#can group by in a similar way to plyr
DT[,a:=x>0]
DT[,b:= mean(x+w),by=a]
DT
#above function gives the mean of all 'TRUE' a's and mean of all 'FALSE' a's

#the package has special functions, such as .N. .N is basically the 'count' function. much faster than table()
DT <- data.table(x=sample(letters[1:3], 1E5, TRUE))
DT[, .N, by=x]
#can set a 'key' which each command refers to by default. for example we will set it as the column x. 
#then if the subset command is performed, it will perform it on x column auto.
DT <- data.table(x=rep(c("a","b","c"),each=100), y=rnorm(300))
setkey(DT, x)
DT['a']
#can merge fast, set two dataframes to have the same key column
DT1 <- data.table(x=c('a', 'a', 'b', 'dt1'), y=1:4)
DT2 <- data.table(x=c('a', 'b', 'dt2'), z=5:7)
setkey(DT1, x); setkey(DT2, x)
merge(DT1, DT2)
#much faster to read files. 19 times faster
#fread(file)
