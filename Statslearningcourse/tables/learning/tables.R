library('tables',lib='C:/Progra~1/R/R-3.1.3/library')
x<-tabular( (Species + 1) ~ (n=1) + Format(digits=2)*(Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
write.csv.tabular(x, 'iris.csv')

y<-read.csv('Genspec.csv')

tabular((Order)*(Family)~(n=1), data=y, drop=TRUE)
