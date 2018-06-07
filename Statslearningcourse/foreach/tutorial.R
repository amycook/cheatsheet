
library('foreach')
library('randomForest')
library('doMC')

# x output is a list
x<- foreach(i = 1:3) %do% sqrt(i)
x<- foreach(a = 1:3, b = rep(10,3)) %do% (a+b)
#use parenthases for functions instead of roudn brackets
x <- foreach(a = 1:3, b = rep(10,3)) %do% {
        a + b
}
# a and b are iteration variables since they change with each paralellisation
# note a and b change at the same time
# if only two values for b, and 1000 for a, list would only be length = 2

#if you dont want your results in a list:
x <- foreach(i=1:3, .combine='c') %do% exp(i) # can it rbind dataframes?
x <- foreach(i=1:4, .combine='cbind') %do% rnorm(4) #cbind to make a matrix

foreach(i=1:4, .combine='*') %do% rnorm(4) # multiplies the values from each parallel process
#write your own combining function 
cfun <- function(a, b) NULL
foreach(i=1:4, .combine='cfun') %do% rnorm(4)

# run a random forest parallel
x <- matrix(runif(500), 100)
y <- gl(2, 50)

foreach(ntree=rep(250, 4), .combine = combine, .packages = 'randomForest') %dopar% 
        randomForest(x,y,ntree = ntree)

# another tutorial: http://www.r-bloggers.com/the-wonders-of-foreach/

max.eig <- function(N, sigma) {
        d <- matrix(rnorm(N**2, sd = sigma), nrow = N)
        E <- eigen(d)$values
        abs(E)[[1]]
}
max.eig(5, 1)
E = sapply(1:10000, function(n) {max.eig(5, 1)})

#do a function a certain number of times
times(10) %do% max.eig(5, 1)
foreach(n = 1:5, .combine = c) %do% max.eig(n, 1)
#nested for loops
foreach(n = 1:5, .combine = rbind) %:% foreach(m = 1:3) %do% max.eig(n, m)
foreach(n = 1:5, m = 1:5, .combine = c) %do% max.eig(n, m)
# filtering
foreach(n = 1:100, .combine = c) %:% when (isPrime(n)) %do% n
#going parallel 
foreach(n = 1:5) %dopar% max.eig(n, 1)
#multicore - need doMC package
registerDoMC(cores=4)
#compare serial and parallel processes
system.time(foreach(n = 1:200) %do% max.eig(n, 1))
system.time(foreach(n = 1:200) %dopar% max.eig(n, 1))


#compare serial and parallel processes
system.time(times(200) %do% max.eig(120, 1))
system.time(times(200) %dopar% max.eig(120, 1))


# try your own rbinding
library(doSNOW)
cluster = makeCluster(2, type = 'SOCK')
registerDoSNOW(cluster)
cfun <- function(x,y){
        mapply(rbind, x, y)
}

whynot = foreach(1:2, .combine = cfun, .packages = c('magrittr','AACo','plyr','dplyr','purrr')) %dopar% {
        irre = mtcars %>% filter(mpg<19)
        a1<-c(1:5,rep(0,5))
        a2<-c(1:5,10:6)
        b2<-c(rep(100,5),rep(50,5))
        z<-c(rep("part1",5),rep("part2",5))
        df1<-data.frame(a1,z)
        df2<-data.frame(a2,b2,z)
        c = list(df1,df2)
        return(c)
}
stopCluster(cluster)

library('foreach')
library('doSNOW')
cluster = makeCluster(2, type = 'SOCK')
registerDoSNOW(cluster)
TESTING = new.env()
TESTING$VAR = 23

new.func <- function(mult = 2){
        ans = mult* TESTING$VAR
        return(ans)
}
whynot = foreach(1:2, .combine = c) %dopar% {
        ans.2 = new.func(2)

        return(ans.2)
}
stopCluster(cluster)


a1<-c(1:5,rep(0,5))
a2<-c(1:5,10:6)
b2<-c(rep(100,5),rep(50,5))
z<-c(rep("part1",5),rep("part2",5))
df1<-data.frame(a1,z)
df2<-data.frame(a2,b2,z)

mylist1<-list(df1,df2)
mylist2<-list(df1,df2)
mapply(rbind, mylist1, mylist2)
length(mylist2)




