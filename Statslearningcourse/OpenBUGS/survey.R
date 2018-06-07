####### bayesian tutorial - BUGS ###########

setwd("C:/Users/n9232371/OneDrive/shared files/Statslearningcourse/OpenBUGS")

# the following indiciates that a random variable y fits a binomial distribution with probability of success p and size N
#y is the number of smokers

y~ dbin(p,N)

#problem question... 
# use dataset sample
# denote p as proportion of smokers
# using uniform prior, find the mean and standard deviation of the posterior p

#prior distribution of p - between 0,1. this is Beta(1,1)

p~dbeta(1,1)

#with this, create the BUGS model

model <- function() {
        #Prior
        p~dbeta(1,1)
        
        #Likelihood
        y~ dbin(p,N)
}

#transfer the model to openBUGS. load the R2OpenBUGS extension
library('R2OpenBUGS',lib='C:/Progra~1/R/R-3.2.0/library')

# write the model to a temporary location using write.model
model.file <- file.path("C:/Users/n9232371/OneDrive/shared files/Statslearningcourse/OpenBUGS", "model.txt") 
write.model(model, model.file)
file.show(model.file)

#inspect data
library(MASS)
head(survey)
dim(survey)
summary(survey$Smoke)

#236 students. 47 smoke. denote these as Y and N
tbl <- table(survey$Smoke)
N <- as.numeric(sum(tbl))
y <- N - as.numeric(tbl["Never"])
data <- list("N", "y")

#identify variable p to be monitored in df 'params'
params<- c('p')

#finally we need some initial parameters for the simulation.
# Rule of thumb: choose values as close to the expected result as possible. In this case initialise p=0.5.
# wrap the initial values inside a list that is to be returned by a function

inits<- function() { list=(p=0.5) }

#then start OpenBUGS with the namesake method bugs and save the result in a variable out. choose 10,000 iterations 
# per simulation chain

out<- bugs(data, inits, params,
           model.file, n.iter= 10000) 









