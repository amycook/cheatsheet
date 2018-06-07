#MAB210 markov chain - lecture 4 example question - sick teachers

A<-matrix(data=c(.4,-.6,.5,0,.06,.06,-.6,.5,.04,.04,.1,-.5,1,1,1,1), nrow=4, ncol=4, byrow= TRUE)
b<- matrix(c(0,0,0,1), nrow=4, ncol=1, byrow=FALSE)
solve(A,b)
