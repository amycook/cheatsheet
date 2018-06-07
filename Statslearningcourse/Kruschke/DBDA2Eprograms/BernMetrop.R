
library(ggplot2)
library(cowplot)

setwd("C:/Users/cook79166/OneDrive/shared files/Statslearningcourse/Kruschke/DBDA2Eprograms")
graphics.off()
rm(list=ls(all=TRUE))
fileNameRoot="BernMetrop" # for output filenames
source("DBDA2E-utilities.R")

# Specify the data, to be used in the likelihood function.
# myData = c(rep(0,6),rep(1,14))
myData = c()

# Define the Bernoulli likelihood function, p(D|theta).
# The argument theta could be a vector, not just a scalar.
# theta is probability in bernoulli
likelihood = function( theta , data ) {
  z = sum( data )
  N = length( data )
  pDataGivenTheta = theta^z * (1-theta)^(N-z)
  # The theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # The likelihood for theta > 1 or for theta < 0 is zero:
  pDataGivenTheta[ theta > 1 | theta < 0 ] = 0
  return( pDataGivenTheta )
}

# likelihood: 
# ggplot(data = data.frame(x = seq(0,1,0.01)), aes(x = x, y = x^10*(1-x)^(50-10))) + geom_point()

# Define the prior density function. 
prior = function( theta ) {
  # pTheta = dbeta( theta , 1 , 1 )
  pTheta = (cos(4*pi*theta) + 1)^2/1.5
  # The theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # The prior for theta > 1 or for theta < 0 is zero:
  pTheta[ theta > 1 | theta < 0 ] = 0
  return( pTheta )
}

# prior: 
# ggplot(data = data.frame(x = seq(0,1,0.01)), aes(x = x, y = (cos(4*pi*x) + 1)^(2/1.5))) + geom_point()


# Define the relative probability (posterior?) of the target distribution, 
# as a function of vector theta. For our application, this
# target distribution is the unnormalized posterior distribution - i.e. no denominator?.

targetRelProb = function( theta , data ) {
  targetRelProb =  likelihood( theta , data ) * prior( theta )
  return( targetRelProb )
}

# Specify the length of the trajectory, i.e., the number of jumps to try:
trajLength = 50000 # arbitrary large number
# Initialize the vector that will store the results:
trajectory = rep( 0 , trajLength )
# Specify where to start the trajectory:
trajectory[1] = 0.01 # arbitrary value
# Specify the burn-in period:
burnIn = ceiling( 0.0 * trajLength ) # arbitrary number, less than trajLength
# Initialize accepted, rejected counters, just to monitor performance:
nAccepted = 0
nRejected = 0

# Now generate the random walk. The 't' index is time or trial in the walk.
# Specify seed to reproduce same random walk:
set.seed(47405)
# Specify standard deviation of proposal distribution:
# the chain wanders from state to state based on some proposal distribution 
shorter <- function(proposal.index = 2, myData = c()){

        proposalSD = c(0.02,0.2,2.0)[proposal.index]

        for ( t in 1:(trajLength-1) ) {
	currentPosition = trajectory[t]
	# Use the proposal distribution to generate a proposed jump.
	proposedJump = rnorm( 1 , mean=0 , sd=proposalSD )
	# Compute the probability of accepting the proposed jump.
	probAccept = min( 1,
		targetRelProb( currentPosition + proposedJump , myData )
		# posterior probability of new position
		# theta is position! x-axis position?
		/ targetRelProb( currentPosition , myData ))
		# posterior probability of current position
	# Generate a random uniform value from the interval [0,1] to
	# decide whether or not to accept the proposed jump.
	if ( runif(1) < probAccept ) {
		# accept the proposed jump
		trajectory[ t+1 ] = currentPosition + proposedJump
		# increment the accepted counter, just to monitor performance
		if ( t > burnIn ) { nAccepted = nAccepted + 1 }
	} else {
		# reject the proposed jump, stay at current position
		trajectory[ t+1 ] = currentPosition
		# increment the rejected counter, just to monitor performance
		if ( t > burnIn ) { nRejected = nRejected + 1 }
	}
}

# Extract the post-burnIn portion of the trajectory.
acceptedTraj = trajectory[ (burnIn+1) : length(trajectory) ]

# End of Metropolis algorithm.

#-----------------------------------------------------------------------
# Display the chain.

a <- ggplot(as.data.frame(acceptedTraj), aes(x = acceptedTraj)) +
        geom_histogram(bins = 50)



# openGraph(width=4,height=8)
# layout( matrix(1:3,nrow=3) )
# par(mar=c(3,4,2,1),mgp=c(2,0.7,0))
# 
# # Posterior histogram:
# paramInfo = plotPost( acceptedTraj , xlim=c(0,1) , xlab=bquote(theta) , 
#                       cex.main=2.0 ,
#                       main=bquote( list( "Prpsl.SD" == .(proposalSD) ,
#                       "Eff.Sz." == .(round(effectiveSize(acceptedTraj),1)) ) ) )

# Trajectory, a.k.a. trace plot, end of chain:
idxToPlot = (trajLength-100):trajLength
b <- ggplot(data.frame(pos = trajectory[idxToPlot],
                  iter = idxToPlot), aes(x = iter, y = pos)) + 
        geom_line(size = 0.3) + coord_flip() +
        labs(title = "End of Chain") +
        scale_y_continuous(limits = c(0,1))
# plot( trajectory[idxToPlot] , idxToPlot , main="End of Chain" ,
#       xlab=bquote(theta) , xlim=c(0,1) , ylab="Step in Chain" ,
#       type="o" , pch=20 , col="skyblue" , cex.lab=1.5 )
# Display proposal SD and acceptance ratio in the plot.
# text( 0.0 , trajLength , adj=c(0.0,1.1) , cex=1.75 ,
#       labels = bquote( frac(N[acc],N[pro]) == 
#                        .(signif( nAccepted/length(acceptedTraj) , 3 ))))

# Trajectory, a.k.a. trace plot, beginning of chain:
idxToPlot2 = 1:100
# plot( trajectory[idxToPlot] , idxToPlot , main="Beginning of Chain" ,
#       xlab=bquote(theta) , xlim=c(0,1) , ylab="Step in Chain" ,
#       type="o" , pch=20 , col="skyblue" , cex.lab=1.5 )
c <- ggplot(data.frame(pos = trajectory[idxToPlot2],
                       iter = idxToPlot2), aes(x = iter, y = pos)) + 
        geom_line(size = 0.3) + coord_flip() +
        labs(title = "Start of Chain") +
        scale_y_continuous(limits = c(0,1))

plot_grid(a, b, c, ncol = 1)
}

shorter(proposal.index = 2, myData = c())
shorter(proposal.index = 2, myData = c(0,1,1))
shorter(proposal.index = 1, myData = c(0,1,1))

trajectory[1] = 0.99
shorter(proposal.index = 1, myData = c(0,1,1))


# test

x <- seq(0,1,0.01)
ggplot(as.data.frame(x), aes(x = x, 
                             y = (cos(4*pi*x) + 1)^2/1.5)) +
        geom_line()


# Indicate burn in limit (might not be visible if not in range):
# if ( burnIn > 0 ) {
#   abline(h=burnIn,lty="dotted")
#   text( 0.5 , burnIn+1 , "Burn In" , adj=c(0.5,1.1) )
# }

#saveGraph( file=paste0( fileNameRoot , 
#                        "SD" , proposalSD ,
#                        "Init" , trajectory[1] ) , type="eps" )

#------------------------------------------------------------------------
