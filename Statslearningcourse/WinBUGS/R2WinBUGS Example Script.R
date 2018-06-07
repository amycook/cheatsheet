# R2WinBUGS Example Script for implementing Bayes on the Beach 2015 model
#
# Created: 17/12/2015
# Updated: 
#==========================================================================

# Set filepaths
setwd("C:/Users/n9232371/OneDrive/shared files/Statslearningcourse/WinBUGS")
fp.wd <- getwd()
fp.csv <- paste(fp.wd,"/boys.csv",sep="")
fp.bugs.code <- paste(fp.wd,"/BUGS Code.bug",sep="")

# Load packages
library('R2WinBUGS', lib = 'C:/Progra~1/R/R-3.2.2/library')	# For calling WinBUGS

#==========================================================================

# Read in data
raw.data <- read.csv(fp.csv, header = T)			# Observations and Covariates

# Prep data (define covariates, observations, etc.)
x1 <- as.vector(raw.data[,2])		# Covariate - year of publication
# x2 <- as.matrix(raw.data[,3])			# Covariate - publication type
y <- as.vector(raw.data[,1])			# Observed citations per publication
N <- length(y)[1]				# Number of publications

# Create data list to be passed to WinBUGS
data <- list(y = y, N = N, Year = x1)

# Create list of initial values (usually randomise these using prior, maybe with 
# slighly smaller variance to avoid starting at extreme values.  Remember R uses 
# std. dev. for specifying the Normal distribution, while WinBUGS uses precision).
# Note: you need to initialise everything in your WinBUGS code that has a "~" symbol,
# UNLESS it is passed in as data, e.g. observed data (see above).
set.seed(1234)
initial.values <- function() {list(
	beta.0 = rnorm(1, 0, sd = 3),
	beta.1 = rnorm(1, 0, sd = 3),
	beta.2 = rnorm(1, 0, sd = 3),
	# beta.3 = rnorm(1, 0, sd = 3),
	gamma = rgamma(1, shape = 1, scale = 1)#,
	# eta.0 = #rnorm(1, 0, sd = 3),
	        # 0,
	# eta.1 = #rnorm(1, 0, sd = 3),
	        # -10,
	# eta.2 = #rnorm(1, 0, sd = 3)
	        # -10
)}

# Set parameters to monitor
parameters <- c("beta.0", "beta.1", "beta.2", "gamma", #"eta.0", 
	"eta.1", "eta.2", "lambda", "zeta")

#==========================================================================

# Call WinBUGS
burnin <- 2000
iterations <- 3000
bugs.run <- bugs(
        data = data,
        inits = initial.values,
        parameters.to.save = parameters,
        model.file = fp.bugs.code,
        n.chains = 1,
        n.iter = burnin + iterations,
        n.burnin = burnin,
        n.thin = 1,
        debug = TRUE,
        bugs.directory = "c:/Program Files/WinBUGS14/",
        program = c("WinBUGS"),
        DIC = FALSE		# You can compute this ex-post if required
)


# Save BUGS output data to disk
save(bugs.run, file = "BUGS Output.RData")

# EOF

