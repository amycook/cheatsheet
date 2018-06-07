# fast fourier transform tutorial

# http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html

xs <- seq(-2*pi,2*pi,pi/100)
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)
par(mfrow = c(1, 2))
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)



# which can be linearly combined into a complex wave:
        
wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)

# FOURIER SERIES
# Joseph Fourier showed that any periodic wave can be represented by a sum of simple sine waves. 
# This sum is called the Fourier Series. The Fourier Series only holds while the system is linear. 
# If there is, eg, some overflow effect (a threshold where the output remains the same no matter how much input is given), 
# a non-linear effect enters the picture, breaking the sinusoidal wave and the superposition principle.

#Also, the Fourier Series only holds if the waves are periodic,
# ie, they have a repeating pattern (non periodic waves are dealt by the Fourier Transform, see below). 
# A periodic wave has a frequency ff and a wavelength λλ (a wavelength is the distance in the medium between 
# the beginning and end of a cycle, λ=v/f0λ=v/f0, where vv is the wave velocity) that are defined by the repeating pattern. 
# A non-periodic wave does not have a frequency or wavelength.


# Here’s a R function for plotting trajectories given a fourier series:
        
        plot.fourier <- function(fourier.series, f.0, ts) {
                w <- 2*pi*f.0
                trajectory <- sapply(ts, function(t) fourier.series(t,w))
                plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
        }

# An eg
plot.fourier(function(t,w) {sin(w*t)}, 1, ts=seq(0,1,1/100)) 

acq.freq <- 100                    # data acquisition frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time,1/acq.freq) # vector of sampling time-points (s) 
f.0      <- 1/time                 # fundamental frequency (Hz)

dc.component       <- 0
component.freqs    <- c(3,10)      # frequency of signal components (Hz)
component.delay    <- c(0,0)       # delay of signal components (radians)
component.strength <- c(.5,.25)    # strength of signal components

f <- function(t,w) { 
        dc.component + 
                sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts) 

# phase shift
component.delay <- c(pi/2,0)       # delay of signal components (radians)
plot.fourier(f,f.0,ts)

# translation over y axis
dc.component <- -2
plot.fourier(f,f.0,ts)

# Each wave component ak × sin(kwt+ρk) is also called a harmonic.

# The Fourier Transform (FT) is a generalization to solve for non-periodic waves. 
# The FT assumes that the finite analyzed segment corresponds to one period of an infinitely extended periodic signal.


library(stats)

# Anyway, here’s a function that applies the previous equation, ie, makes the IFT:
        
        # returns the x.n time series for a given time sequence (ts) and
        # a vector with the amount of frequencies k in the signal (X.k)
        get.trajectory <- function(X.k,ts,acq.freq) {
                
                N   <- length(ts)
                i   <- complex(real = 0, imaginary = 1)
                x.n <- rep(0,N)           # create vector to keep the trajectory
                ks  <- 0:(length(X.k)-1)
                
                for(n in 0:(N-1)) {       # compute each time point x_n based on freqs X.k
                        x.n[n+1] <- sum(X.k * exp(i*2*pi*ks*n/N)) / N
                }
                
                x.n * acq.freq 
        }

# Here’s two useful functions:
# plot.frequency.spectrum() plot a frequency spectrum of a given XkXk
# plot.harmonic() plots the i-th harmonic on the current plot

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
        plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
        
        # TODO: why this scaling is necessary?
        plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
        
        plot(plot.data, t="h", lwd=2, main="", 
             xlab="Frequency (Hz)", ylab="Strength", 
             xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
        Xk.h <- rep(0,length(Xk))
        Xk.h[i+1] <- Xk[i+1] # i-th harmonic
        harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
        points(ts, harmonic.trajectory, type="l", col=color)
}

# Let’s check that last eg. Notice that this plot is equal to the blue line in the animation:
        
X.k <- fft(c(4,0,0,0))                   # get amount of each frequency k

time     <- 4                            # measuring time interval (seconds)
acq.freq <- 100                          # data acquisition frequency (Hz)
ts  <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 

x.n <- get.trajectory(X.k,ts,acq.freq)   # create time wave

plot(ts,x.n,type="l",ylim=c(-2,4),lwd=2)
abline(v=0:time,h=-2:4,lty=3); abline(h=0)

plot.harmonic(X.k,1,ts,acq.freq,"red")
plot.harmonic(X.k,2,ts,acq.freq,"green")
plot.harmonic(X.k,3,ts,acq.freq,"blue")

# Example

acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time

dc.component <- 1
component.freqs <- c(3,7,10)        # frequency of signal components (Hz)
component.delay <- c(0,0,0)         # delay of signal components (radians)
component.strength <- c(1.5,.5,.75) # strength of signal components

f   <- function(t,w) { 
        dc.component + 
                sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts=ts)

# Let’s assume that we don’t know the functional form of trajectory, we only have its contents, the period and the sampling time points:
        
        w <- 2*pi*f.0
trajectory <- sapply(ts, function(t) f(t,w))
head(trajectory,n=30)

# So, given that trajectory we can find where the frequency peaks are:
        
X.k <- fft(trajectory)                   # find all harmonics with fft()
plot.frequency.spectrum(X.k, xlimits=c(0,20))

# And if we only had the frequency peaks we could rebuild the signal:
        
x.n <- get.trajectory(X.k,ts,acq.freq) / acq.freq  # TODO: why the scaling?
plot(ts,x.n, type="l"); abline(h=0,lty=3)
points(ts,trajectory,col="red",type="l") # compare with original

# try with real data

library(zoo)

prices <- read.csv("C:/Users/cook79166/OneDrive/shared files/Statslearningcourse/fast_fourier_transform/FRED-RSGASSN.csv")
prices <- prices[order(nrow(prices):1),]  # revert data frame
plot(prices, type="l")

trend <- lm(VALUE ~ index(DATE), data = prices)
abline(trend, col="red")
# take out upward trend
detrended.trajectory <- trend$residuals
plot(detrended.trajectory, type="l", main="detrended time series")

library(GeneCycle)
f.data <- GeneCycle::periodogram(detrended.trajectory)
harmonics <- 1:30
plot(f.data$freq[harmonics]*length(detrended.trajectory), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")


# biggest harmonics at 25 months, 4 months and 1 month






