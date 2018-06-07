# forecast package
library(forecast)

# notes

#' Types of patterns:
#' 
#' * Trend: long term increase or decrease
#' * Seasonality: time of year or day of week
#' * Autocorrelation: current values impacted by previous values. need to find lag structure
#' * Stationary: no systematic trend
#' 
#' Types of methods
#' 
#' * mean: forecast for all future values = mean of all historical data
#'          meanf(x, h = 20)
#' * naiive: forecast = last observed value
#'          naive(x, h = 20)
#' * seasonal naive: forecast = last value from same season (same time in last season)
#'          snaive(x, h = 20)
#' * drift: last value + av. change
#'      equiv slope of a line drawn from first point to last point
#'      rwf(x, h = 20, drift = T)
#' * other methods: ses, holt, splinef, thetaf, croston, stlf, forecast()


# learning about the R time series object
dat <- sin(c(1:72)*2*pi/12) + c(1:72)/12
ex <- ts(dat, start= c(2009,1), end = c(2014,12), frequency = 12)
# subset the time series from June2014 to Dec2014
ex.sub <- window(ex, start = c(2014,6), end = c(2014,12))
plot(ex)

#seasonal decomposition
# seasonal, additive trend, and irregular components can be decomposed with stl()
# multiplicative effects must be log transformed first
fit <- stl(ex, s.window="period")
plot(fit)
# must be seasonal with a 12 month cycle to work well
# additional plots
monthplot(ex)
forecast::seasonplot(ex)

#exponential models
# holtwinters function or forecast::ets
# simple exponential - models level
fit <- HoltWinters(ex, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit <- HoltWinters(ex, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(ex)
fit <- forecast(fit, 3)
plot(forecast(fit, 3))

#arima model - autoregressive integrated moving averages
#   has three component order: AR order, degree of differencing, MA order
fit <- arima(ex, order=c(0,0,0))
             
# predictive accuracy
accuracy(fit)
# predict next 5 observations
forecast(fit, 5)
plot(forecast(fit, 5))
# prediciton intervals in arima are obtained by simulating many future sample paths

# FORECASTING

# Set up Data
data <- structure(c(12, 20.5, 21, 15.5, 15.3, 23.5, 24.5, 21.3, 23.5,
                    28, 24, 15.5, 17.3, 25.3, 25, 36.5, 36.5, 29.6, 30.5, 28, 26,
                    21.5, 19.7, 19, 16, 20.7, 26.5, 30.6, 32.3, 29.5, 28.3, 31.3,
                    32.2, 26.4, 23.4, 16.4, 15, 16, 18, 27, 21, 49, 21, 22, 28, 36,
                    40, 3, 21, 29, 62, 65, 46, 44, 33, 62, 22, 12, 24, 3, 5, 14,
                    36, 40, 49, 7, 52, 65, 17, 5, 17, 1),
                  .Dim = c(36L, 2L), .Dimnames = list(NULL, c("Advertising", "Sales")),
                  .Tsp = c(2006, 2008.91666666667, 12), class = c("mts", "ts", "matrix"))
head(data); nrow(data)
plot(data)


###########################################
# Moving average method
#   average values within k periods of t
#   best for stationary patterns
###########################################

moving_average = forecast(ma(data[1:31], order=2), h=5)
moving_average_accuracy = accuracy(moving_average, data[32:36])
moving_average; moving_average_accuracy
plot(moving_average, ylim=c(0,60))
lines(data[1:36])

# used the ETS method by default - error, trend and seasonality


###########################################
# Simple exponential smoothing
#   good when data has no trend or seasonal patterns
#   unlike ma, gives greater weight to most recent obs
###########################################

exp <- ses(data[1:31,1], 5, initial="simple")
exp_accuracy = accuracy(exp, data[32:36])
exp; exp_accuracy
plot(exp, ylim=c(0,60))
lines(data[1:36,1])

###########################################
# automatic method selection
#   runs through possible models and selects th best one
#   autoregressive of first order, ARIMA, something else?
###########################################

train = data[1:31,1]
test = data[32:36,1]
arma_fit <- auto.arima(train)
arma_forecast <- forecast(arma_fit, h = 5)
arma_fit_accuracy <- accuracy(arma_forecast, test)
arma_fit; arma_forecast; arma_fit_accuracy
plot(arma_forecast, ylim=c(0,60))
lines(data[1:36,1])




