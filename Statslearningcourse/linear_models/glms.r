
## POISSON - LOG LINK FUNCTION

# In the following example we fit a generalized linear model to count data using a Poisson error structure. 
# The data set consists of counts of high school students diagnosed with an infectious disease within a 
# period of days from an initial outbreak.

cases <-  
    structure(list(Days = c(1L, 2L, 3L, 3L, 4L, 4L, 4L, 6L, 7L, 8L, 
                            8L, 8L, 8L, 12L, 14L, 15L, 17L, 17L, 17L, 18L, 19L, 19L, 20L, 
                            23L, 23L, 23L, 24L, 24L, 25L, 26L, 27L, 28L, 29L, 34L, 36L, 36L, 
                            42L, 42L, 43L, 43L, 44L, 44L, 44L, 44L, 45L, 46L, 48L, 48L, 49L, 
                            49L, 53L, 53L, 53L, 54L, 55L, 56L, 56L, 58L, 60L, 63L, 65L, 67L, 
                            67L, 68L, 71L, 71L, 72L, 72L, 72L, 73L, 74L, 74L, 74L, 75L, 75L, 
                            80L, 81L, 81L, 81L, 81L, 88L, 88L, 90L, 93L, 93L, 94L, 95L, 95L, 
                            95L, 96L, 96L, 97L, 98L, 100L, 101L, 102L, 103L, 104L, 105L, 
                            106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 114L, 115L), 
                   Students = c(6L, 8L, 12L, 9L, 3L, 3L, 11L, 5L, 7L, 3L, 8L, 
                                4L, 6L, 8L, 3L, 6L, 3L, 2L, 2L, 6L, 3L, 7L, 7L, 2L, 2L, 8L, 
                                3L, 6L, 5L, 7L, 6L, 4L, 4L, 3L, 3L, 5L, 3L, 3L, 3L, 5L, 3L, 
                                5L, 6L, 3L, 3L, 3L, 3L, 2L, 3L, 1L, 3L, 3L, 5L, 4L, 4L, 3L, 
                                5L, 4L, 3L, 5L, 3L, 4L, 2L, 3L, 3L, 1L, 3L, 2L, 5L, 4L, 3L, 
                                0L, 3L, 3L, 4L, 0L, 3L, 3L, 4L, 0L, 2L, 2L, 1L, 1L, 2L, 0L, 
                                2L, 1L, 1L, 0L, 0L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 0L, 0L, 
                                0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L)), .Names = c("Days", "Students"
                                ), class = "data.frame", row.names = c(NA, -109L))

attach(cases)

head(cases) 

plot(Days, Students, xlab = "DAYS", ylab = "STUDENTS", pch = 16)

model1 <- glm(Students ~ Days, poisson)

summary(model1)

#The negative coefficient for Days indicates that as days increase, the mean number of 
# students with the disease is smaller.

plot(model1)
# theoretical quantiles need to be narrower to follow qqline. This means the model
# is prone to type 2 errors - rejecting a relationship when there is in fact one.

#Over-dispersion is a problem if the conditional variance (residual variance) is larger than the conditional 
#mean.  One way to check for and deal with over-dispersion is to run a quasi-poisson model, 
# which fits an extra dispersion parameter to account for that extra variance.

model2 <- glm(Students ~ Days, quasipoisson)

summary(model2)

# uses a dispersion parameter - .79, that tells us how many times bigger the variance is than the mean.
# parameter is <1, so conditional variance is smaller than conditional mean.

model2$coefficients

## plot
timeaxis <-seq (0,150,0.1)
Y <- predict(model2, list(Days = timeaxis))

# need to transform predict with exp
plot(Days, Students, xlab = "DAYS", ylab = "STUDENTS", pch = 16)
lines(timeaxis, exp(Y), lwd = 2, col = "blue")

# OR use, predict, type = "response" and it will be transformed
Z<- predict(model2, list(Days = timeaxis), type = 'response')

# need to transform predict with exp
plot(Days, Students, xlab = "DAYS", ylab = "STUDENTS", pch = 16)
lines(timeaxis, Z, lwd = 2, col = "red")

# examine coefs
coeffs <- exp(coef(model2))
coeffs

CI <- exp(confint.default(model2))

CI

#We can calculate the change in number of students presenting with the disease for each additional day
1 - 0.9826884



