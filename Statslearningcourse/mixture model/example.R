

library(mclust)

X <- c(rnorm(200, 10, 3), rnorm(200, 25,3), rnorm(200,35,3), rnorm(200,65, 3), rnorm(200,80,5))
Y <- c(rnorm(1000, 30, 2))
plot(X,Y, ylim = c(10, 60), pch = 19, col = "gray40")

xyMclust <- Mclust(data.frame (X,Y))
plot(xyMclust)

# classes are xyMclust$classification

