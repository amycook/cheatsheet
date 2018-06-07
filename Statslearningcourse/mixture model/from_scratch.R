# good explanation of the difference between kmeans and mixture models:
# https://stats.stackexchange.com/questions/69424/if-k-means-clustering-is-a-form-of-gaussian-mixture-modeling-can-it-be-used-whe


library(mclust)
# Mclust comes with a method of hierarchical clustering. We'll
# initialize 3 different classes.
initialk <- mclust::hc(data = iris, modelName = "EII")
initialk <- mclust::hclass(initialk, 3)

# First split by class and calculate column-means for each class.
mu <- split(iris[, 1:4], initialk)
mu <- t(sapply(mu, colMeans))

# Covariance Matrix for each initial class.
cov <- list(diag(4), diag(4), diag(4))

# Mixing Components
a <- runif(3)
a <- a/sum(a)

# Covariance Matrix for each initial class.
cov <- list(diag(4), diag(4), diag(4))

# Mixing Components
a <- runif(3)
a <- a/sum(a)


library(NPflow)
# Calculate PDF with class means and covariances.
z <- cbind(mvpdf(x = iris[, 1:4], mu = mu[1, ], sigma = cov[[1]]), 
           mvpdf(x = iris[, 1:4], mu = mu[2, ], sigma = cov[[2]]), 
           mvpdf(x = iris[, 1:4], mu = mu[3, 
                                                                                                                                                        ], sigma = cov[[3]]))

# Expectation Step for each class.
r <- cbind((a[1] * z[, 1])/rowSums(t((t(z) * a))), (a[2] * z[, 2])/rowSums(t((t(z) * 
                                                                                  a))), (a[3] * z[, 3])/rowSums(t((t(z) * a))))

# Choose the highest rowwise probability
eK <- factor(apply(r, 1, which.max))
Now let’s begin to update

# Total Responsibility
mc <- colSums(r)

# Update Mixing Components.
a <- mc/NROW(iris)

# Update our Means
mu <- rbind(colSums(iris[, 1:4] * r[, 1]) * 1/mc[1], colSums(iris[, 1:4] * 
                                                                 r[, 2]) * 1/mc[2], colSums(iris[, 1:4] * r[, 3]) * 1/mc[3])


# Update Covariance matrix.
cov[[1]] <- t(r[, 1] * t(apply(iris[, 1:4], 1, function(x) x - mu[1, ]))) %*% 
    (r[, 1] * t(apply(iris[, 1:4], 1, function(x) x - mu[1, ]))) * 1/mc[1]

cov[[2]] <- t(r[, 2] * t(apply(iris[, 1:4], 1, function(x) x - mu[2, ]))) %*% 
    (r[, 2] * t(apply(iris[, 1:4], 1, function(x) x - mu[2, ]))) * 1/mc[2]

cov[[3]] <- t(r[, 3] * t(apply(iris[, 1:4], 1, function(x) x - mu[3, ]))) %*% 
    (r[, 3] * t(apply(iris[, 1:4], 1, function(x) x - mu[3, ]))) * 1/mc[3]



# just use mclust package

# Load the package
library(mclust)

# Select 4 continuous variables and look for three distinct groups.
mcl.model <- Mclust(iris[, 1:4], 3)

# Plot our results.
plot(mcl.model, what = "classification", main = "Mclust Classification")
table(iris$Species, mcl.model$classification)

# compare to kmeans
km <- kmeans(scale(iris[, 1:4]), 3, iter.max = 50000)
table(iris$Species, km$cluster)

# compare to hclust
x<- dist(scale(iris[, 1:4]), method = 'euclidean')
h <- hclust(x, method = 'ward.D2')
plot(h)
h <- cutree(h, k=3)
table(iris$Species, h)


