library(tidyr)
library(purrr)
library(broom)
library(dplyr)

head(iris)

lr <- lm(Sepal.Length ~ ., data = iris) 
lr
str(lr)

lr$coefficients
lr$residuals
summary(lr)

i.t <- as_data_frame(iris)
nested <- i.t %>% group_by(Species) %>% nest()

nested <- nested %>% mutate(
        lms = map(data, function(x) lm(Sepal.Length ~ ., data = x)),
        r2 = map_dbl(lms, function(x) summary(x)$r.squared %>% round(2)))





