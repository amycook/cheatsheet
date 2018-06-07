# Assess strenght of association between nominal and numeric variables using --- lm

data.df <- data.frame(
        topic = c(rep(c("Gossip", "Sports", "Weather"), each = 4)),
        duration  = c(6:9, 2:5, 4:7)
)
print(data.df)
boxplot(duration ~ topic, data = data.df, ylab = "Duration of conversation")

#model using lm against each other
model.lm <- lm(duration ~ topic, data = data.df)
summary(model.lm)

# R^2 = coefficient of determination. = 0.6809
# sqrt(R^2) = multiple correlation coefficient

rsq<- summary(model.lm)$r.squared
sqrt(rsq)

#sqrt(R^2) = 0.825137. 
# This represents correlation between the observed durations and predicted durations

print(model.lm$fitted)

#check pearson correlation between fitted and observed

cor.test(data.df$duration, model.lm$fitted)

#note all linear relationships.. 