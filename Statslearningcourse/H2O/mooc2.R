
library("dplyr")
library("ggplot2")
library("h2o")

# start the H2O server locally
localH2O = h2o.init(ip = "127.0.0.1", port = 54321)
filePath = "C:/Users/n9232371/OneDrive/shared files/Statslearningcourse/H2O/FLbigdataStats-master/bank_customer_data.csv"
market_data <- h2o.uploadFile(filePath,
                              destination_frame = "",
                              parse = T,
                              header = T,
                              sep = ",",
                              na.strings = c("unknown"),
                              progressBar = FALSE,
                              parse_type = "CSV") 
# the H2) server is local so the data ends up in our ram. if the server were the cloud, data would be stored there
#print first 6 rows of the data
market_data
summary(market_data)

#split the data into 20/80, keep the 20% slice
sample_frame <- h2o.splitFrame(market_data, ratio = 0.2)[[1]]
market_data_sample <- as.data.frame(sample_frame)

#view offer take-up
by_y_job <- market_data_sample %>% group_by(y, job) %>% tally()

#plot
ggplot(by_y_job, aes(x = job, y = n, fill = y)) + geom_bar(stat = "identity", position = "fill")

# predict which customer will accept sale
# remove length of call - no way of knowing this before call
market_dataex1 <- market_data[,-11] 
split_data <- h2o.splitFrame(market_dataex1, ratios=0.75)
train_data <- split_data[[1]]
validation_data <- split_data[[2]] 

#fit the model
glm_model = h2o.glm(x = 1:19,
                    y=20,
                    training_frame = train_data,
                    validation_frame = validation_data,
                    max_iterations = 100,
                    solver="L_BFGS",
                    family="binomial",
                    alpha = 1, #L2 regularisation
                    intercept = T) 
summary(glm_model) 




