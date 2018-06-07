# We simulate two queues in series.
set.seed(1L)
n_customers <- 100
arrival_df <- data.frame(ID = c(1:n_customers), times = rlnorm(n_customers, meanlog = 3))
service_1 <- rlnorm(n_customers)


firstqueue <- queue_step(arrival_df = arrival_df,
                         servers = 2, service = service_1)

server_list <- as.server.stepfun(c(50),c(1,2))

service_2 <- rlnorm(n_customers)
secondqueue <- queue_step(arrival_df = firstqueue,
                          servers = server_list, service = service_2)

curve(ecdf(arrival_df$times)(x) * n_customers , from = 0, to = 200,
      xlab = "time", ylab = "Number of customers")
curve(ecdf(firstqueue$times)(x) * n_customers , add = TRUE, col = "red")
curve(ecdf(secondqueue$times)(x) * n_customers, add = TRUE, col = "blue")
legend(100,40, legend = c("Customer input - arrivals",
                          "Customer output - firstqueue",
                          "Customer output - secondqueue"),
       col = c("black","red","blue"), lwd = 1, cex = 0.8
)

summary(firstqueue)
summary(secondqueue)