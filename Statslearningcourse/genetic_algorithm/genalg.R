
# genetic algorithm tutorial 
# https://www.r-bloggers.com/genetic-algorithms-a-simple-r-example/


library(genalg)
library(ggplot2)

# optimise combination of survival gear. gear cannot weigh more than 20 kg

dataset <- data.frame(item = c("pocketknife", "beans", "potatoes", "unions", 
                               "sleeping bag", "rope", "compass"), survivalpoints = c(10, 20, 15, 2, 30, 
                                                                                      10, 30), weight = c(1, 5, 10, 1, 7, 5, 1))
weightlimit <- 20

# The genalg algorithm tries to optimize towards the minimum value.

# evaluation function

evalFunc <- function(x) {
        
        current_solution_survivalpoints <- x %*% dataset$survivalpoints
        current_solution_weight <- x %*% dataset$weight
        
        if (current_solution_weight > weightlimit) 
                return(0) else return(-current_solution_survivalpoints)
}

# Next, we choose the number of iterations, design and run the model.

iter = 100
GAmodel <- rbga.bin(size = 7, popSize = 200, iters = iter, mutationChance = 0.01, 
                    elitism = T, evalFunc = evalFunc)

cat(summary.rbga(GAmodel))


