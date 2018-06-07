# http://varianceexplained.org/r/bayesian_ab_baseball/
        
library(dplyr)
library(tidyr)
library(Lahman)

# Grab career batting average of non-pitchers
# (allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>%
        group_by(playerID) %>%
        summarize(gamesPitched = sum(G)) %>%
        filter(gamesPitched > 3)

career <- Batting %>%
        filter(AB > 0) %>%
        anti_join(pitchers, by = "playerID") %>%
        group_by(playerID) %>%
        summarize(H = sum(H), AB = sum(AB)) %>%
        mutate(average = H / AB)

# Add player names
career <- Master %>%
        tbl_df() %>%
        select(playerID, nameFirst, nameLast) %>%
        unite(name, nameFirst, nameLast, sep = " ") %>%
        inner_join(career, by = "playerID")

# Estimate hyperparameters alpha0 and beta0 for empirical Bayes
career_filtered <- career %>% filter(AB >= 500)
m <- MASS::fitdistr(career_filtered$average, dbeta,
                    start = list(shape1 = 1, shape2 = 10))

alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

# For each player, update the beta prior based on the evidence
# to get posterior parameters alpha1 and beta1
career_eb <- career %>%
        mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0)) %>%
        mutate(alpha1 = H + alpha0,
               beta1 = AB - H + beta0) %>%
        arrange(desc(eb_estimate))

#Lets look at the two batters in question
# while we're at it, save them as separate objects too for later:
aaron <- career_eb %>% filter(name == "Hank Aaron")
piazza <- career_eb %>% filter(name == "Mike Piazza")
two_players <- bind_rows(aaron, piazza)

two_players

# is Piazza's true probability of getting a hit higher? Or is the difference due to chance?
# consider actual posterior distributions - range of plausible values for their true batting averages
# after we've taken teh evidence (batting record) into account. 
# These posterior distributions are modeled as beta distributions with the parameters:
# Beta(alpha0 + H, alpha0 + beta0 + H + AB)

library(broom)
library(ggplot2)
theme_set(theme_bw())

two_players %>%
        inflate(x = seq(.28, .33, .00025)) %>%
        mutate(density = dbeta(x, alpha1, beta1)) %>%
        ggplot(aes(x, density, color = name)) +
        geom_line() +
        labs(x = "Batting average", color = "")

# The posterior is a probabilistic representation of our uncertainty in each estimate
# When asking the probability Piazza is better we're asking 'if i drew a random draw from piazza's
# and a random draw from Aaron's, whats the probability Piazzas is higher?
# Curves overlap a lot! so high

# throw in a retired player - Hideki Matsui
Matsui <- career_eb %>% filter(name == "Hideki Matsui")
three_players <- bind_rows(two_players, Matsui)
three_players %>%
        inflate(x = seq(.26, .33, .00025)) %>%
        mutate(density = dbeta(x, alpha1, beta1)) %>%
        ggplot(aes(x, density, color = name)) +
        geom_line() +
        labs(x = "Batting average", color = "")

# it's very unlikely a random draw from Matsui would be higher than Piazza's or Aaron's

# how to find the probability that Piazza is better than Aaron within our model?
# looks greater than 50%, but probably not much greater
# need to know the probability one beta distribution is greater than another..
# not trivial to answer. four routes commonly used:
# simulation of posterior draws
# numerical integration
# closed form solution - may not be known or exist
# closed form approximation

# SIMULATION

#use each player's alpha1 and beta1 parameters, draw a million times from each using rbeta and
# compare results

piazza_simulation <- rbeta(1e6, piazza$alpha1, piazza$beta1)
aaron_simulation <- rbeta(1e6, aaron$alpha1, aaron$beta1)

sim <- mean(piazza_simulation > aaron_simulation)
sim

# that's 60.7% probability piazza is better than aaron!
# that's often good enough depending on need for precision and computational efficiency
# this is why bayesian simulation approaches like MCMC have become popular:
# computational power is cheap, doing maths is expensive as ever

# INTEGRATION

#the two posteriors have independent distributions. together they form a joint distribution
# that is density over particular pairs of x and y
# could imagine as density cloud

d <- .00002
limits <- seq(.29, .33, d)
sum(outer(limits, limits, function(x, y) {
        (x > y) *
                dbeta(x, piazza$alpha1, piazza$beta1) *
                dbeta(y, aaron$alpha1, aaron$beta1) *
                d ^ 2
}))

# finds fraction of cloud that has Piazza greater than Aaron
#brute force method, but unlike simulation, becomes intractable for problems with many dimensions


# CLOSED FORM SOLUTION

# find someone good at calculus
h <- function(alpha_a, beta_a,
              alpha_b, beta_b) {
        j <- seq.int(0, round(alpha_b) - 1)
        log_vals <- (lbeta(alpha_a + j, beta_a + beta_b) - log(beta_b + j) -
                             lbeta(1 + j, beta_b) - lbeta(alpha_a, beta_a))
        1 - sum(exp(log_vals))
}

h(piazza$alpha1, piazza$beta1,
  aaron$alpha1, aaron$beta1)

# not every problem has an exact solution like this - simulation rocks
# this is slow for large alpha_b and not straightforward to vectorize
# though still usually faster than simulation :)



# CLOSED FORM APPROXIMATION

#approximate the beta distributions in this case to be normal!
# probability one normal dist is greater than another is very easy to calculate!

h_approx <- function(alpha_a, beta_a,
                     alpha_b, beta_b) {
        u1 <- alpha_a / (alpha_a + beta_a) #mean? expected value
        u2 <- alpha_b / (alpha_b + beta_b) # mean?
        var1 <- alpha_a * beta_a / ((alpha_a + beta_a) ^ 2 * (alpha_a + beta_a + 1)) #variance
        var2 <- alpha_b * beta_b / ((alpha_b + beta_b) ^ 2 * (alpha_b + beta_b + 1))
        pnorm(0, u2 - u1, sqrt(var1 + var2))
}

h_approx(piazza$alpha1, piazza$beta1, aaron$alpha1, aaron$beta1)

#downside - for low alpha or low beta, the normal approximation is going to fit poorly


 




