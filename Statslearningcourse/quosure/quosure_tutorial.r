
# dplyr quasiquotation
# quo
# quosures

mutate_y <- function(df) {
        mutate(df, y = a + x)
}

df1 <- tibble(x = 1:3)
a <- 10
mutate_y(df1)

# can specify that a  must come from within the dataframe
mutate_y <- function(df) {
        mutate(df, y = .data$a + .data$x)
}
mutate_y(df1)

# this does not work
df <- tibble(
        g1 = c(1, 1, 2, 2, 2),
        g2 = c(1, 2, 1, 2, 1),
        a = sample(5), 
        b = sample(5)
)

my_summarise <- function(df, group_var) {
        df %>%
                group_by(group_var) %>%
                summarise(a = mean(a))
}

my_summarise(df, 'g1')
# doesnt work because group_by() doesn't evaluate input, it quotes it first.

#therefore, need to 
#' 1. need to quote the input into the function ourselves
#'        * can't use "" because that is a string.
#'        * need something that captures the expression and the environment
#'        * use quo()
#'            * returns a quosure

#' 2. need to tell group_by not to quote the input
#'        * use !! to tell group_by to not quote its input

my_summarise <- function(df, group_var) {
        df %>%
                group_by(!!group_var) %>%
                summarise(a = mean(a))
}
my_summarise(df, quo(g1))

# but don't want user to have to write quo(g1), so use enquo() to perform lazy evaluation
# enquo turns an argument into a quosure

my_summarise <- function(df, col) {
        col <- enquo(col)
        print(col) # prove col input is now a quosure
        
        df %>%
                group_by(!!col) %>%
                summarise(a = mean(a))
}

my_summarise(df,g1)


# something harder
my_var <- quo(a)
summarise(df, mean = mean(!!my_var), sum = sum(!!my_var), n = n())
# you can wrap quo around teh whole thing - good tool for debugging?
quo(summarise(df, mean(!!my_var), sum(!!my_var), n = n()))
# and as a function
new_sum <- function(df, col){
        col = enquo(col)
        
        summarise(df, mean(!!col), sum(!!col), n = n())
}
new_sum(df, a)

# what if you want to make new colnames?!
my_mutate <- function(df, col) {
        col <- enquo(col)
        mean_name <- paste0("mean_", quo_name(col))
        sum_name <- paste0("sum_", quo_name(col))
        
        mutate(df, 
               !!mean_name := mean(!!col), 
               !!sum_name := sum(!!col)
        )
}

# capturing multiple variables into say group_by
# need to enquo() the ..., but use quos()
# need to use !!! to split up the variables inside group_by
my_groupby <- function(df, ...){
        vars = quos(...)
        
        df %>% group_by(!!!vars) %>% summarise(a = mean(a))
}
my_groupby(df, g1, g2)


new <- function(df, col){
        
        # df %>% select(col)
        # df[, a]
        df %>% filter(col == 2)
}

new(df, 'g2')
new(df, g2)



