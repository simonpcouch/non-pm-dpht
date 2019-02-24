# kw_new is the new procedure for the Kruskal Wallis test:
# the absolute value of n_i is now taken instead of squaring
# x is a numeric variable, group is a factor variable with length = length(x)
# public is a logical indicating whether the test is public or private
# continuous random noise is added to all inputs to break ties if the test 
# is not public. the output is an "H_hat" test statistic

kw_new <- function(x, group, public) {
  
  if (public == FALSE) {
  x <- x + rlaplace(n = length(x), s = 1e-9)
  }
  
  N <- length(x)
  
  if ((N %% 2) == 0) {
    # procedure for even numbers
    term_1 <- 4*(N - 1) / (N^2) 
  } else {
    # procedure for odd numbers
    term_1 <- 4 / (N + 1)
  }
  
  
  group_rank <- function(subgroup, x) {
    # number of observations in the subgroup
    n_i <- length(subset(x, group %in% subgroup))
    # average rank of observations in the subgroup 
    r_hat_i <-  mean(rank(x)[group %in% subgroup])
    # average rank
    r_hat <- (N+1)/2
    # compute 1 element of the sum
    n_i * abs(r_hat_i - r_hat)
  }
  
  term_2 <- sum(unlist(lapply(unique(group), 
                              group_rank, 
                              x = x)))  
  
  H_hat <- term_1 * term_2
  
  return(H_hat)
  
}