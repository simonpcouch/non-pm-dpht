# this is the regular kruskal wallis procedure with tidy inputs
# x is a numeric variable, group is a factor variable with length = length(x)
# public is a logical indicating whether the test is public or private
# continuous random noise is added to every value if the test is not public
# to break ties. the output is an H test statistic.

kw <- function(x, group, public) {

  if (public == FALSE) {
    x <- x + rlaplace(n = length(x), s = 1e-9)
  }  
    
  group_rank <- function(subgroup, x) {
    n_i <- length(subset(x, group %in% subgroup))
    r_hat_i <- (mean(rank(x)[group %in% subgroup])^2)
    n_i*r_hat_i
  }
  
  term_1 <- 12/(length(x)*(length(x) + 1))
  term_2 <- sum(unlist(lapply(unique(group), 
                              group_rank, 
                              x = x)))
  term_3 <- 3*(length(x) + 1)
  
  H <- term_1 * term_2 - term_3
  
  return(H)
  
}