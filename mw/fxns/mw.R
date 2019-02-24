# this is simply the tidy mann-whitney procedure.
# the inputs are as follows:
# * `x`: a continuous vector of `length(N) = length(group)` of 
#       values for the Mann-Whitney test to evaluate
# * `group`: a character vector the same length as `x` that indicates 
#       which group the values in `x` came from
# The output is a U statistic.

mw <- function(x, group) {
  n1 <- length(x[group == "x"])
  n2 <- length(x[group == "y"])
  U1 <- n1*n2 + (n1*(n1 + 1)/2) - sum(rank(x)[group == "x"], na.rm = TRUE)
  U2 <- n1*n2 + (n2*(n2 + 1)/2) - sum(rank(x)[group == "y"], na.rm = TRUE)
  U <- min(U1, U2)
  return(U)
}