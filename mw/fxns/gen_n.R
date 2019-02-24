# `gen_n` adds the random noise to sample sizes supplied to `gen_data` based 
# on the epsilon supplied. The arguments are as follows:
  
# * `n`: a vector (length 2) when `sum(n) = N`
# * `epsilons`: a vector (length 2) where the total privacy budget is `sum(epsilons)`
# * `delta`: a numeric--defaults to a "cryptographically small" number

# The function returns a privatized `n` vector.

gen_n <- function(n, epsilon, delta) {
  # if epsilon is NA, just return the public n
  if (is.na(epsilon[1])) {
    return(c(n[1], n[2]))
  } else {
    n1_hat <- n[1] + rlaplace(n = 1, m = 0, s = 1/(epsilon[1]))
    if (!is.na(delta)) {
    # if delta isn't NA, bound n1_hat by a function of delta
    n1_hat <- min(n1_hat, sum(n) - n1_hat) - log(1/delta)/epsilon[1] }
    if (n1_hat > 0) {
      return(c(round(n1_hat), sum(n) - round(n1_hat)))
    } else {
      return(c(0, sum(n)))
    }
  }
}
