# `gen_null` generates a null distribution of test statistics with a given 
# sample size `n` and privacy parameter `epsilon`. reps is the
# number of simulations to carry out.

gen_null <- function(n, epsilon, reps) {
  s <- sqrt(n*(n+1)*(2*n+1)/6)
  Z <- rnorm(n = reps,
             mean = 0,
             sd = s)
  if (is.na(epsilon)) {
    return(Z)
  } else {
    noise <- rlaplace(n = reps,
                      m = 0,
                      s = 2*n/(epsilon))  
    return(noise + Z)
  }
}