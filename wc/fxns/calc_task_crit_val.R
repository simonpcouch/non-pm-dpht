# load rmutil for the laplace functions
library(rmutil)

# this function finds Task's critical values for a given sample size n
# and significance level alpha. A few notes:
# 1) alpha is the resultant significance level once laplacian noise is 
# accounted for 
# 2) this function does not work for alphas less than or equal to .01.
# 3) the values outputted by this function are the critical values with the
# correct density function of the laplace, not those actually found in 
# the Task 2016 paper

calc_task_crit_val <- function(n, alpha, epsilon) {
  
  # find the new alpha, accounting for a 99% noise bound
  new_alpha <- (alpha - .01)/.99
  
  # find the z value of new_alpha
  z_norm <- qnorm(p = 1 - new_alpha)
  
  # find the noise bound as a function of sample size
  noise_bound <- qlaplace(p = .99,
                          m = 0,
                          s = 2*n/(sqrt(n*(n+1)*(2*n+1)/6)*epsilon))
  # return their sum
  return(z_norm + noise_bound)  
}