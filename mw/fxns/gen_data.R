# `gen_data` is a function that generates samples of `x` and `group` to 
# run through `mw`, as well as noise to add to each trial. The arguments 
# are as follows:
# 
# * `n`: a vector (length 2) of desired sample sizes
# * `mean`: a vector (length 2) of means to draw the two samples with
# * `sd`: a vector (length 2) of means to draw the two samples with
# * `reps`: a numeric of repititions to generate
# * `epsilons`: a vector (length 2) of amount of epsilon to dedicate to 
#       `n1` and `U`, respectively. The sum of `epsilons` is the total 
#       privacy budget.
#
# The output is a dataframe of values randomly generated with specified 
# parameters that can be fed to `calc_stat`.

gen_data <- function(n, new_n = NULL, mean, sd, reps, epsilons = c(NA, NA)) {
  if (is.na(epsilons[2])) {
    data_frame(x = rnorm(n = reps*sum(n), 
                         mean = rep(rep(mean, n), reps),
                         sd = rep(rep(sd, n), reps)),
               group = rep(rep(c("x", "y"), n), reps),
               replicate = as.factor(rep(1:reps, each = sum(n))))
  } else {
    delta_f <- max(new_n[1], new_n[2])
    data_frame(x = rnorm(n = reps*sum(n), 
                         mean = rep(rep(mean, n), reps),
                         sd = rep(rep(sd, n), reps)),
               group = rep(rep(c("x", "y"), n), reps),
               replicate = as.factor(rep(1:reps, each = sum(n))),
               noise = rep(rlaplace(n = reps,
                                    m = 0,
                                    s = delta_f/epsilons[2]), each = sum(n)))
  }
}