# `gen_data` randomly generates data to be supplied to the `kw` or `kw_new` 
# function, as well as noise for private trials. 

# `n`, `mean`, `sd`: A vector (length h) of desired sample sizes, means, and 
#     standard deviations, respectively. Note that all three of these arguments 
#     must be of the same length. 
# `reps`: The desired number of test statistics to be calculated from the output
# `epsilon`: The privacy parameter to generate the `noise` column with. 
#     Supply `epsilon = NA` for the public test (a noise column will not be 
#     generated)
# `kw_version`: Either "sq" (referring to the regular H statistic) or "abs" 
#     (referring to the unsquared absolute value H statistic) s.t. noise with 
#     the appropriate sensitivity can be generated.

gen_data <- function(n, mean, sd, reps, epsilon, kw_version) {
  if (is.na(epsilon)) { # procedure for the public test
    df <-  data_frame(x = rnorm(n = round(reps*sum(n)), 
                                mean = rep(rep(mean, n), reps),
                                sd = rep(round(rep(sd, n)), reps)),
                      group = rep(rep(1:length(n), n), reps),
                      replicate = as.factor(rep(1:reps, each = sum(n))))
  } else { # procedure for the private test
    # generate noise depending on the kw_version
    if (kw_version == "sq") {
      df <- data_frame(x = rnorm(n = reps*sum(n), 
                                 mean = rep(rep(mean, n), reps),
                                 sd = rep(rep(sd, n), reps)),
                       group = rep(rep(1:length(n), n), reps),
                       replicate = as.factor(rep(1:reps, each = round(sum(n)))),
                       noise = rep(rlaplace(n = reps,
                                            m = 0,
                                            s = 78/epsilon), 
                                   each = round(sum(n))))
    } else if (kw_version == "abs") {
      df <- data_frame(x = rnorm(n = reps*round(sum(n)), 
                                 mean = rep(rep(mean, n), reps),
                                 sd = rep(rep(sd, n), reps)),
                       group = rep(rep(1:length(n), n), reps),
                       replicate = as.factor(rep(1:reps, each = round(sum(n)))),
                       noise = rep(rlaplace(n = reps,
                                            m = 0,
                                            s = 8/epsilon), 
                                   each = round(sum(n))))
    }
  }
  return(list(df, kw_version))
}
