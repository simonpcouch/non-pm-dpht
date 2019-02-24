# `wc` is the normal procedure of the Wilcoxon (not the Pratt variant)
# the TC test follows this procedure
# the input, `x`, is a vector of d_i's
wc <- function(x) {
  ranks <- x[x != 0] %>% abs() %>% rank(ties.method = "average")
  W <- sum(ranks * sign(x[x!=0]))
  return(W)
}