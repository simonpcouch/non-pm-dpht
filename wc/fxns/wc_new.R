# `wc_new` is the Pratt variant of the test, which our test uses
# this variant essentially keeps all of the zeroes in the ranking step, and 
# then replaces their rankings with zero after the ranking takes place.
# the input, `x`, is a vector of d_i's

wc_new <- function(x) {
  ranks <- x %>% abs() %>% rank(ties.method = "average")
  ranks[x == 0] <- 0
  W <- sum(ranks * sign(x))
  return(W)
}