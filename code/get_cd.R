get_cd <- function(beta, dist, t){
  # Prepare variables
  prob <- dist$p.norm
  u <- dist$t
  t <- t - min(u)
  u <- u - min(u)
  # Calculate CD
  u_prob <- prob*exp(u*beta)
  p <- u_prob/sum(u_prob)
  sum(p[u > t]) + 0.5*p[u == t]
}