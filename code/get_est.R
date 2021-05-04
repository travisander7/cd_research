library(tidyverse)
source('code/CMLE.R')

get_est <- function(dist, t, alpha = 0.05){
  u <- dist$t
  p <- dist$p.norm

  t_prob <- p[u == t]
  p_val <- sum(p[p <= t_prob])
  midp_val <- sum(p[p < t_prob]) + 0.5*t_prob

  p <- ifelse(p == 0, 1.0e-16, p)

  only_one <- length(u) == 1
  at_extreme <- t %in% range(u)
  approx_extreme <- min(sum(p[u < t]), sum(p[u > t])) < 1.0e-15

  if(only_one){
    print('Error: Only one in distribution')
  } else if(at_extreme){
    print('Error: At extreme')
  } else if(approx_extreme){
    print('Error: Approximately at extreme')
  } else {
    t <- t - min(u)
    u <- u - min(u)
    beta_hats <- CMLE(p, u, t, t.or.alpha = t, ci = FALSE)
    lower <- CMLE(p, u, t, t.or.alpha = alpha/2, ci = TRUE, dir = 'lower', midp = FALSE)
    upper <- CMLE(p, u, t, t.or.alpha = alpha/2, ci = TRUE, dir = 'upper', midp = FALSE)
    midp_lower <- CMLE(p, u, t, t.or.alpha = alpha/2, ci = TRUE, dir = 'lower', midp = TRUE)
    midp_upper <- CMLE(p, u, t, t.or.alpha = alpha/2, ci = TRUE, dir = 'upper', midp = TRUE) 
    tibble(beta_hats, lower, upper, midp_lower, midp_upper, p_val, midp_val, only_one, at_extreme, approx_extreme) 
  }
}
