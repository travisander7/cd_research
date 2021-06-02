library(tidyverse)
source('zab_code/CMLE.R')

get_est <- function(prob, u, t_obs, alpha = 0.05){
  obs_prob <- prob[u == t_obs]
  p_val <- sum(prob[prob <= obs_prob])
  midp_val <- sum(prob[prob < obs_prob]) + 0.5*obs_prob

  prob <- ifelse(prob == 0, 1.0e-16, prob)

  only_one <- length(u) == 1
  at_extreme <- t_obs %in% range(u)
  approx_extreme <- min(sum(prob[u < t_obs]), sum(prob[u > t_obs])) < 1.0e-15

  if(only_one){
    print('Error: Only one in distribution')
  } else if(at_extreme){
    print('Error: At extreme')
  } else if(approx_extreme){
    print('Error: Approximately at extreme')
  } else {
    t_obs <- t_obs - min(u)
    u <- u - min(u)
    beta_hats <- CMLE(prob, u, t_obs, t.or.alpha = t_obs, ci = FALSE)
    lower <- CMLE(prob, u, t_obs, t.or.alpha = alpha/2, ci = TRUE, dir = 'lower', midp = FALSE)
    upper <- CMLE(prob, u, t_obs, t.or.alpha = alpha/2, ci = TRUE, dir = 'upper', midp = FALSE)
    midp_lower <- CMLE(prob, u, t_obs, t.or.alpha = alpha/2, ci = TRUE, dir = 'lower', midp = TRUE)
    midp_upper <- CMLE(prob, u, t_obs, t.or.alpha = alpha/2, ci = TRUE, dir = 'upper', midp = TRUE) 
    tibble(beta_hats, lower, upper, midp_lower, midp_upper, p_val, midp_val, only_one, at_extreme, approx_extreme) 
  }
}
