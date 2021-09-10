get_cd <- function(thetas, prob, u, t_obs){
  # rescale test statistic
  t_obs <- t_obs - min(u)
  u <- u - min(u)
  # get normalized probabilites for each theta
  P_unnorm <- prob*exp(u %x% t(thetas))
  P_norm <- t(t(P_unnorm)*colSums(P_unnorm)^(-1))
  # sum up extreme values
  extreme <- as.numeric(u > t_obs) + 0.5*(u == t_obs)
  colSums(P_norm*extreme)
}