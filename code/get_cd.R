#' Compute a CD for the perm method
#' 
#' @param thetas An x-grid of theta values.
#' @param u Possible values of distribution of test statistic.
#' @param prob Probabilities of each test statistic in distribution.
#' @param t_obs Observed test statistic.
#' @return A vector containing the corresponding CD for thetas.
#' @examples
#' m <- rema::rema(TRT_event, TRT_n, CTRL_event, CTRL_n)
#' theta_grid <- seq(-1, 1, length = 1000)
#' get_cd(theta_grid, m$dist$norm.probs, m$dist$test.stat, m$tstat)
get_cd <- function(thetas, u, prob, t_obs){
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
