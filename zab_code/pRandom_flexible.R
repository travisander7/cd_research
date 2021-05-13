# based on the incorporation of the between-study variance in both 
# treatment arms via the use of logits

# theta=1; k=10; p_ic_init=0.05; tau2=0.2; min.n=10; max.n=50; set.seed(1234)

pRandom <- function(theta, k, p_ic_init, tau2, min.n, max.n) {
  
  # 2
  p_it_init <- p_ic_init * exp(theta) / (1 - p_ic_init + p_ic_init * exp(theta))
  
  # For balanced designs: ------------------------------------------------------
  # # 3
  # n_i <- round(runif(n = k, min = min.n, max = max.n))
  # 
  # # 4
  # n_ic <- n_i
  # n_it <- n_i
  # ----------------------------------------------------------------------------
  
  # For unbalanced designs: ----------------------------------------------------
  # 3 & 4
  n_ic <- round(runif(n = k, min = min.n, max = max.n))
  n_it <- round(runif(n = k, min = min.n, max = max.n))
  # ----------------------------------------------------------------------------
  
  # 5
  mu_ic <- log(p_ic_init / (1 - p_ic_init))
  mu_it <- log(p_it_init / (1 - p_it_init))
  
  # For equal variances: -------------------------------------------------------
  # 6
  # theta_ic <- rnorm(n = k, mean = mu_ic, sd = sqrt(tau2) / sqrt(2))
  # theta_it <- rnorm(n = k, mean = mu_it, sd = sqrt(tau2) / sqrt(2))
  # ----------------------------------------------------------------------------
  
  # For unequal variances: -----------------------------------------------------
  # 6
  theta_ic <- rnorm(n = k, mean = mu_ic, sd = sqrt(0.5))
  theta_it <- rnorm(n = k, mean = mu_it, sd = sqrt(tau2))
  # ----------------------------------------------------------------------------
  
  
  # 7
  p_ic <- 1 / (1 + exp(-theta_ic))
  p_it <- 1 / (1 + exp(-theta_it))
  
  # 8
  r_ic <- rbinom(n = k, size = n_ic, prob = p_ic)
  r_it <- rbinom(n = k, size = n_it, prob = p_it)
  
  # first check: make sure there are at least two studies with non-zeros in at
  # least one arm (if not, then estimating heterogeneity is pointless since
  # only one study is used in the analysis)
  # second check: make sure there is at least one event across studies in the
  # treatment group
  # third check: make sure there is at least one event across studies in the
  # control group
  while(sum((r_ic + r_it) > 0) < 2 ||
        sum(r_it) == 0 ||
        sum(r_ic) == 0) {
    r_ic <- rbinom(n = k, size = n_ic, prob = p_ic)
    r_it <- rbinom(n = k, size = n_it, prob = p_it)
  }
  
  r.list <- list("CTRL_n" = n_ic, "TRT_n" = n_it, 
                 "CTRL_event" = r_ic, "TRT_event" = r_it)
  return(r.list)
  
}