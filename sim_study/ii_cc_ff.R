ii_cc_ff <- function(TRT_event, TRT_n, CTRL_event, CTRL_n, psival){
  # First, get reasonable starting values from metabin
  metabin.res <- meta::metabin(
    event.e = TRT_event,
    n.e = TRT_n,
    event.c = CTRL_event,
    n.c = CTRL_n,
    sm = "OR",
    method = "Inverse",
    method.tau = "DL"
  )
  metabin.estimates <- metabin.res$TE
  psisT <- sapply(metabin.estimates, 
                  FUN = function(x) {ifelse(is.na(x), rnorm(1, 0, 0.1), x)})
  psi0T <- metabin.res$TE.fixed  # log scale
  tauT <- metabin.res$tau

  # Now, run through II-CC-FF method
  nll <- TMB::MakeADFun(
    data = list(
      vm1 = TRT_n,
      vy1 = TRT_event,
      vm0 = CTRL_n,
      vz = CTRL_event + TRT_event
    ),
    parameters = list(psis = psisT, psi = psi0T, tau2 = tauT ^ 2),
    random = "psis",
    silent = T,
    DLL = "meta2x2_re_full_new"
  )
  llp <- llprof_tmb_corr(nll, psival = psival)
  ll1 <- -llp$llp

  cc1 <- pchisq(2 * (max(ll1) - ll1), df = 1)  # original CD

  icf.results <- as.data.frame(cbind(psival, cc1))
  log.or.median.est <- icf.results$psival[icf.results$cc1 == min(icf.results$cc1)]
  bound.info <- icf.results$cc1 <= 0.95
  lower.bound.position <- min(which(bound.info == TRUE))
  lower.bound <- icf.results[lower.bound.position, 1]
  upper.bound.position <- max(which(bound.info == TRUE))
  upper.bound <- icf.results[upper.bound.position, 1]

  data.frame(log.or.median.est, lower.bound, upper.bound)  # log scale
  list(cd = cc1, estimate = log.or.median.est, lower = lower.bound, upper = upper.bound)
}

