
##### II-CC-FF for 2x2 tables with random effects (log Odds ratio)

psif <- function(tmbfunc, psi, tau) {
  tmbfunc$fn(c(psi, tau ^ 2))
}

llprof_tmb_corr <- function(tmbfunc, psival) {
  nn <- length(psival)
  ll <- rep(NA, nn)
  hessCorr <- rep(0, nn)
  taumax <- rep(NA, nn)
  code <- rep(NA, nn)
  for (i in 1:nn) {
    cco <- nlminb(0.1,
                  psif,
                  psi = psival[i],
                  tmbfunc = tmbfunc,
                  lower = 0)
    ll[i] <- cco$objective
    taumax[i] <- cco$par #
    code[i] <- cco$co
    hessCorr[i] <- -log(taumax[i])
  }
  return(list(llp = ll,
              taumax = taumax,
              corr = hessCorr,
              code = code))
}