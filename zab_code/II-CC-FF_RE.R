
# Note: same results if you remove the double zero studies
# (so method essentially removes them)
# Note: method does utilize single zero studies
# Note: the corrected version produces very strange looking confidence
# curves (don't have the right shape, have multiple cusps, etc.), so we are just 
# using the standard version.
# If EstMedian == 6 or -6, then the cusp did not reach zero, so throw that 
# simulation out. 
# If the confidence bound == -6, then replace it with -INF
# If the confidence bound == 6, then replace it with INF

CDResults <- function(scenario.num, num.reps, theta, k, mu, tau2) {
  
  library(meta)
  library(TMB)
  dyn.load(dynlib("meta2x2_re_full_new"))  # after compilation
  source("0_icf_code.R")
  
  all.cd.results <- data.frame(matrix(ncol = 3, nrow = num.reps))
  colnames(all.cd.results) <- c("EstMedian", "CIlow", "CIup")
  
  ##############################################################################
  #                             read in data
  ##############################################################################
  
  load(paste0("Theta=", theta, ",Tau2=", tau2, ",Mu=", mu, ".RData"))
  
  ##############################################################################
  #                       run data through CD method
  ##############################################################################
  for (j in 1:num.reps) {
    
    # First, get reasonable starting values from metabin
    metabin.res <- metabin(event.e = ma_data_sets[, j]$TRT_event,
                           n.e = ma_data_sets[, j]$TRT_n,
                           event.c = ma_data_sets[, j]$CTRL_event,
                           n.c = ma_data_sets[, j]$CTRL_n,
                           sm = "OR",
                           method = "Inverse",
                           method.tau = "DL")
    metabin.estimates <- metabin.res$TE
    psisT <- sapply(metabin.estimates, 
                    FUN = function(x) {ifelse(is.na(x), rnorm(1, 0, 0.1), x)})
    psi0T <- metabin.res$TE.fixed  # log scale
    tauT <- metabin.res$tau
    
    # Now, run through II-CC-FF method
    psival <- seq(-6, 6, length = 10 ^ 3)
    nll <- MakeADFun(data = list(vm1 = ma_data_sets[, j]$TRT_n,
                                 vy1 = ma_data_sets[, j]$TRT_event,
                                 vm0 = ma_data_sets[, j]$CTRL_n,
                                 vz = ma_data_sets[, j]$CTRL_event + 
                                   ma_data_sets[, j]$TRT_event),
                     parameters = list(psis = psisT, 
                                       psi = psi0T, 
                                       tau2 = tauT ^ 2),
                     random = "psis",
                     silent = T,
                     DLL = "meta2x2_re_full_new") #
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
    
    all.cd.results[j, ] <- data.frame(log.or.median.est, 
                                      lower.bound, 
                                      upper.bound)  # log scale
    
  }
  
  ##############################################################################
  #                               save results
  ##############################################################################
  
  saveRDS(all.cd.results, paste0("II-CC-FF_RE_Results_", scenario.num, ".rds"))
}

################################################################################
#                             set parameters
################################################################################

mu.sim.vals <- seq(-4.5, -3, by = 0.5)
tau2.sim.vals <- c(0, 0.05, 0.1, 0.2, 0.4, 0.8)
theta.sim.vals <- c(0, 0.5, 1, 1.5)
params <- expand.grid("mu" = mu.sim.vals, 
                      "tau2" = tau2.sim.vals, 
                      "theta" = theta.sim.vals)


library(future.apply)
plan(multisession, workers = 11)

future_mapply(FUN = "CDResults",
              scenario.num = row.names(params),
              mu = params$mu,
              tau2 = params$tau2,
              theta = params$theta,
              MoreArgs = list(k = 10,
                              num.reps = 2000),
              future.seed = 1234)

