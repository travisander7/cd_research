source("CMLE.R")  # Conditional Maximum Likelihood Estimate


###########################################################################
#                         read in distribution
###########################################################################
distribution <- read.table(paste0("distn_", data.set.rep, ".txt"))
colnames(distribution) <- c("test.stat", "unnorm.probs", "norm.probs")

###########################################################################
# stop if the distribution contains only one value of the test statistic
###########################################################################
only.one.in.dist <- ifelse(dim(distribution)[1] == 1, TRUE, FALSE)

if (only.one.in.dist) {
  
  beta.hats <- NA
  conf.up <- NA
  conf.low <- NA
  conf.up.midp <- NA
  conf.low.midp <- NA
  pval.twosided <- NA
  pval.twosided.midp <- NA
  t.obs.at.extremes <- NA
  t.obs.approx.at.extremes <- NA
  
} else {
  
  #########################################################################
  #                            read in data
  #########################################################################
  perm.ma.data.set <- read.csv(paste0("MAdata", data.set.rep, ".txt"), 
                               sep = " ",
                               skip = 1)
  colnames(perm.ma.data.set) <- c("trt", "event", "total")
  # observed test statistic from our data
  t.obs <- as.numeric(perm.ma.data.set$trt %*% perm.ma.data.set$event)  
  
  #########################################################################
  #                      compute two-sided p-value
  #########################################################################
  
  obs.prob <- distribution$norm.probs[distribution$test.stat == t.obs]
  pval.twosided <- 
    sum(distribution$norm.probs[distribution$norm.probs <= obs.prob])
  pval.twosided.midp <- 
    sum(distribution$norm.probs[distribution$norm.probs < obs.prob]) + 
    (0.5 * distribution$norm.probs[distribution$norm.probs == obs.prob])
  
  #########################################################################
  #                        save parts of distribution
  #########################################################################  
  u <- distribution$test.stat
  prob <- ifelse(distribution$norm.probs == 0, 
                 1.0e-16, 
                 distribution$norm.probs)
  
  #########################################################################
  #        note if t.obs is at either extremes of the distribution
  #########################################################################
  t.obs.at.extremes <- ifelse(t.obs == max(u) || t.obs == min(u), 
                              TRUE, FALSE)
  t.obs.approx.at.extremes <- ifelse(sum(prob[u > t.obs]) < 1.0e-15 || 
                                       sum(prob[u < t.obs]) < 1.0e-15, 
                                     TRUE, FALSE)
  
  #########################################################################
  #      if t.obs is at either extremes of the distribution, skip (NA)
  #########################################################################
  if (t.obs.at.extremes || t.obs.approx.at.extremes) {
    
    beta.hats <- NA
    conf.up <- NA
    conf.low <- NA
    conf.up.midp <- NA
    conf.low.midp <- NA
    
    #########################################################################
    #        otherwise (if t.obs is NOT at an extreme), use CMLE
    #########################################################################
  } else {
    
    # scale down test statistics to start at 0
    t.obs <- t.obs - min(distribution$test.stat)
    u <- distribution$test.stat - min(distribution$test.stat)
    
    beta.hats <- CMLE(prob = prob, 
                      u = u, 
                      t.obs = t.obs, 
                      t.or.alpha = t.obs, 
                      ci = FALSE)
    
    conf.low <- CMLE(prob = prob, 
                     u = u, 
                     t.obs = t.obs, 
                     t.or.alpha = 0.025, 
                     ci = TRUE, 
                     dir = "lower",
                     midp = FALSE)
    conf.up <- CMLE(prob = prob, 
                    u = u, 
                    t.obs = t.obs, 
                    t.or.alpha = 0.025, 
                    ci = TRUE, 
                    dir = "upper",
                    midp = FALSE)
    
    conf.low.midp <- CMLE(prob = prob, 
                          u = u, 
                          t.obs = t.obs, 
                          t.or.alpha = 0.025, 
                          ci = TRUE, 
                          dir = "lower",
                          midp = TRUE)
    conf.up.midp <- CMLE(prob = prob, 
                         u = u, 
                         t.obs = t.obs, 
                         t.or.alpha = 0.025, 
                         ci = TRUE, 
                         dir = "upper",
                         midp = TRUE)
  }
}

data.frame(beta.hats,
           conf.up, 
           conf.low,
           conf.up.midp,
           conf.low.midp,
           pval.twosided,
           pval.twosided.midp,
           only.one.in.dist, 
           t.obs.at.extremes,
           t.obs.approx.at.extremes)
}

