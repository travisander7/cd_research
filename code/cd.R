library(tidyverse)

t <- 2
dist <- tibble(
  t = c(1, 2, 3, 4),
  p = c(5, 20, 60, 15),
  p.norm = p/sum(p)
)

prob <- dist$p.norm
u <- dist$t
t.obs <- t - min(u)
u <- u - min(u)
t.or.alpha <- t.obs
beta0 <- 1

dist %>%
  mutate(
    u = t - 1,
    logp = log(p.norm),
    new = logp + u*beta0,
    new2 = exp(new - log(sum(p.norm + u*beta0)))
  ) 

CMLE <- function(prob, u, t.obs, t.or.alpha, ci = FALSE, dir = "upper", 
                 midp = TRUE) {
  
  prob <- log(prob)  # normalized probabilities on log scale
  
  # Objective function for beta estimate and CI estimates
  # add log probabilities based on:
  # log(p + q) = log(p) + log(1 + exp{log(q) - log(p)})
  betaObjectiveFun <- function(beta0, prob, u, t.obs, t.or.alpha, 
                               ci = FALSE, dir = "upper") {
    add.log.probs <- prob[1] + u[1] * beta0
    for (q in 2:length(u)) {  # add log probabilities
      log.p <- max(add.log.probs, prob[q] + u[q] * beta0)
      add.log.probs <- log.p + log(exp(add.log.probs - log.p) + 
                                     exp(prob[q] + u[q] * beta0 - log.p))
    }
    # changed from "prob * exp{u * beta}" since on log scale 
    new <- prob + (u * beta0)
    # changed from "new / sum(new)" since on log scale 
    new2 <- exp(new - add.log.probs)  
    if (ci) {
      
      if (midp) {
        sub <- ifelse(dir == "lower", 
                      sum(c(0.5 * new2[u == t.obs], new2[u > t.obs])), 
                      sum(c(new2[u < t.obs], 0.5 * new2[u == t.obs])))
      } else {
        sub <- ifelse(dir == "lower", 
                      sum(new2[u >= t.obs]), 
                      sum(new2[u <= t.obs]))
      }
      
    } else {
      sub <- sum(u * new2) / sum(new2)
    }
    abs(t.or.alpha - sub)
  }
  
  est.res <- optimize(betaObjectiveFun, c(-15, 15), prob, u, t.obs,
                      t.or.alpha, ci, dir, tol = 0.00000001, maximum = FALSE)
  result <- est.res$minimum
  result
}