library(tidyverse)
dyn.load(TMB::dynlib('code/meta2x2_re_full_new'))
source('code/get_cd.R')
source('code/ii_cc_ff.R')
source('code/0_icf_code.R')
source('code/pRandom_flexible.R')
set.seed(1)

get_metrics <- function(theta, k, p_ic_init, tau2, min.n, max.n){
  # Simulate dataset
  sim <- pRandom(theta, k, p_ic_init, tau2, min.n, max.n)

  # Evaluate whether t-statistic is on extreme or only one in distribution
  perm <- with(sim, rema::rema(TRT_event, TRT_n, CTRL_event, CTRL_n, alpha = 0.01))
  only_one <- nrow(perm$dist) == 1
  extreme <- perm$tstat %in% range(perm$dist$test.stat)

  if(only_one | extreme){
    tibble(only_one, extreme, theta, k, p_ic_init, tau2, min.n, max.n)
  } else{
    # Get perm CI for x-grid
    perm_ci <- log(perm$CI)
    thetas <- seq(max(perm_ci[1], -15), min(perm_ci[2], 15), by = 0.005) 

    # Get CDs/models for three methods
    perm_cd <- get_cd(thetas, perm$dist$norm.probs, perm$dist$test.stat, perm$tstat)
    liu <- gmeta::gmeta(
      with(sim, cbind(TRT_event, TRT_n, CTRL_event, CTRL_n)),
      gmi.type = '2x2',
      method = 'exact1',
      gmo.xgrid = thetas
    )
    m_ii_cc_ff <- with(sim, ii_cc_ff(TRT_event, TRT_n, CTRL_event, CTRL_n, thetas))

    # Make table of estimates, CIs, and p-values
    ci_df <- tibble(
      names = c('estimate', 'lower', 'upper', 'p_value'),
      ii_cc_ff = with(m_ii_cc_ff, c(estimate, lower, upper, NA)),
      liu = with(liu, c(combined.mean, combined.ci[1], combined.ci[2], pvalue)),
      perm = with(perm, log(c(TE, CI[1], CI[2], exp(pval))))
    ) %>%
      pivot_longer(-names, 'method') %>%
      pivot_wider(method, names)

    # Calculate metrics for all CDs, and then combine with above table
    tibble(thetas, perm = perm_cd, liu = liu$combined.cd, ii_cc_ff = m_ii_cc_ff$cd) %>%
      pivot_longer(-thetas, names_to = 'method', values_to = 'distribution') %>%
      mutate(curve = 1-abs(1-2*distribution)) %>%
      group_by(method) %>%
      mutate(is_height = row_number() == which.min(abs(thetas - theta))) %>%
      summarize(area = sum(curve)*(max(thetas) - min(thetas))/length(thetas), height = sum(curve*is_height)) %>%
      full_join(ci_df, 'method') %>%
      mutate(only_one, extreme, theta, k, p_ic_init, tau2, min.n, max.n)
  }
}

