model_data <- function(filepath){
  sim <- read_csv(filepath)
  params <- filepath %>%
    str_split('/') %>%
    unlist %>%
    last %>%
    str_remove('.csv') %>%
    str_split('_') %>%
    unlist %>%
    as.numeric
  theta <- params[1]
  p_ic_init <- params[2]
  tau2 <- params[3]
  rep <- params[4]
  perm <- with(sim, rema::rema(TRT_event, TRT_n, CTRL_event, CTRL_n, alpha = 0.01))
  only_one <- nrow(perm$dist) == 1
  extreme <- perm$tstat %in% range(perm$dist$test.stat)

  if(only_one){
    tibble(rep, only_one, extreme, theta, p_ic_init, tau2)
  } else{
    perm_ci <- log(perm$CI)
    thetas <- seq(max(perm_ci[1], -15), min(perm_ci[2], 15), by = 0.005) 
    perm_cd <- get_cd(thetas, perm$dist$norm.probs, perm$dist$test.stat, perm$tstat)
    liu <- gmeta::gmeta(
      with(sim, cbind(TRT_event, TRT_n, CTRL_event, CTRL_n)),
      gmi.type = '2x2',
      method = 'exact1',
      gmo.xgrid = thetas
    )

    dyn.load(TMB::dynlib('code/meta2x2_re_full_new'))
    m_ii_cc_ff <- with(sim, ii_cc_ff(TRT_event, TRT_n, CTRL_event, CTRL_n, thetas))

    ci_df <- tibble(
      names = c('estimate', 'lower', 'upper', 'p_value'),
      ii_cc_ff = with(m_ii_cc_ff, c(estimate, lower, upper, NA)),
      liu = with(liu, c(combined.mean, combined.ci[1], combined.ci[2], pvalue)),
      perm = with(perm, log(c(TE, CI[1], CI[2], exp(pval))))
    ) %>%
      pivot_longer(-names, 'method') %>%
      pivot_wider(method, names)

    tibble(thetas, perm = perm_cd, liu = liu$combined.cd, ii_cc_ff = m_ii_cc_ff$cd) %>%
      pivot_longer(-thetas, names_to = 'method', values_to = 'distribution') %>%
      mutate(curve = 1-2*abs(0.5-distribution)) %>%
      group_by(method) %>%
      mutate(is_height = row_number() == which.min(abs(thetas - theta))) %>%
      summarize(area = sum(curve)*(max(thetas) - min(thetas))/length(thetas), height = sum(curve*is_height)) %>%
      full_join(ci_df, 'method') %>%
      mutate(rep, only_one, extreme, theta, p_ic_init, tau2)
  }
}
