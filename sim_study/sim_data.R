library(tidyverse)
source('code/pRandom_flexible.R')
sim_data <- function(rep_num, theta, k, p_ic_init, tau2, min.n, max.n, write = TRUE){
  sim <- pRandom(theta, k, p_ic_init, tau2, min.n, max.n) %>%
    as_tibble
  ident_str <- str_c(theta, '_', p_ic_init, '_', tau2, '_', rep_num)
  write_csv(sim, str_c('data/datasets/', ident_str, '.csv'))
}


future::plan('multisession', workers = 32)

tictoc::tic()
expand.grid(
  rep_num = 1:1000,
  theta = c(-1, -0.5, 0, 0.5, 1),
  k = 10,
  p_ic_init = c(0.01, 0.05),
  tau2 = c(0, 0.2, 0.4, 0.8),
  min.n = 10,
  max.n = 50
) %>%
  furrr::future_pmap(sim_data, .options = furrr::furrr_options(seed = 1))
tictoc::toc()
