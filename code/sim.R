library(tidyverse)
source('zab_code/pRandom_flexible.R')
source('code/get_cd.R')
set.seed(1)

sims <- rerun(20, {
  invalid <- TRUE
  while(invalid){
    sim <- pRandom(
      theta = 1, 
      k = 5, 
      p_ic_init = 0.1, 
      tau2 = 0.2, 
      min.n = 20, 
      max.n = 30
    ) %>%
      as_tibble

    dist <- with(sim, rema::rema(TRT_event, TRT_n, CTRL_event, CTRL_n))
    t_obs <- sum(sim$TRT_event)
    invalid <- t_obs %in% range(dist$test.stat)

    cd <- tibble(
      thetas =  seq(-2, 4, length = 1000),
      cd = with(dist, get_cd(thetas, norm.probs, test.stat, t_obs))
    )
  }
  cd
})

sim_df <- bind_rows(sims, .id = 'simulation') %>%
  mutate_at('simulation', as.numeric)

ggplot(sim_df, aes(thetas, 1 - 2*abs(0.5-cd))) +
  geom_line() +
  facet_wrap(~simulation, nrow = 4, ncol = 5)
