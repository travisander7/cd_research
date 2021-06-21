library(tidyverse)
source('zab_code/pRandom_flexible.R')

get_sim <- function(seed){
  set.seed(seed)

  data <- pRandom(
    theta = 1, 
    k = 5, 
    p_ic_init = 0.1, 
    tau2 = 0.2, 
    min.n = 20, 
    max.n = 30
  ) %>%
    as_tibble %>%
    select(TRT_event, TRT_n, CTRL_event, CTRL_n)

  liu <- gmeta::gmeta(
    as.matrix(data), 
    gmi.type = '2x2', 
    method = 'exact1', 
    gmo.xgrid = seq(0, 2, length = 1000),
    report.error = TRUE
  )
  with(liu, tibble(theta = x.grids, cd = combined.cd, density = combined.density, curve = 1-2*abs(0.5-cd)))
}

sim_df <- map_df(1:12, get_sim, .id = 'sim')

sim_df %>%
  pivot_longer(-c(sim, theta), names_to = 'type') %>%
  ggplot(aes(theta, value, col = type)) +
  geom_line() +
  facet_wrap(~sim, ncol = 4)

sim_df %>%
  group_by(sim) %>%
  summarize(which.max(density) - which.max(curve)) %>%
  pull(2)
