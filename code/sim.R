library(tidyverse)
source('zab_code/pRandom_flexible.R')
source('code/get_dist.R')
source('code/get_cd.R')
source('code/get_est.R')
set.seed(1)

sim <- pRandom(
  theta = 1, 
  k = 5, 
  p_ic_init = 0.1, 
  tau2 = 0.2, 
  min.n = 20, 
  max.n = 30
) %>%
  as_tibble

sim_long <- sim %>%
  pivot_longer(everything(), names_to = c('Treatment', '.value'), names_sep = '_') %>%
  mutate(Treatment = as.numeric(Treatment == 'TRT')) %>%
  arrange(Treatment)
x <- sim_long$Treatment
z <- sim_long$event
n <- sim_long$n

t_obs <- sum(x*z)
dist <- get_dist(x, z, n)
dist
t_obs

tibble(
  theta = seq(-10, 10, length = 1000),
  cd = map_dbl(theta, get_cd, dist, 21),
  pval_func = 1 - 2*abs(0.5-cd)
) %>%
  ggplot() +
  geom_line(aes(theta, pval_func))

