library(tidyverse)
source('code/get_cd.R')
source('code/get_est.R')

#################### Data ####################
t.obs <- 2
dist <- tibble(
  t = c(1, 2, 3, 4),
  p = c(5, 20, 60, 15),
  p.norm = p/sum(p)
)

#################### CD from intervals ####################
cd_int <- exp(seq(-5, 0, length = 100)) %>%
  set_names %>%
  map_df(function(alpha) get_est(dist, t.obs, alpha), .id = 'alpha') %>%
  mutate_at('alpha', as.numeric)

#################### CD graph (midp) ####################
tibble(
  beta = seq(-4, 2, length = 1000),
  alpha = 1 - 2*abs(0.5-map_dbl(beta, get_cd, dist, t.obs, midp = TRUE))
) %>%
  ggplot() +
  geom_line(aes(beta, alpha)) +
  geom_line(aes(midp_lower, alpha), cd_int, col = 'blue') +
  geom_line(aes(midp_upper, alpha), cd_int, col = 'blue')

#################### CD graph (not midp) ####################
tibble(
  beta = seq(-4, 2, length = 1000),
  alpha = 1 - 2*abs(0.5-map_dbl(beta, get_cd, dist, t.obs, midp = FALSE))
) %>%
  ggplot() +
  geom_line(aes(beta, alpha)) +
  geom_line(aes(lower, alpha), cd_int, col = 'blue') +
  geom_line(aes(upper, alpha), cd_int, col = 'blue')

#################### Both on same graph ####################
ggplot(cd_int) +
  geom_line(aes(midp_lower, alpha), cd_int) +
  geom_line(aes(midp_upper, alpha), cd_int) +
  geom_line(aes(lower, alpha), cd_int, linetype = 'dashed') +
  geom_line(aes(upper, alpha), cd_int, linetype = 'dashed')
